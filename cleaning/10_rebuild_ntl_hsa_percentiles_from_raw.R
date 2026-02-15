# Full rebuild of ntl_hsa_percentiles.csv from raw ACS/SDI inputs.
# This replaces the need to execute the legacy Rmd end-to-end.
#
# Usage:
#   source("cleaning/10_rebuild_ntl_hsa_percentiles_from_raw.R")
#   rebuild_ntl_hsa_percentiles_from_raw()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

resolve_dir <- function(primary_dir, label) {
  if (!is.null(primary_dir) && dir.exists(primary_dir)) return(primary_dir)
  stop(label, " directory not found: ", primary_dir)
}

resolve_file <- function(primary_file, label) {
  if (!is.null(primary_file) && file.exists(primary_file)) return(primary_file)
  stop(label, " file not found: ", primary_file)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", x)))
}

read_acs_slice <- function(file_path, named_map) {
  if (!file.exists(file_path)) return(NULL)
  df <- read_csv(file_path, show_col_types = FALSE, progress = FALSE)
  if (!"NAME" %in% names(df)) return(NULL)

  df <- df %>%
    mutate(zcta = str_extract(NAME, "(?<=ZCTA5\\s)\\d+")) %>%
    select(any_of(c("zcta", names(named_map)))) %>%
    rename(any_of(named_map))

  for (col in setdiff(names(df), "zcta")) {
    df[[col]] <- safe_numeric(df[[col]])
  }
  df
}

load_sdi_panel <- function(sdi_root) {
  sdi_2012 <- read_csv(file.path(sdi_root, "sdi_2008_2012_zcta.csv"), show_col_types = FALSE)
  sdi_2015 <- read_csv(file.path(sdi_root, "sdi_2011_2015_zcta.csv"), show_col_types = FALSE)
  sdi_2016 <- read_csv(file.path(sdi_root, "sdi_2012_2016_zcta.csv"), show_col_types = FALSE)
  sdi_2017 <- read_csv(file.path(sdi_root, "sdi_2013_2017_zcta.csv"), show_col_types = FALSE)
  sdi_2018 <- read_csv(file.path(sdi_root, "sdi_2014_2018_zcta.csv"), show_col_types = FALSE)
  sdi_2019 <- read_csv(file.path(sdi_root, "sdi_2015_2019_zcta.csv"), show_col_types = FALSE)

  keep_cols <- c("ZCTA5_FIPS", "ZCTA5_population", "SDI_score")
  sdi_2012 <- select(sdi_2012, any_of(keep_cols))
  sdi_2015 <- select(sdi_2015, any_of(keep_cols))
  sdi_2016 <- select(sdi_2016, any_of(keep_cols))
  sdi_2017 <- select(sdi_2017, any_of(keep_cols))
  sdi_2018 <- select(sdi_2018, any_of(keep_cols))
  sdi_2019 <- select(sdi_2019, any_of(keep_cols))

  sdi_year_map <- list(
    "2011" = sdi_2012,
    "2012" = sdi_2012,
    "2013" = sdi_2015,
    "2014" = sdi_2015,
    "2015" = sdi_2015,
    "2016" = sdi_2016,
    "2017" = sdi_2017,
    "2018" = sdi_2018,
    "2019" = sdi_2019,
    "2020" = sdi_2019,
    "2021" = sdi_2019,
    "2022" = sdi_2019,
    "2023" = sdi_2019
  )

  out <- bind_rows(lapply(names(sdi_year_map), function(y) {
    sdi_year_map[[y]] %>%
      mutate(
        year = as.integer(y),
        zcta = str_pad(as.character(ZCTA5_FIPS), width = 5, side = "left", pad = "0")
      ) %>%
      transmute(zcta, year, SDI_score_event = safe_numeric(SDI_score))
  }))

  distinct(out, zcta, year, .keep_all = TRUE)
}

apply_weighting <- function(data, weight_var, variables) {
  data %>%
    filter(!is.na(.data[[weight_var]])) %>%
    group_by(hsanum, year) %>%
    summarise(
      across(
        all_of(variables),
        ~ ifelse(
          sum(.data[[weight_var]], na.rm = TRUE) > 0,
          sum(.x * .data[[weight_var]], na.rm = TRUE) / sum(.data[[weight_var]], na.rm = TRUE),
          NA_real_
        ),
        .names = "weighted_{.col}"
      ),
      .groups = "drop"
    )
}

validate_ntl_hsa_percentiles <- function(df) {
  required_cols <- c(
    "hsanum",
    "year",
    "weighted_median_household_income_event",
    "weighted_percent_any_health_insur_event",
    "weighted_percent_public_health_insur_event",
    "weighted_unemployment_rate_event",
    "weighted_percent_bachelors_event",
    "weighted_percent_black_event",
    "weighted_percent_hispanic_or_latino_event",
    "weighted_percent_below_poverty_line_event",
    "weighted_SDI_score_event"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Rebuilt ntl_hsa_percentiles is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
}

#' Rebuild ntl_hsa_percentiles from raw inputs
#'
#' @param raw_dir Preferred raw directory (default "data/raw")
#' @param output_raw_path Output CSV path for staged raw artifact
#' @param output_interim_path Optional mirrored interim output
#' @return list with output paths invisibly
rebuild_ntl_hsa_percentiles_from_raw <- function(
  raw_dir = "data/raw",
  output_raw_path = "data/raw/ntl_hsa_percentiles.csv",
  output_interim_path = "data/interim/ntl_hsa_percentiles.csv"
) {
  raw_root <- resolve_dir(raw_dir, "Raw data root")

  census_root <- resolve_dir(file.path(raw_root, "census_raw_data"), "Census raw data")
  sdi_root <- resolve_dir(file.path(raw_root, "sdi_data"), "SDI raw data")

  zip_hsa_path <- resolve_file(file.path(raw_root, "ZipHsaHrr.csv"), "ZipHsaHrr crosswalk")
  zip_zcta_path <- resolve_file(file.path(raw_root, "ZIPCodetoZCTACrosswalk2022UDS.xlsx"), "ZIP->ZCTA crosswalk")
  map_dp02_path <- resolve_file(file.path(raw_root, "variable_mapping_DP02.csv"), "DP02 mapping")
  map_dp05_path <- resolve_file(file.path(raw_root, "variable_mapping_DP05.csv"), "DP05 mapping")
  map_s1701_path <- resolve_file(file.path(raw_root, "variable_mapping_S1701.csv"), "S1701 mapping")
  area_path <- resolve_file("data/processed/zctas_with_area.csv", "ZCTA area file")

  years <- 2010:2023
  census_years <- 2011:2023

  zip_hsa <- read_csv(zip_hsa_path, show_col_types = FALSE) %>%
    mutate(
      zipcode19 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    )

  zip_zcta <- read_excel(zip_zcta_path) %>%
    mutate(
      ZIP_CODE = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      zcta = str_pad(as.character(zcta), width = 5, side = "left", pad = "0")
    )

  hsa_zip_zcta <- crossing(zip_hsa, year = years) %>%
    left_join(zip_zcta, by = c("zipcode19" = "ZIP_CODE")) %>%
    mutate(zcta = str_pad(as.character(zcta), width = 5, side = "left", pad = "0"))

  mapping_dp02 <- read_csv(map_dp02_path, show_col_types = FALSE)
  mapping_dp05 <- read_csv(map_dp05_path, show_col_types = FALSE)
  mapping_s1701 <- read_csv(map_s1701_path, show_col_types = FALSE)

  dp03_map <- c(
    "DP03_0062E" = "median_household_income",
    "DP03_0096PE" = "percent_any_health_insur",
    "DP03_0098PE" = "percent_public_health_insur",
    "DP03_0095E" = "pop_not_institutionalized",
    "DP03_0009PE" = "unemployment_rate",
    "DP03_0008E" = "pop_civilian_labor_force"
  )
  b01003_map <- c("B01003_001E" = "total_pop")

  year_panels <- lapply(years, function(y) {
    base <- hsa_zip_zcta %>%
      filter(year == y)

    if (y %in% census_years) {
      dp03 <- read_acs_slice(
        file.path(census_root, "DP03", paste0("DP03_", y), paste0("ACSDP5Y", y, ".DP03-Data.csv")),
        dp03_map
      )
      dp02_map_year <- mapping_dp02 %>% filter(year == y)
      dp02 <- read_acs_slice(
        file.path(census_root, "DP02", paste0("DP02_", y), paste0("ACSDP5Y", y, ".DP02-Data.csv")),
        setNames(dp02_map_year$standardized_name, dp02_map_year$raw_name)
      )
      b01003 <- read_acs_slice(
        file.path(census_root, "B01003", paste0("B01003_", y), paste0("ACSDT5Y", y, ".B01003-Data.csv")),
        b01003_map
      )
      s1701_map_year <- mapping_s1701 %>% filter(year == y)
      s1701 <- read_acs_slice(
        file.path(census_root, "S1701", paste0("S1701_", y), paste0("ACSST5Y", y, ".S1701-Data.csv")),
        setNames(s1701_map_year$standardized_name, s1701_map_year$raw_name)
      )
      dp05_map_year <- mapping_dp05 %>% filter(year == y)
      dp05 <- read_acs_slice(
        file.path(census_root, "DP05", paste0("DP05_", y), paste0("ACSDP5Y", y, ".DP05-Data.csv")),
        setNames(dp05_map_year$standardized_name, dp05_map_year$raw_name)
      )

      for (piece in list(dp03, dp02, b01003, s1701, dp05)) {
        if (!is.null(piece)) {
          base <- base %>% left_join(piece, by = "zcta")
        }
      }
    }
    base
  })

  percentile_df <- bind_rows(year_panels) %>%
    filter(year >= 2010)

  percentile_df <- percentile_df %>%
    {
      event_vars <- intersect(
        c(
          "median_household_income",
          "percent_any_health_insur",
          "percent_public_health_insur",
          "unemployment_rate",
          "percent_bachelors",
          "percent_black",
          "percent_hispanic_or_latino",
          "percent_below_poverty_line",
          "total_pop",
          "pop_25_older",
          "pop_not_institutionalized",
          "total_pop_poverty_status",
          "pop_civilian_labor_force"
        ),
        names(.)
      )
      if (length(event_vars) == 0) {
        .
      } else {
        rename_with(., ~ paste0(.x, "_event"), .cols = all_of(event_vars))
      }
    }

  percentile_df <- percentile_df %>%
    select(-any_of("zipcode19")) %>%
    group_by(zcta, hsanum, year) %>%
    summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")

  sdi_panel <- load_sdi_panel(sdi_root)
  percentile_df <- percentile_df %>%
    left_join(sdi_panel, by = c("zcta", "year"))

  population_density <- read_csv(area_path, show_col_types = FALSE) %>%
    mutate(zcta = str_pad(as.character(ZCTA5CE20), width = 5, side = "left", pad = "0")) %>%
    transmute(zcta, AREA_SQ_MI = safe_numeric(AREA_SQ_MI))

  percentile_df <- percentile_df %>%
    left_join(population_density, by = "zcta")

  weight_mapping <- list(
    pop_not_institutionalized_event = c(
      "percent_any_health_insur_event",
      "percent_public_health_insur_event"
    ),
    pop_civilian_labor_force_event = c("unemployment_rate_event"),
    pop_25_older_event = c("percent_bachelors_event"),
    total_pop_event = c(
      "median_household_income_event",
      "percent_black_event",
      "percent_hispanic_or_latino_event",
      "SDI_score_event"
    ),
    total_pop_poverty_status_event = c("percent_below_poverty_line_event"),
    sum_var = c("AREA_SQ_MI", "total_pop_event")
  )

  weighted_results <- lapply(names(weight_mapping), function(weight_var) {
    if (weight_var == "sum_var") {
      vars_to_sum <- weight_mapping[[weight_var]]
      strict_sums <- percentile_df %>%
        filter(if_all(all_of(vars_to_sum), ~ !is.na(.x))) %>%
        group_by(hsanum, year) %>%
        summarise(across(all_of(vars_to_sum), sum, .names = "sum_{.col}"), .groups = "drop")

      completeness_scores <- percentile_df %>%
        group_by(hsanum, year) %>%
        summarise(
          complete_zip_rows = sum(if_all(all_of(vars_to_sum), ~ !is.na(.x))),
          total_zip_rows = n(),
          .groups = "drop"
        ) %>%
        mutate(sum_completeness_pct = (complete_zip_rows / total_zip_rows) * 100) %>%
        select(hsanum, year, sum_completeness_pct)

      full_join(strict_sums, completeness_scores, by = c("hsanum", "year"))
    } else {
      apply_weighting(percentile_df, weight_var, weight_mapping[[weight_var]])
    }
  })

  ntl_hsa_percentiles <- Reduce(
    function(x, y) full_join(x, y, by = c("hsanum", "year")),
    weighted_results
  ) %>%
    mutate(
      population_density = ifelse(sum_AREA_SQ_MI > 0, sum_total_pop_event / sum_AREA_SQ_MI, NA_real_)
    ) %>%
    arrange(hsanum, year) %>%
    group_by(hsanum) %>%
    mutate(
      pop_change_pct = ifelse(
        lag(sum_total_pop_event, 3) > 0,
        ((lag(sum_total_pop_event, 1) - lag(sum_total_pop_event, 3)) / lag(sum_total_pop_event, 3)) * 100,
        NA_real_
      )
    ) %>%
    ungroup()

  validate_ntl_hsa_percentiles(ntl_hsa_percentiles)

  dir.create(dirname(output_raw_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(ntl_hsa_percentiles, output_raw_path)

  if (!is.null(output_interim_path)) {
    dir.create(dirname(output_interim_path), recursive = TRUE, showWarnings = FALSE)
    write_csv(ntl_hsa_percentiles, output_interim_path)
  }

  invisible(list(
    raw_output_path = output_raw_path,
    interim_output_path = output_interim_path
  ))
}
