suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

ocgh_manual_rural_small_town_hsas <- c(11094L, 18042L)

ocgh_mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

ocgh_load_ruca_lookup <- function(ruca_file) {
  readxl::read_excel(ruca_file, sheet = "Data") %>%
    transmute(
      zip5 = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      ruca_simple = case_when(
        RUCA1 %in% c(1, 2, 3) ~ "Metropolitan",
        RUCA1 %in% c(4, 5, 6) ~ "Micropolitan",
        RUCA1 %in% c(7, 8, 9) ~ "Small Town",
        RUCA1 == 10 ~ "Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      ruca_grouped = case_when(
        ruca_simple %in% c("Rural", "Small Town") ~ "Rural & Small Town",
        TRUE ~ ruca_simple
      ),
      geography_type = case_when(
        ruca_simple == "Metropolitan" ~ "Urban",
        ruca_grouped == "Rural & Small Town" ~ "Rural & Small Town",
        TRUE ~ ruca_grouped
      )
    ) %>%
    distinct(zip5, .keep_all = TRUE)
}

ocgh_load_zip_hsa_lookup <- function(crosswalk_file) {
  read_csv(crosswalk_file, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()
}

ocgh_load_zip_year_population <- function(
  zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
  census_root = "data/raw/census_raw_data"
) {
  zip_zcta <- readxl::read_excel(zip_zcta_file) %>%
    transmute(
      zip5 = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      zcta = str_pad(as.character(zcta), width = 5, side = "left", pad = "0")
    ) %>%
    distinct(zip5, zcta)

  read_b01003 <- function(year_value) {
    file_path <- file.path(
      census_root,
      "B01003",
      paste0("B01003_", year_value),
      paste0("ACSDT5Y", year_value, ".B01003-Data.csv")
    )
    read_csv(file_path, show_col_types = FALSE, progress = FALSE) %>%
      transmute(
        zcta = str_extract(NAME, "(?<=ZCTA5\\s)\\d+"),
        total_pop = suppressWarnings(as.numeric(gsub(",", "", B01003_001E)))
      ) %>%
      filter(!is.na(zcta)) %>%
      mutate(
        year = as.integer(year_value),
        zcta = str_pad(zcta, width = 5, side = "left", pad = "0")
      )
  }

  b01003_years <- 2011:2023
  bind_rows(lapply(b01003_years, read_b01003)) %>%
    left_join(zip_zcta, by = "zcta", relationship = "many-to-many") %>%
    filter(!is.na(zip5)) %>%
    select(zip5, year, total_pop) %>%
    distinct()
}

ocgh_build_hsa_year_ruca_assignment <- function(
  years,
  crosswalk_file = "data/raw/ZipHsaHrr.csv",
  ruca_file = "data/raw/RUCA2010zipcode.xlsx",
  method = c("hsa_zip_count", "hsa_population_weighted"),
  zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
  census_root = "data/raw/census_raw_data"
) {
  method <- match.arg(method)
  zip_hsa <- ocgh_load_zip_hsa_lookup(crosswalk_file)
  ruca <- ocgh_load_ruca_lookup(ruca_file)

  if (method == "hsa_zip_count") {
    return(
      zip_hsa %>%
        left_join(ruca, by = "zip5") %>%
        filter(!is.na(ruca_grouped)) %>%
        group_by(hsanum, ruca_grouped, geography_type) %>%
        summarise(
          zip_count = n(),
          .groups = "drop"
        ) %>%
        group_by(hsanum) %>%
        arrange(desc(zip_count), ruca_grouped, .by_group = TRUE) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        mutate(ruca_simple = ruca_grouped) %>%
        tidyr::crossing(year = sort(unique(as.integer(years)))) %>%
        select(hsanum, year, ruca_simple, ruca_grouped, geography_type)
    )
  }

  zip_year_pop <- ocgh_load_zip_year_population(
    zip_zcta_file = zip_zcta_file,
    census_root = census_root
  )

  zip_year_pop <- bind_rows(
    zip_year_pop,
    zip_year_pop %>%
      filter(year == 2011L) %>%
      mutate(year = 2010L)
  ) %>%
    distinct(zip5, year, .keep_all = TRUE)

  tidyr::crossing(
    year = sort(unique(as.integer(years))),
    zip_hsa
  ) %>%
    left_join(ruca, by = "zip5") %>%
    left_join(zip_year_pop, by = c("zip5", "year")) %>%
    filter(!is.na(ruca_grouped)) %>%
    group_by(hsanum, year, ruca_grouped, geography_type) %>%
    summarise(
      total_pop = sum(total_pop, na.rm = TRUE),
      zip_count = n(),
      .groups = "drop"
  ) %>%
    group_by(hsanum, year) %>%
    arrange(desc(total_pop), desc(zip_count), ruca_grouped, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    # These HSAs have mechanical ties caused by two ZIPs sharing one ZCTA.
    mutate(
      ruca_grouped = if_else(
        hsanum %in% ocgh_manual_rural_small_town_hsas,
        "Rural & Small Town",
        ruca_grouped
      ),
      geography_type = if_else(
        hsanum %in% ocgh_manual_rural_small_town_hsas,
        "Rural & Small Town",
        geography_type
      )
    ) %>%
    mutate(ruca_simple = ruca_grouped) %>%
    select(hsanum, year, ruca_simple, ruca_grouped, geography_type)
}

ocgh_attach_hsa_panel_assignment <- function(
  df,
  crosswalk_file = "data/raw/ZipHsaHrr.csv",
  ruca_file = "data/raw/RUCA2010zipcode.xlsx",
  method = c("hsa_zip_count", "hsa_population_weighted"),
  zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
  census_root = "data/raw/census_raw_data"
) {
  method <- match.arg(method)
  assignments <- ocgh_build_hsa_year_ruca_assignment(
    years = df %>% distinct(year) %>% pull(year),
    crosswalk_file = crosswalk_file,
    ruca_file = ruca_file,
    method = method,
    zip_zcta_file = zip_zcta_file,
    census_root = census_root
  )

  df %>% left_join(assignments, by = c("hsanum", "year"))
}
