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
      # `Urban` pools Metropolitan and Micropolitan RUCA codes. This is the
      # two-level analysis variable; `ruca_grouped` retains the Metropolitan /
      # Micropolitan split for descriptive figures only.
      geography_type = case_when(
        ruca_simple %in% c("Metropolitan", "Micropolitan") ~ "Urban",
        ruca_simple %in% c("Rural", "Small Town") ~ "Rural & Small Town",
        TRUE ~ NA_character_
      )
    ) %>%
    distinct(zip5, .keep_all = TRUE)
}

# Assign each HSA (or HSA-year) a geography and a nested sub-bucket label.
#
# `bucket_totals` carries one row per key x ruca_grouped x geography_type with
# weights `w1` (primary: population or ZIP count) and `w2` (tie-break).
#
# Selection is two-stage and strictly nested:
#   1. Pool Metropolitan and Micropolitan weight into `Urban`, then let `Urban`
#      compete against `Rural & Small Town`. The larger pooled weight wins.
#   2. Within the winning geography only, pick the largest sub-bucket to produce
#      the descriptive `ruca_grouped` label.
#
# Pooling before the comparison is what makes `Urban` a genuine pooled category:
# an HSA whose Metropolitan and Micropolitan populations jointly exceed its
# Rural & Small Town population is Urban even when neither exceeds it alone.
# Nesting stage 2 inside the stage 1 winner guarantees that the Metropolitan and
# Micropolitan counts sum exactly to the Urban count, so the three-line and
# two-line figures agree by construction.
#
# Ties fall to the alphabetically first label, which sends geography ties to
# `Rural & Small Town` and sub-bucket ties to `Metropolitan`.
ocgh_select_nested_assignment <- function(bucket_totals, keys) {
  geography_winner <- bucket_totals %>%
    group_by(across(all_of(c(keys, "geography_type")))) %>%
    summarise(w1 = sum(w1), w2 = sum(w2), .groups = "drop") %>%
    group_by(across(all_of(keys))) %>%
    arrange(desc(w1), desc(w2), geography_type, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(all_of(c(keys, "geography_type")))

  bucket_totals %>%
    inner_join(geography_winner, by = c(keys, "geography_type")) %>%
    group_by(across(all_of(keys))) %>%
    arrange(desc(w1), desc(w2), ruca_grouped, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(all_of(c(keys, "ruca_grouped", "geography_type")))
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
    bucket_totals <- zip_hsa %>%
      left_join(ruca, by = "zip5") %>%
      filter(!is.na(geography_type)) %>%
      group_by(hsanum, ruca_grouped, geography_type) %>%
      summarise(
        w1 = n(),
        w2 = n(),
        .groups = "drop"
      )

    return(
      ocgh_select_nested_assignment(bucket_totals, keys = "hsanum") %>%
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

  bucket_totals <- tidyr::crossing(
    year = sort(unique(as.integer(years))),
    zip_hsa
  ) %>%
    left_join(ruca, by = "zip5") %>%
    left_join(zip_year_pop, by = c("zip5", "year")) %>%
    filter(!is.na(geography_type)) %>%
    group_by(hsanum, year, ruca_grouped, geography_type) %>%
    summarise(
      w1 = sum(total_pop, na.rm = TRUE),
      w2 = n(),
      .groups = "drop"
    )

  ocgh_select_nested_assignment(bucket_totals, keys = c("hsanum", "year")) %>%
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
