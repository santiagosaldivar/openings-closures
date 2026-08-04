# Build HSA-level 2010 Decennial Census population from the SF1 P1 ZCTA file.
# Usage: source("cleaning/03b_clean_census2010_pop.R"); clean_census2010_pop()
#
# PURPOSE
#   Provides the population denominator for the 2010 and 2011 values of
#   certbeds_per_1000_residents_lag1 (staged in 04_stage_national_percentiles.R).
#   No ACS 5-year ZCTA population exists for 2009 or 2010 in this pipeline, so:
#     - year 2010 uses a CONTEMPORANEOUS denominator (2010 Census population)
#       with a lagged numerator (2009 beds);
#     - year 2011 is fully lagged as usual (2010 beds / 2010 Census population).
#
# INPUT FORMAT (verified comma-delimited despite resembling TSV in some viewers)
#   Row 1: column names (GEO_ID, NAME, P001001, plus a trailing empty column)
#   Row 2: human-readable descriptors -- dropped, never parsed as data
#   Rows 3+: one row per ZCTA; NAME is "ZCTA5 00601"
#
# ZCTA -> HSA AGGREGATION
#   Mirrors 10_rebuild_ntl_hsa_percentiles_from_raw.R exactly:
#   ZipHsaHrr.csv (ZIP -> HSA) joined to ZIPCodetoZCTACrosswalk2022UDS.xlsx
#   (ZIP -> ZCTA), deduplicated to one row per (hsanum, zcta), then P001001
#   summed by HSA. This matches the construction of sum_total_pop_event for
#   ACS years, so 2010/2011 denominators are conceptually comparable to the
#   denominators used from 2012 onward.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
})

resolve_input_path <- function(primary_path, label) {
  if (file.exists(primary_path)) return(primary_path)
  stop(label, " not found at: ", primary_path)
}

#' Build HSA-level 2010 Decennial Census population
#'
#' @param census_file Decennial SF1 P1 ZCTA data file
#' @param crosswalk_file ZIP -> HSA crosswalk (ZipHsaHrr.csv)
#' @param zip_zcta_file ZIP -> ZCTA crosswalk (UDS 2022)
#' @param output_path Destination CSV (data/processed)
#' @return tibble (hsanum, pop2010_census) invisibly
clean_census2010_pop <- function(
    census_file = "data/raw/census_raw_data/DECENNIALSF12010/DECENNIALSF12010.P1-Data.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
    output_path = "data/processed/hsa_census2010_pop.csv"
) {
  census_path <- resolve_input_path(census_file, "2010 Decennial SF1 P1 file")
  crosswalk_path <- resolve_input_path(crosswalk_file, "ZIP-HSA crosswalk")
  zip_zcta_path <- resolve_input_path(zip_zcta_file, "ZIP-ZCTA crosswalk")

  # Read everything as character so P001001 is only cast AFTER the descriptor
  # row (row 2 of the file, first data row here) is dropped.
  raw <- read_csv(
    census_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  required_cols <- c("GEO_ID", "NAME", "P001001")
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop("Decennial P1 file is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  zcta_pop <- raw %>%
    slice(-1) %>%  # drop the human-readable descriptor row
    transmute(
      # Keep as character; leading zeros are meaningful.
      zcta = str_remove(NAME, "^ZCTA5\\s+"),
      pop2010 = suppressWarnings(as.numeric(P001001))
    ) %>%
    filter(str_detect(zcta, "^\\d{5}$"))

  if (nrow(zcta_pop) == 0) stop("No ZCTA rows parsed from the Decennial P1 file.")

  n_na_pop <- sum(is.na(zcta_pop$pop2010))
  if (n_na_pop > 0) {
    warning(n_na_pop, " ZCTA row(s) have non-numeric P001001; kept as NA.")
  }

  dup_zcta <- zcta_pop %>% count(zcta) %>% filter(n > 1) %>% nrow()
  if (dup_zcta > 0) {
    stop(dup_zcta, " duplicate ZCTA(s) in the Decennial P1 file.")
  }

  zip_hsa <- read_csv(crosswalk_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()

  multi_hsa_zips <- zip_hsa %>% count(zip5) %>% filter(n > 1) %>% nrow()
  if (multi_hsa_zips > 0) {
    stop(multi_hsa_zips, " ZIP code(s) map to multiple HSAs in the crosswalk.")
  }

  zip_zcta <- read_excel(zip_zcta_path) %>%
    transmute(
      zip5 = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      zcta = str_pad(as.character(zcta), width = 5, side = "left", pad = "0")
    ) %>%
    distinct()

  # One row per (hsanum, zcta): a ZCTA spanning several ZIPs of the same HSA
  # must be counted once, matching the group_by(zcta, hsanum, year) dedup in
  # 10_rebuild_ntl_hsa_percentiles_from_raw.R.
  hsa_zcta <- zip_hsa %>%
    left_join(zip_zcta, by = "zip5") %>%
    filter(!is.na(zcta), !is.na(hsanum)) %>%
    distinct(hsanum, zcta)

  hsa_pop <- hsa_zcta %>%
    inner_join(zcta_pop, by = "zcta") %>%
    group_by(hsanum) %>%
    summarise(
      pop2010_census = if (all(is.na(pop2010))) NA_real_ else sum(pop2010, na.rm = TRUE),
      n_zctas_2010 = n(),
      .groups = "drop"
    )

  dup_hsa <- hsa_pop %>% count(hsanum) %>% filter(n > 1) %>% nrow()
  if (dup_hsa > 0) stop(dup_hsa, " duplicate hsanum row(s) in HSA population output.")

  n_zero <- sum(!is.na(hsa_pop$pop2010_census) & hsa_pop$pop2010_census == 0)
  n_na <- sum(is.na(hsa_pop$pop2010_census))
  message("HSA 2010 Census population: ", nrow(hsa_pop), " HSAs (",
          n_zero, " zero, ", n_na, " NA).")

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(hsa_pop, output_path)
  invisible(hsa_pop)
}
