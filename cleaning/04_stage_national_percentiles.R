# Build/stage national HSA percentiles into data/interim.
# Usage: source("cleaning/04_stage_national_percentiles.R"); stage_national_percentiles()
#
# CHANGES (facility count addition):
#   - `ccn` pulled through from POS; `!is.na(certbeds)` dropped from the row
#     filter so that hospitals with unreported beds still count as facilities.
#   - `total_certbeds` guarded so all-NA HSA-years remain NA rather than
#     becoming 0. Values should be identical to the previous version.
#   - New columns: n_facilities, n_facilities_lag1.
#   - Hard stops added for duplicate ccn-year rows, multi-HSA ZIPs, duplicate
#     HSA-year rows in the staged file, and non-contiguous year sequences.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

resolve_input_path <- function(primary_path, label) {
  if (file.exists(primary_path)) return(primary_path)
  stop(label, " not found at: ", primary_path)
}

#' Load national HSA percentiles and stage to data/interim
#'
#' @param source_path Preferred source path (default "data/raw/ntl_hsa_percentiles.csv")
#' @param interim_dir Destination directory (default "data/interim")
#' @return tibble invisibly
stage_national_percentiles <- function(
    source_path = "data/raw/ntl_hsa_percentiles.csv",
    interim_dir = "data/interim",
    pos_path = "data/processed/pos_panel_reconciled.csv",
    crosswalk_path = "data/raw/ZipHsaHrr.csv"
) {
  resolved <- resolve_input_path(source_path, "National percentile source")
  ntl <- read_csv(resolved, show_col_types = FALSE)
  
  required_cols <- c(
    "hsanum",
    "year",
    "sum_total_pop_event",
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
  missing_cols <- setdiff(required_cols, names(ntl))
  if (length(missing_cols) > 0) {
    stop("National percentile source is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # --- staged file must be unique by HSA-year; the group-wise lag assumes it ---
  dup_hsa_year <- ntl %>% count(hsanum, year) %>% filter(n > 1) %>% nrow()
  if (dup_hsa_year > 0) {
    stop("National percentile source has ", dup_hsa_year,
         " hsanum-year combinations with multiple rows.")
  }
  
  pos_resolved <- resolve_input_path(pos_path, "Processed POS panel")
  crosswalk_resolved <- resolve_input_path(crosswalk_path, "ZIP-HSA crosswalk")
  
  # CHANGED: carry ccn through; drop !is.na(certbeds) so that a hospital with
  # unreported beds is still counted as an operating facility.
  pos <- read_csv(pos_resolved, show_col_types = FALSE) %>%
    transmute(
      ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"),
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(year),
      active = as.integer(active),
      certbeds = as.numeric(certbeds)
    ) %>%
    filter(active == 1, !is.na(year))
  
  # One row per hospital-year is assumed by BOTH the bed sum and the facility
  # count. If this fires, total_certbeds is already double counting.
  dup_ccn_year <- pos %>% count(ccn, year) %>% filter(n > 1) %>% nrow()
  if (dup_ccn_year > 0) {
    stop("POS panel has ", dup_ccn_year,
         " ccn-year combinations with multiple rows. ",
         "Both total_certbeds and n_facilities assume one row per hospital-year.")
  }
  
  zip_hsa <- read_csv(crosswalk_resolved, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()
  
  # A ZIP mapping to more than one HSA would duplicate hospitals across HSAs,
  # inflating both bed sums and facility counts.
  multi_hsa_zips <- zip_hsa %>% count(zip5) %>% filter(n > 1) %>% nrow()
  if (multi_hsa_zips > 0) {
    stop(multi_hsa_zips, " ZIP code(s) map to multiple HSAs in the crosswalk. ",
         "Hospitals in these ZIPs would be counted in more than one HSA.")
  }
  
  # CHANGED: adds n_facilities; total_certbeds guarded to preserve NA for
  # HSA-years in which no active hospital reported a bed count.
  hsa_supply <- pos %>%
    left_join(zip_hsa, by = "zip5") %>%
    filter(!is.na(hsanum)) %>%
    group_by(hsanum, year) %>%
    summarise(
      n_facilities = n_distinct(ccn),
      total_certbeds = if (all(is.na(certbeds))) NA_real_ else sum(certbeds, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Positional lag() below is only correct when each HSA's years are contiguous.
  gaps <- ntl %>%
    distinct(hsanum, year) %>%
    count(hsanum, name = "n_years") %>%
    left_join(
      ntl %>%
        group_by(hsanum) %>%
        summarise(span = as.integer(max(year) - min(year) + 1L), .groups = "drop"),
      by = "hsanum"
    ) %>%
    filter(n_years != span)
  
  if (nrow(gaps) > 0) {
    stop(nrow(gaps), " HSA(s) have non-contiguous year sequences. ",
         "Positional lag() would return the wrong year for these.")
  }
  
  ntl <- ntl %>%
    left_join(hsa_supply, by = c("hsanum", "year")) %>%
    arrange(hsanum, year) %>%
    group_by(hsanum) %>%
    mutate(
      certbeds_per_1000_residents = if_else(
        !is.na(total_certbeds) & !is.na(sum_total_pop_event) & sum_total_pop_event > 0,
        (total_certbeds / sum_total_pop_event) * 1000,
        NA_real_
      ),
      certbeds_per_1000_residents_lag1 = lag(certbeds_per_1000_residents),
      # NEW. Left NA-preserving here, matching the total_certbeds convention.
      # Zero-fill for the closure risk set happens downstream in the analysis
      # script, where the at-risk logic lives.
      n_facilities_lag1 = lag(n_facilities)
    ) %>%
    ungroup()
  
  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(interim_dir, "ntl_hsa_percentiles.csv")
  write_csv(ntl, dest)
  invisible(ntl)
}