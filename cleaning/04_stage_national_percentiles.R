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
#
# CHANGES (2010/2011 beds-per-1000 addition):
#   - certbeds_per_1000_residents_lag1 now populated for 2010 and 2011 using
#     the 2010 Decennial Census population (data/processed/hsa_census2010_pop.csv,
#     built by 03b_clean_census2010_pop.R):
#       * 2010: lagged numerator (2009 beds), CONTEMPORANEOUS denominator
#         (2010 Census population) -- no 2009 population source exists.
#       * 2011: fully lagged as usual (2010 beds / 2010 Census population),
#         flowing through the unlagged 2010 value patched below.
#   - The POS panel now includes 2009 (see 02_clean_pos.R). 2009 enters ONLY
#     as a lag-source bed total; it is filtered out of hsa_supply before the
#     join, so it can never become an observation year in the staged file.
#   - All other years' definitions are unchanged (ACS denominators, lagged).

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
    crosswalk_path = "data/raw/ZipHsaHrr.csv",
    census2010_path = "data/processed/hsa_census2010_pop.csv"
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

  # --- 2009 beds: lag source ONLY, never an observation year ------------------
  # Extracted as one row per HSA, then 2009 is dropped from hsa_supply so the
  # join below cannot introduce a 2009 row even if the ntl source ever changed.
  beds_2009 <- hsa_supply %>%
    filter(year == 2009L) %>%
    select(hsanum, total_certbeds_2009 = total_certbeds)

  hsa_supply <- hsa_supply %>%
    filter(year >= 2010L)

  # --- 2010 Decennial Census population (denominator for 2010/2011) ----------
  census2010_resolved <- resolve_input_path(census2010_path, "HSA 2010 Census population")
  hsa_pop2010 <- read_csv(census2010_resolved, show_col_types = FALSE) %>%
    transmute(
      hsanum = as.integer(hsanum),
      pop2010_census = as.numeric(pop2010_census)
    )
  dup_pop_hsa <- hsa_pop2010 %>% count(hsanum) %>% filter(n > 1) %>% nrow()
  if (dup_pop_hsa > 0) {
    stop(dup_pop_hsa, " duplicate hsanum row(s) in HSA 2010 Census population file.")
  }
  
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
    left_join(hsa_pop2010, by = "hsanum") %>%
    left_join(beds_2009, by = "hsanum") %>%
    arrange(hsanum, year) %>%
    group_by(hsanum) %>%
    mutate(
      certbeds_per_1000_residents = if_else(
        !is.na(total_certbeds) & !is.na(sum_total_pop_event) & sum_total_pop_event > 0,
        (total_certbeds / sum_total_pop_event) * 1000,
        NA_real_
      ),
      # 2010: the ACS ZCTA population series begins in 2011, so
      # sum_total_pop_event is NA in 2010 and the line above yields NA. Patch
      # the contemporaneous 2010 value with the 2010 Decennial Census
      # denominator. Via lag() below, this also supplies the fully-lagged 2011
      # value (2010 beds / 2010 Census population).
      certbeds_per_1000_residents = if_else(
        year == 2010L & !is.na(total_certbeds) &
          !is.na(pop2010_census) & pop2010_census > 0,
        (total_certbeds / pop2010_census) * 1000,
        certbeds_per_1000_residents
      ),
      certbeds_per_1000_residents_lag1 = lag(certbeds_per_1000_residents),
      # 2010 lagged value: numerator lagged (2009 beds), denominator NOT
      # lagged (2010 Census population) -- no 2009 population source exists in
      # the pipeline. This is the ONLY place 2009 data enter; 2009 is never an
      # observation year. NA-preserving guards match the conventions above.
      certbeds_per_1000_residents_lag1 = if_else(
        year == 2010L,
        if_else(
          !is.na(total_certbeds_2009) & !is.na(pop2010_census) & pop2010_census > 0,
          (total_certbeds_2009 / pop2010_census) * 1000,
          NA_real_
        ),
        certbeds_per_1000_residents_lag1
      ),
      # NEW. Left NA-preserving here, matching the total_certbeds convention.
      # Zero-fill for the closure risk set happens downstream in the analysis
      # script, where the at-risk logic lives.
      n_facilities_lag1 = lag(n_facilities)
    ) %>%
    ungroup() %>%
    # Helper columns dropped so the staged schema is unchanged apart from the
    # newly populated 2010/2011 values.
    select(-pop2010_census, -total_certbeds_2009)
  
  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(interim_dir, "ntl_hsa_percentiles.csv")
  write_csv(ntl, dest)
  invisible(ntl)
}