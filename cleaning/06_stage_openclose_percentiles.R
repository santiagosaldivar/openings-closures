# Build opening/closure/nonevent percentile table from event HSAs + national percentiles.
# Usage: source("cleaning/06_stage_openclose_percentiles.R"); stage_openclose_percentiles()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

resolve_input_path <- function(primary_path, label) {
  if (file.exists(primary_path)) return(primary_path)
  stop(label, " not found at: ", primary_path)
}

percentile_rank <- function(x, value) {
  mean(x <= value, na.rm = TRUE) * 100
}

add_percentile_cols <- function(df, ntl, vars_to_rank) {
  df %>%
    rowwise() %>%
    mutate(
      across(
        all_of(vars_to_rank),
        ~ percentile_rank(ntl[[cur_column()]][ntl$year == year], .x),
        .names = "{.col}_percentile"
      )
    ) %>%
    ungroup()
}

#' Build opening/closure/nonevent percentiles into data/interim
#'
#' @param openings_file Preferred openings file
#' @param closures_file Preferred closures file
#' @param crosswalk_file Preferred ZIP-HSA crosswalk
#' @param national_percentiles_file Preferred national percentile source file
#' @param interim_dir Destination directory (default "data/interim")
#' @return tibble invisibly
stage_openclose_percentiles <- function(
  openings_file = "data/raw/updated_openings_august2025.csv",
  closures_file = "data/raw/updated_closures_august2025.csv",
  crosswalk_file = "data/raw/ZipHsaHrr.csv",
  national_percentiles_file = "data/interim/ntl_hsa_percentiles.csv",
  interim_dir = "data/interim"
) {
  openings_path <- resolve_input_path(openings_file, "Openings file")
  closures_path <- resolve_input_path(closures_file, "Closures file")
  crosswalk_path <- resolve_input_path(crosswalk_file, "ZIP-HSA crosswalk")
  national_path <- resolve_input_path(national_percentiles_file, "National percentile file")

  openings <- read_csv(openings_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      event_year = as.integer(part_year),
      potential_open = 1L,
      potential_closure = 0L
    )

  closures <- read_csv(closures_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      event_year = as.integer(term_year),
      potential_open = 0L,
      potential_closure = 1L
    )

  zip_hsa <- read_csv(crosswalk_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()

  hsa_events <- bind_rows(openings, closures) %>%
    filter(event_year >= 2010, !is.na(event_year)) %>%
    left_join(zip_hsa, by = "zip5")

  ntl <- read_csv(national_path, show_col_types = FALSE)

  vars_to_rank <- c(
    "weighted_median_household_income_event",
    "weighted_percent_any_health_insur_event",
    "weighted_percent_public_health_insur_event",
    "weighted_unemployment_rate_event",
    "weighted_percent_bachelors_event",
    "weighted_percent_black_event",
    "weighted_percent_hispanic_or_latino_event",
    "weighted_percent_below_poverty_line_event",
    "weighted_SDI_score_event",
    "population_density",
    "pop_change_pct"
  )

  missing_cols <- setdiff(c("hsanum", "year", vars_to_rank), names(ntl))
  if (length(missing_cols) > 0) {
    stop("National percentile file is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  openings_events <- hsa_events %>%
    filter(potential_open == 1, !is.na(hsanum), !is.na(event_year)) %>%
    distinct(hsanum, event_year) %>%
    transmute(hsanum, year = event_year, opening = 1L, closure = NA_integer_)

  closures_events <- hsa_events %>%
    filter(potential_closure == 1, !is.na(hsanum), !is.na(event_year)) %>%
    distinct(hsanum, event_year) %>%
    transmute(hsanum, year = event_year, closure = 1L, opening = NA_integer_)

  openings_with_stats <- openings_events %>%
    left_join(ntl, by = c("hsanum", "year")) %>%
    filter(year >= 2010) %>%
    add_percentile_cols(ntl = ntl, vars_to_rank = vars_to_rank) %>%
    mutate(group = "Opening")

  closures_with_stats <- closures_events %>%
    left_join(ntl, by = c("hsanum", "year")) %>%
    filter(year >= 2010) %>%
    add_percentile_cols(ntl = ntl, vars_to_rank = vars_to_rank) %>%
    mutate(group = "Closure")

  event_keys <- bind_rows(
    openings_with_stats %>% select(hsanum, year),
    closures_with_stats %>% select(hsanum, year)
  ) %>% distinct()

  non_event <- ntl %>%
    filter(year >= 2010) %>%
    anti_join(event_keys, by = c("hsanum", "year")) %>%
    mutate(opening = 0L, closure = 0L) %>%
    add_percentile_cols(ntl = ntl, vars_to_rank = vars_to_rank) %>%
    mutate(group = "non-event")

  df_all <- bind_rows(openings_with_stats, closures_with_stats, non_event) %>%
    rename(
      income_percentile = weighted_median_household_income_event_percentile,
      health_insurance_percentile = weighted_percent_any_health_insur_event_percentile,
      public_health_insurance_percentile = weighted_percent_public_health_insur_event_percentile,
      unemployment_rate_percentile = weighted_unemployment_rate_event_percentile,
      bachelors_percentile = weighted_percent_bachelors_event_percentile,
      black_percentile = weighted_percent_black_event_percentile,
      latino_percentile = weighted_percent_hispanic_or_latino_event_percentile,
      poverty_percentile = weighted_percent_below_poverty_line_event_percentile,
      SDI_percentile = weighted_SDI_score_event_percentile
    )

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(interim_dir, "opening_closure_nonevent_percentiles.csv")
  write_csv(df_all, dest)
  invisible(df_all)
}
