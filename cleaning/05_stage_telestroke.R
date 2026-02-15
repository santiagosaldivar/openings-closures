# Build telestroke adoption dataset with percentiles.
# Usage: source("cleaning/05_stage_telestroke.R"); stage_telestroke_dataset()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(lubridate)
})

resolve_input_path <- function(primary_path, label) {
  if (file.exists(primary_path)) return(primary_path)
  stop(label, " not found at: ", primary_path)
}

percentile_rank <- function(x, value) {
  mean(x <= value, na.rm = TRUE) * 100
}

#' Build telestroke dataset into data/processed
#'
#' @param telestroke_file Preferred telestroke raw file
#' @param crosswalk_file Preferred ZIP-HSA crosswalk file
#' @param national_percentiles_file Preferred staged national percentiles file
#' @param processed_dir Destination directory (default "data/processed")
#' @return tibble invisibly
stage_telestroke_dataset <- function(
  telestroke_file = "data/raw/telestroke_data.xlsx",
  crosswalk_file = "data/raw/ZipHsaHrr.csv",
  national_percentiles_file = "data/interim/ntl_hsa_percentiles.csv",
  processed_dir = "data/processed"
) {
  telestroke_path <- resolve_input_path(telestroke_file, "Telestroke raw file")
  crosswalk_path <- resolve_input_path(crosswalk_file, "ZIP-HSA crosswalk")
  national_path <- resolve_input_path(national_percentiles_file, "National percentile file")

  telestroke_data <- read_excel(telestroke_path) %>%
    mutate(
      `Zip Code` = str_pad(as.character(`Zip Code`), width = 5, side = "left", pad = "0"),
      start_date = suppressWarnings(ymd(start_date)),
      year = year(start_date),
      year_proxy = year,
      year_proxy = if_else(year_proxy <= 2010, 2011L, year_proxy),
      year_proxy = if_else(year_proxy >= 2024, 2023L, year_proxy)
    )

  hsa_hrr_crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE) %>%
    mutate(zipcode19 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"))

  ntl <- read_csv(national_path, show_col_types = FALSE)

  telestroke_data_updated <- telestroke_data %>%
    left_join(hsa_hrr_crosswalk, by = c("Zip Code" = "zipcode19")) %>%
    left_join(ntl, by = c("hsanum" = "hsanum", "year_proxy" = "year"))

  vars_to_rank <- c(
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

  missing_cols <- setdiff(vars_to_rank, names(telestroke_data_updated))
  if (length(missing_cols) > 0) {
    stop("Telestroke build is missing required columns after merge: ",
         paste(missing_cols, collapse = ", "))
  }

  telestroke_data_updated <- telestroke_data_updated %>%
    rowwise() %>%
    mutate(
      across(
        all_of(vars_to_rank),
        ~ percentile_rank(ntl[[cur_column()]][ntl$year == year_proxy], .x),
        .names = "{.col}_percentile"
      )
    ) %>%
    ungroup() %>%
    mutate(
      ruca_simple = case_when(
        `RUCA 1` %in% c(1, 2, 3) ~ "metropolitan",
        `RUCA 1` %in% c(4, 5, 6) ~ "micropolitan",
        `RUCA 1` %in% c(7, 8, 9) ~ "small town",
        `RUCA 1` == 10 ~ "rural",
        TRUE ~ NA_character_
      )
    ) %>%
    rename(
      income_percentile = weighted_median_household_income_event_percentile,
      any_health_insurance_percentile = weighted_percent_any_health_insur_event_percentile,
      public_health_insurance_percentile = weighted_percent_public_health_insur_event_percentile,
      unemployment_rate_percentile = weighted_unemployment_rate_event_percentile,
      bachelors_percentile = weighted_percent_bachelors_event_percentile,
      black_percentile = weighted_percent_black_event_percentile,
      latino_percentile = weighted_percent_hispanic_or_latino_event_percentile,
      poverty_percentile = weighted_percent_below_poverty_line_event_percentile,
      SDI_percentile = weighted_SDI_score_event_percentile,
      income = weighted_median_household_income_event,
      any_health_insurance = weighted_percent_any_health_insur_event,
      public_health_insurance = weighted_percent_public_health_insur_event,
      unemployment_rate = weighted_unemployment_rate_event,
      bachelors_degree = weighted_percent_bachelors_event,
      black = weighted_percent_black_event,
      latino = weighted_percent_hispanic_or_latino_event,
      poverty_rate = weighted_percent_below_poverty_line_event,
      SDI = weighted_SDI_score_event
    )

  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(processed_dir, "telestroke_adopt_w_percentiles.csv")
  write_csv(telestroke_data_updated, dest)
  invisible(telestroke_data_updated)
}
