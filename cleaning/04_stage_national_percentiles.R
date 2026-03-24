# Build/stage national HSA percentiles into data/interim.
# Usage: source("cleaning/04_stage_national_percentiles.R"); stage_national_percentiles()

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
  pos_path = "data/processed/pos_panel_updated.csv",
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

  pos_resolved <- resolve_input_path(pos_path, "Processed POS panel")
  crosswalk_resolved <- resolve_input_path(crosswalk_path, "ZIP-HSA crosswalk")

  pos <- read_csv(pos_resolved, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(year),
      active = as.integer(active),
      certbeds = as.numeric(certbeds)
    ) %>%
    filter(active == 1, !is.na(year), !is.na(certbeds))

  zip_hsa <- read_csv(crosswalk_resolved, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()

  hsa_certbeds <- pos %>%
    left_join(zip_hsa, by = "zip5") %>%
    filter(!is.na(hsanum)) %>%
    group_by(hsanum, year) %>%
    summarise(total_certbeds = sum(certbeds, na.rm = TRUE), .groups = "drop")

  ntl <- ntl %>%
    left_join(hsa_certbeds, by = c("hsanum", "year")) %>%
    arrange(hsanum, year) %>%
    group_by(hsanum) %>%
    mutate(
      certbeds_per_1000_residents = if_else(
        !is.na(total_certbeds) & !is.na(sum_total_pop_event) & sum_total_pop_event > 0,
        (total_certbeds / sum_total_pop_event) * 1000,
        NA_real_
      ),
      certbeds_per_1000_residents_lag1 = lag(certbeds_per_1000_residents)
    ) %>%
    ungroup()

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(interim_dir, "ntl_hsa_percentiles.csv")
  write_csv(ntl, dest)
  invisible(ntl)
}
