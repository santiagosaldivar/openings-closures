# Optional helper to place/validate ntl_hsa_percentiles.csv in data/raw.
# Usage:
#   source("cleaning/08_optional_prepare_ntl_hsa_percentiles.R")
#   prepare_ntl_hsa_percentiles()

suppressPackageStartupMessages({
  library(readr)
})

resolve_input_path <- function(primary_path, label) {
  if (!is.null(primary_path) && file.exists(primary_path)) return(primary_path)
  stop(label, " not found at: ", primary_path)
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
    stop("ntl_hsa_percentiles is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
}

#' Prepare ntl_hsa_percentiles.csv in data/raw
#'
#' @param source_path Source path (default "data/raw/ntl_hsa_percentiles.csv")
#' @param raw_output_path Destination path (default "data/raw/ntl_hsa_percentiles.csv")
#' @return destination path invisibly
prepare_ntl_hsa_percentiles <- function(
  source_path = "data/raw/ntl_hsa_percentiles.csv",
  raw_output_path = "data/raw/ntl_hsa_percentiles.csv"
) {
  resolved <- resolve_input_path(primary_path = source_path, label = "Source ntl_hsa_percentiles.csv")

  ntl <- read_csv(resolved, show_col_types = FALSE)
  validate_ntl_hsa_percentiles(ntl)

  dir.create(dirname(raw_output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(ntl, raw_output_path)
  invisible(raw_output_path)
}
