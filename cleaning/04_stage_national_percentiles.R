# Build/stage national HSA percentiles into data/interim.
# Usage: source("cleaning/04_stage_national_percentiles.R"); stage_national_percentiles()

suppressPackageStartupMessages({
  library(readr)
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
  interim_dir = "data/interim"
) {
  resolved <- resolve_input_path(source_path, "National percentile source")
  ntl <- read_csv(resolved, show_col_types = FALSE)

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
  missing_cols <- setdiff(required_cols, names(ntl))
  if (length(missing_cols) > 0) {
    stop("National percentile source is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(interim_dir, "ntl_hsa_percentiles.csv")
  write_csv(ntl, dest)
  invisible(ntl)
}
