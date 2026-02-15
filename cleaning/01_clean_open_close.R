# Clean the openings/closures raw files and write standardized interim outputs.
# Usage: source("R/cleaning/01_clean_open_close.R"); clean_open_close()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

#' Clean the openings and closures datasets
#'
#' @param raw_dir Directory containing the raw CSVs (default "data/raw")
#' @param interim_dir Directory to write cleaned CSVs (default "data/interim")
#' @param openings_file Raw openings filename
#' @param closures_file Raw closures filename
#' @return list with cleaned openings and closures tibbles
clean_open_close <- function(
  raw_dir = "data/raw",
  interim_dir = "data/interim",
  openings_file = "updated_openings_august2025.csv",
  closures_file = "updated_closures_august2025.csv"
) {
  openings_path <- file.path(raw_dir, openings_file)
  closures_path <- file.path(raw_dir, closures_file)

  if (!file.exists(openings_path)) {
    stop("Openings file not found: ", openings_path)
  }
  if (!file.exists(closures_path)) {
    stop("Closures file not found: ", closures_path)
  }

  openings <- read_csv(openings_path, show_col_types = FALSE)
  closures <- read_csv(closures_path, show_col_types = FALSE)

  cleaned_openings <- openings %>%
    filter(part_year >= 2010) %>%
    mutate(ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"))

  cleaned_closures <- closures %>%
    filter(term_year >= 2010) %>%
    mutate(ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"))

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(cleaned_openings, file.path(interim_dir, "openings_clean.csv"))
  write_csv(cleaned_closures, file.path(interim_dir, "closures_clean.csv"))

  invisible(list(openings = cleaned_openings, closures = cleaned_closures))
}

