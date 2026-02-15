# Clean the POS panel file according to study inclusion rules.
# Usage: source("R/cleaning/02_clean_pos.R"); clean_pos()

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(readr)
})

#' Clean the POS panel data
#'
#' @param raw_dir Directory containing raw inputs (default "data/raw")
#' @param processed_dir Directory to write cleaned panel (default "data/processed")
#' @param pos_file Raw POS panel filename
#' @param do_not_exclude_file CSV listing hospitals to force-include
#' @param exclude_file CSV listing specialty hospitals to exclude
#' @return tibble of cleaned POS panel
clean_pos <- function(
  raw_dir = "data/raw",
  processed_dir = "data/processed",
  pos_file = "pos_panel_2009_2024.dta",
  do_not_exclude_file = "pos_do_not_exclude.csv",
  exclude_file = "POS_double_checking_exclude.csv"
) {
  pos_path <- file.path(raw_dir, pos_file)
  include_path <- file.path(raw_dir, do_not_exclude_file)
  exclude_path <- file.path(raw_dir, exclude_file)

  if (!file.exists(pos_path)) stop("POS panel not found: ", pos_path)
  if (!file.exists(include_path)) stop("Do-not-exclude list not found: ", include_path)
  if (!file.exists(exclude_path)) stop("Exclude list not found: ", exclude_path)

  raw_pos_data <- read_dta(pos_path)
  hospitals_dont_exclude <- read_csv(include_path, show_col_types = FALSE)
  excluded_hospitals <- read_csv(exclude_path, show_col_types = FALSE)

  matched_hospitals <- raw_pos_data %>%
    semi_join(hospitals_dont_exclude, by = c("hospname" = "Name"))

  n_matched <- dplyr::n_distinct(matched_hospitals$hospname)
  n_do_not_exclude <- dplyr::n_distinct(hospitals_dont_exclude$Name)

  if (n_matched != n_do_not_exclude) {
    stop(
      sprintf(
        "Hospital match FAILED: %d matched vs %d in hospitals_dont_exclude",
        n_matched, n_do_not_exclude
      )
    )
  }

  matching_hospnames <- matched_hospitals %>%
    distinct(hospname) %>%
    pull(hospname)

  pos_panel_updated <- raw_pos_data %>%
    mutate(
      specialty_hosp = if_else(hospname %in% matching_hospnames, 0, specialty_hosp),
      year = as.integer(year)
    ) %>%
    filter(
      specialty_hosp != 1,
      between(year, 2010, 2023)
    ) %>%
    filter(is.na(control) | !(control %in% c(3, 10))) %>%
    anti_join(excluded_hospitals, by = c("hospname" = "name"))

  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(pos_panel_updated, file.path(processed_dir, "pos_panel_updated.csv"))

  invisible(pos_panel_updated)
}

