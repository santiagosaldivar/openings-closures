# Unique active hospitals (CCNs) by Census region, 2010-2023.
# Usage:
#   source("analysis/14_active_hospitals_by_census_region.R")
#   build_active_hospitals_by_census_region()
#
# PURPOSE
#   Reproducible denominator for the manuscript sentence reporting how many
#   unique hospitals were active in each Census region between 2010 and 2023.
#   Earlier manuscript values (570/1342/1884/920, total 4716) were computed
#   from the PRE-reconciliation panel (data/processed/pos_panel_updated.csv);
#   this script uses the reconciled panel that every other analysis uses.
#
#   Counts are produced under BOTH region-assignment methods, recorded in the
#   `assignment_method` column:
#     - "hospital_state": region from the hospital's own state in the POS
#       panel (`state_abbrev`). Partitions CCNs cleanly; region counts sum to
#       the unique CCN total.
#     - "zip_hsastate": region from the hospital ZIP via ZipHsaHrr `hsastate`,
#       mirroring analysis/13_event_counts_by_census_region.R. A CCN whose
#       ZIP is missing from the crosswalk lands in "Unmatched", and a CCN
#       whose ZIP/state changes across years can appear in more than one
#       region, so region counts can sum to slightly more than the unique
#       total. The count of such multi-region CCNs is reported in the
#       `n_ccns_in_multiple_regions` column of the "Total unique CCNs" row.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

build_active_hospitals_by_census_region <- function(
  pos_path = "data/processed/pos_panel_reconciled.csv",
  zip_hsa_path = "data/raw/ZipHsaHrr.csv",
  census_region_path = "data/raw/census_division_crosswalk.csv",
  out_csv = "outputs/tables/census_region/active_hospitals_by_census_region.csv",
  year_min = 2010L,
  year_max = 2023L
) {
  if (!file.exists(pos_path)) stop("POS panel not found: ", pos_path)
  if (!file.exists(zip_hsa_path)) stop("ZIP-HSA crosswalk not found: ", zip_hsa_path)
  if (!file.exists(census_region_path)) stop("Census region crosswalk not found: ", census_region_path)

  pos <- read_csv(pos_path, show_col_types = FALSE) %>%
    transmute(
      ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"),
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      state_abbrev = str_to_upper(str_trim(as.character(state_abbrev))),
      year = as.integer(year),
      active = as.integer(active)
    ) %>%
    filter(active == 1, year >= year_min, year <= year_max)

  # Mirrors 13_event_counts_by_census_region.R: first crosswalk row per ZIP.
  zip_hsa <- read_csv(zip_hsa_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsastate = str_to_upper(str_trim(as.character(hsastate)))
    ) %>%
    distinct(zip5, .keep_all = TRUE)

  census_regions <- read_csv(census_region_path, show_col_types = FALSE, name_repair = "unique_quiet") %>%
    select(State, Region) %>%
    mutate(
      State = str_to_upper(str_trim(as.character(State))),
      Region = str_trim(as.character(Region))
    ) %>%
    filter(!is.na(State), State != "") %>%
    distinct(State, .keep_all = TRUE)

  region_levels <- c("Northeast", "Midwest", "South", "West", "Unmatched")
  n_unique_ccns <- n_distinct(pos$ccn)

  summarise_method <- function(ccn_region, method_label) {
    ccn_region <- ccn_region %>%
      mutate(Region = coalesce(Region, "Unmatched")) %>%
      distinct(ccn, Region)

    n_multi <- ccn_region %>%
      count(ccn) %>%
      filter(n > 1) %>%
      nrow()

    region_rows <- ccn_region %>%
      count(Region, name = "n_hospitals") %>%
      complete(Region = region_levels, fill = list(n_hospitals = 0L)) %>%
      mutate(Region = factor(Region, levels = region_levels)) %>%
      arrange(Region) %>%
      mutate(Region = as.character(Region))

    bind_rows(
      region_rows,
      tibble(Region = "Total unique CCNs", n_hospitals = n_unique_ccns)
    ) %>%
      mutate(
        assignment_method = method_label,
        n_ccns_in_multiple_regions = if_else(
          Region == "Total unique CCNs", n_multi, NA_integer_
        )
      )
  }

  by_state <- pos %>%
    left_join(census_regions, by = c("state_abbrev" = "State")) %>%
    summarise_method("hospital_state")

  by_zip <- pos %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(census_regions, by = c("hsastate" = "State")) %>%
    summarise_method("zip_hsastate")

  out <- bind_rows(by_state, by_zip) %>%
    mutate(year_min = year_min, year_max = year_max) %>%
    select(assignment_method, Region, n_hospitals,
           n_ccns_in_multiple_regions, year_min, year_max)

  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  write_csv(out, out_csv)

  message("Unique active hospitals ", year_min, "-", year_max, " by Census region:")
  print(out, n = Inf)
  message("Written to: ", out_csv)

  invisible(out)
}
