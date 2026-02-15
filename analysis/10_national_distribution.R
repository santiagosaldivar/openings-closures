# Create national distribution percentile tables for selected years.
# Usage: source("R/analysis/10_national_distribution.R"); create_national_distribution_tables()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(gt)
})

#' Build percentile tables for national HSA demographics
#'
#' @param input_csv Path to ntl_hsa_percentiles.csv (default "data/interim/ntl_hsa_percentiles.csv")
#' @param years Vector of years to report (default c(2012, 2015, 2018, 2022))
#' @param out_dir Directory to save output tables (default "outputs/tables")
#' @return list of gt tables (invisible)
create_national_distribution_tables <- function(
  input_csv = "data/interim/ntl_hsa_percentiles.csv",
  years = c(2012, 2015, 2018, 2022),
  out_dir = "outputs/tables"
) {
  if (!file.exists(input_csv)) stop("Input CSV not found: ", input_csv)

  ntl_hsa_percentiles <- read_csv(input_csv, show_col_types = FALSE)

  national_distribution <- ntl_hsa_percentiles %>%
    rename(
      income = weighted_median_household_income_event,
      insurance_any = weighted_percent_any_health_insur_event,
      insurance_public = weighted_percent_public_health_insur_event,
      unemployment = weighted_unemployment_rate_event,
      bachelors_degree = weighted_percent_bachelors_event,
      black = weighted_percent_black_event,
      hispanic_latino = weighted_percent_hispanic_or_latino_event,
      poverty = weighted_percent_below_poverty_line_event,
      SDI = weighted_SDI_score_event
    )

  demographic_vars <- c(
    "income", "insurance_any", "insurance_public", "unemployment",
    "bachelors_degree", "black", "hispanic_latino", "poverty", "SDI"
  )
  percentile_probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  percentile_names <- c("5th", "25th", "50th", "75th", "95th")

  create_percentile_table <- function(target_year, data) {
    data %>%
      filter(year == target_year) %>%
      reframe(across(all_of(demographic_vars),
                     ~ quantile(., probs = percentile_probs, na.rm = TRUE))) %>%
      mutate(percentile = percentile_names) %>%
      pivot_longer(
        cols = -percentile,
        names_to = "variable",
        values_to = "value"
      ) %>%
      pivot_wider(names_from = percentile, values_from = value) %>%
      select(variable, `5th`, `25th`, `50th`, `75th`, `95th`)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  tables <- list()
  for (yr in years) {
    pct_df <- create_percentile_table(yr, national_distribution)
    tab <- pct_df %>%
      gt() %>%
      tab_header(title = paste0("National Distribution of HSA Demographics for ", yr)) %>%
      fmt_number(columns = everything(), rows = everything(), decimals = 2)

    png_file <- file.path(out_dir, paste0("demographics_table_", yr, ".png"))
    save_ok <- TRUE
    tryCatch(
      gtsave(tab, file = png_file),
      error = function(e) {
        save_ok <<- FALSE
        message(
          "PNG export failed for ", yr, " (", conditionMessage(e), "). ",
          "Falling back to LaTeX."
        )
      }
    )
    if (!save_ok) {
      tex_file <- file.path(out_dir, paste0("demographics_table_", yr, ".tex"))
      gtsave(tab, file = tex_file)
    }
    tables[[as.character(yr)]] <- tab
  }

  invisible(tables)
}
