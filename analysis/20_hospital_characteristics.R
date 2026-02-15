# Create hospital characteristics summary table comparing openings vs closures.
# Usage: source("R/analysis/20_hospital_characteristics.R"); run_hospital_characteristics()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(kableExtra)
  library(rlang)
})

#' Build hospital characteristics table
#'
#' @param openings_path Clean openings CSV (default "data/interim/openings_clean.csv")
#' @param closures_path Clean closures CSV (default "data/interim/closures_clean.csv")
#' @param pos_path Cleaned POS panel CSV (default "data/processed/pos_panel_updated.csv")
#' @param out_tex Output TeX path (default "outputs/tables/hospital_characteristics.tex")
#' @return final summary dataframe (invisible)
run_hospital_characteristics <- function(
  openings_path = "data/interim/openings_clean.csv",
  closures_path = "data/interim/closures_clean.csv",
  pos_path = "data/processed/pos_panel_updated.csv",
  out_tex = "outputs/tables/hospital_characteristics.tex"
) {
  if (!file.exists(openings_path)) stop("Openings file not found: ", openings_path)
  if (!file.exists(closures_path)) stop("Closures file not found: ", closures_path)
  if (!file.exists(pos_path)) stop("POS panel not found: ", pos_path)

  openings <- read_csv(openings_path, show_col_types = FALSE)
  closures <- read_csv(closures_path, show_col_types = FALSE)
  pos_panel_updated <- read_csv(pos_path, show_col_types = FALSE)

  openings <- openings %>%
    select(opening, ccn, part_year)

  closures <- closures %>%
    select(closure, ccn, term_year)

  closed_short_lived <- pos_panel_updated %>%
    filter(!is.na(term_year)) %>%
    group_by(ccn) %>%
    summarize(
      start_yr = min(part_year, na.rm = TRUE),
      end_yr = max(term_year, na.rm = TRUE),
      duration = end_yr - start_yr,
      partdate = min(partdate),
      termdate = min(termdate),
      .groups = "drop"
    ) %>%
    filter(duration <= 1)

  pos_panel_updated <- merge(pos_panel_updated, openings, by.x = c("ccn", "year"), by.y = c("ccn", "part_year"), all.x = TRUE)
  pos_panel_updated <- merge(pos_panel_updated, closures, by.x = c("ccn", "year"), by.y = c("ccn", "term_year"), all.x = TRUE)

  panel_wo_events <- subset(
    pos_panel_updated,
    (opening != 1 | is.na(opening)) & (closure != 1 | is.na(closure) & beds > 1)
  )

  panel_openings <- subset(pos_panel_updated, opening == 1)
  panel_closures <- subset(pos_panel_updated, closure == 1)

  calculate_yearly_event_weights <- function(event_df) {
    total_overall_ccns <- event_df %>%
      summarise(overall_distinct_ccns = n_distinct(ccn)) %>%
      pull(overall_distinct_ccns)

    event_df %>%
      group_by(year) %>%
      summarise(distinct_ccns_in_year = n_distinct(ccn), .groups = "drop") %>%
      mutate(weight = distinct_ccns_in_year / total_overall_ccns) %>%
      select(year, weight)
  }

  analyze_binary_variable <- function(var_name_string, var_col_sym, binary_value, panel_df, event_df, yearly_event_weights_df, event_type_string) {
    national_yearly_stats <- panel_df %>%
      filter(active == 1) %>%
      filter(!is.na(!!var_col_sym)) %>%
      group_by(year) %>%
      summarise(
        yearly_pct = (sum(!!var_col_sym %in% binary_value, na.rm = TRUE) / n()) * 100,
        n_in_year = n(),
        .groups = "drop"
      )

    total_national_obs <- sum(national_yearly_stats$n_in_year)

    national_average_value <- national_yearly_stats %>%
      left_join(yearly_event_weights_df, by = "year") %>%
      mutate(weight = coalesce(weight, 0)) %>%
      summarise(weighted_avg = sum(yearly_pct * weight)) %>%
      pull(weighted_avg)

    overall_event_calc <- event_df %>%
      filter(!is.na(!!var_col_sym)) %>%
      summarise(
        overall_average = mean(!!var_col_sym %in% binary_value, na.rm = TRUE) * 100,
        total_distinct_event_ccns = n_distinct(ccn),
        .groups = "drop"
      )

    national_avg_col_name <- paste0(event_type_string, "_National_Average")
    overall_value_col_name <- paste0(event_type_string, "_Overall_Value")
    hospitals_nat_count_col_name <- paste0(event_type_string, "_Hospitals_National_Count")
    hospitals_event_count_col_name <- paste0(event_type_string, "_Hospitals_Event_Count")

    result_df <- data.frame(Variable = var_name_string)
    result_df[[national_avg_col_name]] <- national_average_value
    result_df[[overall_value_col_name]] <- overall_event_calc$overall_average
    result_df[[hospitals_nat_count_col_name]] <- total_national_obs
    result_df[[hospitals_event_count_col_name]] <- overall_event_calc$total_distinct_event_ccns
    result_df
  }

  analyze_continuous_variable <- function(var_name_string, var_col_sym, panel_df, event_df, yearly_event_weights_df, event_type_string) {
    national_yearly_stats <- panel_df %>%
      filter(active == 1) %>%
      filter(!is.na(!!var_col_sym)) %>%
      group_by(year) %>%
      summarise(
        yearly_mean = mean(!!var_col_sym, na.rm = TRUE),
        n_in_year = n(),
        .groups = "drop"
      )

    total_national_obs <- sum(national_yearly_stats$n_in_year)

    national_average_value <- national_yearly_stats %>%
      left_join(yearly_event_weights_df, by = "year") %>%
      mutate(weight = coalesce(weight, 0)) %>%
      summarise(weighted_avg = sum(yearly_mean * weight)) %>%
      pull(weighted_avg)

    overall_event_calc <- event_df %>%
      filter(!is.na(!!var_col_sym)) %>%
      summarise(
        overall_average = mean(!!var_col_sym, na.rm = TRUE),
        total_distinct_event_ccns = n_distinct(ccn),
        .groups = "drop"
      )

    national_avg_col_name <- paste0(event_type_string, "_National_Average")
    overall_value_col_name <- paste0(event_type_string, "_Overall_Value")
    hospitals_nat_count_col_name <- paste0(event_type_string, "_Hospitals_National_Count")
    hospitals_event_count_col_name <- paste0(event_type_string, "_Hospitals_Event_Count")

    result_df <- data.frame(Variable = var_name_string)
    result_df[[national_avg_col_name]] <- national_average_value
    result_df[[overall_value_col_name]] <- overall_event_calc$overall_average
    result_df[[hospitals_nat_count_col_name]] <- total_national_obs
    result_df[[hospitals_event_count_col_name]] <- overall_event_calc$total_distinct_event_ccns
    result_df
  }

  yearly_openings_weights <- calculate_yearly_event_weights(panel_openings)
  yearly_closures_weights <- calculate_yearly_event_weights(panel_closures)

  all_variable_results <- list()

  all_variable_results[["for_profit"]] <- left_join(
    analyze_binary_variable("Percent For-Profit", quo(control), c(4, 9), panel_wo_events, panel_openings, yearly_openings_weights, "Openings"),
    analyze_binary_variable("Percent For-Profit", quo(control), c(4, 9), panel_wo_events, panel_closures, yearly_closures_weights, "Closures"),
    by = "Variable"
  )

  all_variable_results[["non_profit"]] <- left_join(
    analyze_binary_variable("Percent Non-Profit", quo(control), c(1, 2), panel_wo_events, panel_openings, yearly_openings_weights, "Openings"),
    analyze_binary_variable("Percent Non-Profit", quo(control), c(1, 2), panel_wo_events, panel_closures, yearly_closures_weights, "Closures"),
    by = "Variable"
  )

  all_variable_results[["public"]] <- left_join(
    analyze_binary_variable("Percent Public", quo(control), c(5, 6, 7, 8), panel_wo_events, panel_openings, yearly_openings_weights, "Openings"),
    analyze_binary_variable("Percent Public", quo(control), c(5, 6, 7, 8), panel_wo_events, panel_closures, yearly_closures_weights, "Closures"),
    by = "Variable"
  )

  all_variable_results[["teach_major"]] <- left_join(
    analyze_binary_variable("Percent Teaching Major", quo(teach_major), 1, panel_wo_events, panel_openings, yearly_openings_weights, "Openings"),
    analyze_binary_variable("Percent Teaching Major", quo(teach_major), 1, panel_wo_events, panel_closures, yearly_closures_weights, "Closures"),
    by = "Variable"
  )

  all_variable_results[["teach_majgrad"]] <- left_join(
    analyze_binary_variable("Percent Teaching Major Graduate", quo(teach_majgrad), 1, panel_wo_events, panel_openings, yearly_openings_weights, "Openings"),
    analyze_binary_variable("Percent Teaching Major Graduate", quo(teach_majgrad), 1, panel_wo_events, panel_closures, yearly_closures_weights, "Closures"),
    by = "Variable"
  )

  all_variable_results[["certbeds"]] <- left_join(
    analyze_continuous_variable("Average Certified Beds", quo(certbeds), panel_wo_events, panel_openings, yearly_openings_weights, "Openings"),
    analyze_continuous_variable("Average Certified Beds", quo(certbeds), panel_wo_events, panel_closures, yearly_closures_weights, "Closures"),
    by = "Variable"
  )

  final_summary_df <- bind_rows(all_variable_results)

  latex_table <- final_summary_df %>%
    rename(
      "Variable" = Variable,
      "Natl. Avg" = Openings_National_Average,
      "Event Avg" = Openings_Overall_Value,
      "N (Natl)" = Openings_Hospitals_National_Count,
      "N (Event)" = Openings_Hospitals_Event_Count,
      "Natl. Avg " = Closures_National_Average,
      "Event Avg " = Closures_Overall_Value,
      "N (Natl) " = Closures_Hospitals_National_Count,
      "N (Event) " = Closures_Hospitals_Event_Count
    ) %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      caption = "Summary of Hospital Characteristics by Event Type",
      digits = 2,
      align = "lcccccccc",
      col.names = c("Variable", "Natl. Avg", "Event Avg", "N (Natl)", "N (Event)",
                    "Natl. Avg", "Event Avg", "N (Natl)", "N (Event)")
    ) %>%
    add_header_above(c(" " = 1, "Hospital Openings" = 4, "Hospital Closures" = 4)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))

  out_dir <- dirname(out_tex)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(latex_table, out_tex)

  invisible(final_summary_df)
}

