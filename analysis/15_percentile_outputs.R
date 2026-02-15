# Generate percentile-based plots and tables for openings/closures (plus
# non-event comparisons where needed in testing/table outputs).
# Functions assume the staged file `data/interim/opening_closure_nonevent_percentiles.csv`.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(broom)
  library(gt)
  library(patchwork)
})

#' Create percentile histograms, violins, and a 3-panel violin figure.
#'
#' @param input_csv path to percentile dataset
#' @param out_fig_dir directory for figure outputs
run_percentile_plots <- function(
  input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
  out_fig_dir = "outputs/figures/percentiles"
) {
  df_all <- read_csv(input_csv, show_col_types = FALSE) %>%
    mutate(group = factor(group, levels = c("Opening", "Closure", "non-event")))
  df_openings_closures <- df_all %>%
    filter(group %in% c("Opening", "Closure")) %>%
    mutate(group = factor(as.character(group), levels = c("Opening", "Closure")))

  percentile_vars <- c(
    "income_percentile", "health_insurance_percentile", "public_health_insurance_percentile",
    "unemployment_rate_percentile", "bachelors_percentile", "black_percentile",
    "latino_percentile", "poverty_percentile", "SDI_percentile",
    "population_density_percentile", "pop_change_pct_percentile"
  )

  labels <- list(
    income_percentile = c("Lower Income", "Higher Income"),
    health_insurance_percentile = c("Fewer Insured", "More Insured"),
    public_health_insurance_percentile = c("Less Public Ins.", "More Public Ins."),
    unemployment_rate_percentile = c("Lower Unempl.", "Higher Unempl."),
    bachelors_percentile = c("Fewer Grads", "More Grads"),
    black_percentile = c("Smaller Prop.", "Larger Prop."),
    latino_percentile = c("Smaller Prop.", "Larger Prop."),
    poverty_percentile = c("Lower Poverty", "Higher Poverty"),
    SDI_percentile = c("Less Disadvantage", "More Disadvantage"),
    population_density_percentile = c("Lower density", "Higher density"),
    pop_change_pct_percentile = c("Lower pop. shift", "Higher pop. shift")
  )

  dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)

  plot_percentile_histogram <- function(df, var, summary_n, output_path, below_label, above_label, bins = 20) {
    legend_labels <- setNames(summary_n$label, summary_n$group)
    x_axis_label <- sprintf("%s   |   %s (within-year percentile)", below_label, above_label)

    p <- ggplot(df, aes(x = .data[[var]], color = group, linetype = group)) +
      geom_step(aes(y = after_stat(density)), stat = "bin", position = "identity", bins = bins, linewidth = 0.8) +
      scale_color_manual(values = c("Closure" = "#D73027", "Opening" = "#1BC9C9", "non-event" = "gray50"), labels = legend_labels) +
      scale_linetype_manual(values = c("Closure" = "solid", "Opening" = "solid", "non-event" = "dashed"), labels = legend_labels) +
      labs(x = x_axis_label, y = "Density", color = "", linetype = "") +
      theme_minimal(base_size = 12)

    ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
  }

  plot_filled_violin <- function(df, var, summary_n, output_path) {
    legend_labels <- setNames(summary_n$label, summary_n$group)
    p <- ggplot(df, aes(x = group, y = .data[[var]], fill = group, color = group)) +
      geom_violin(alpha = 0.5, linetype = "dashed") +
      scale_fill_manual(values = c("Closure" = "#D73027", "Opening" = "#1BC9C9", "non-event" = "gray70"), labels = legend_labels) +
      scale_color_manual(values = c("Closure" = "#D73027", "Opening" = "#1BC9C9", "non-event" = "gray50"), guide = "none") +
      labs(x = "Group", y = "Within-Year Percentile", fill = "") +
      theme_minimal(base_size = 12)
    if (!is.null(output_path)) {
      ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
    }
    p
  }

  # Individual histograms and violins
  walk(percentile_vars, function(var_name) {
    cfg <- labels[[var_name]]
    summary_n <- df_openings_closures %>%
      group_by(group) %>%
      summarise(n = sum(!is.na(.data[[var_name]])), .groups = "drop") %>%
      mutate(label = paste0(group, " (n = ", n, ")"))

    plot_percentile_histogram(
      df = df_openings_closures,
      var = var_name,
      summary_n = summary_n,
      output_path = file.path(out_fig_dir, paste0(var_name, "_hist.png")),
      below_label = cfg[1],
      above_label = cfg[2]
    )

    plot_filled_violin(
      df = df_openings_closures,
      var = var_name,
      summary_n = summary_n,
      output_path = file.path(out_fig_dir, paste0(var_name, "_violin.png"))
    )
  })

  # Three-panel violin
  vars_to_plot <- c("public_health_insurance_percentile", "pop_change_pct_percentile", "SDI_percentile")
  title_lookup <- c(
    "public_health_insurance_percentile" = "Public Health Insurance",
    "pop_change_pct_percentile" = "Population Change",
    "SDI_percentile" = "Social Deprivation Index (SDI)"
  )

  plot_list <- list()
  for (i in seq_along(vars_to_plot)) {
    var_name <- vars_to_plot[i]
    summary_n <- df_openings_closures %>%
      group_by(group) %>%
      summarise(n = sum(!is.na(.data[[var_name]])), .groups = "drop") %>%
      mutate(label = paste0(group, " (n = ", n, ")"))
    p <- plot_filled_violin(df_openings_closures, var_name, summary_n, output_path = NULL) +
      ggtitle(title_lookup[[var_name]]) +
      coord_cartesian(ylim = c(0, 100))
    if (i > 1) {
      p <- p + theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    }
    plot_list[[i]] <- p
  }

  combined_plot <- plot_list[[1]] + plot_list[[2]] + plot_list[[3]] +
    plot_layout(guides = "collect", ncol = 3) &
    theme(legend.position = "bottom")

  ggsave(file.path(out_fig_dir, "combined_3_panel_violin.png"), combined_plot, width = 20, height = 8, dpi = 300)

  invisible(NULL)
}

#' Build Kruskal-Wallis summary table (final_table)
#'
#' @param input_csv path to percentile dataset
#' @param out_table_dir directory to save table
run_kw_final_table <- function(
  input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
  out_table_dir = "outputs/tables"
) {
  df_all <- read_csv(input_csv, show_col_types = FALSE) %>%
    mutate(group = factor(group, levels = c("Opening", "Closure", "non-event")))

  # Subsets
  df_openings_closures <- df_all %>% filter(group %in% c("Opening", "Closure"))
  df_openings_none <- df_all %>% filter(group %in% c("Opening", "non-event"))
  df_closures_none <- df_all %>% filter(group %in% c("Closure", "non-event"))

  percentile_cols <- c(
    "income_percentile", "health_insurance_percentile", "public_health_insurance_percentile",
    "unemployment_rate_percentile", "bachelors_percentile", "black_percentile",
    "latino_percentile", "poverty_percentile", "SDI_percentile",
    "population_density_percentile", "pop_change_pct_percentile"
  )

  pretty_names <- c(
    income_percentile = "Median household income",
    health_insurance_percentile = "Any health insurance (%)",
    public_health_insurance_percentile = "Public health insurance (%)",
    unemployment_rate_percentile = "Unemployment rate (%)",
    bachelors_percentile = "Bachelor's degree (%)",
    black_percentile = "Black (%)",
    latino_percentile = "Hispanic/Latino (%)",
    poverty_percentile = "Below poverty line (%)",
    SDI_percentile = "Social deprivation index",
    population_density_percentile = "Population density",
    pop_change_pct_percentile = "Population change (%)"
  )

  # summary stats for openings vs closures
  summary_stats_df <- df_openings_closures %>%
    select(group, all_of(percentile_cols)) %>%
    pivot_longer(-group, names_to = "Measure", values_to = "Value") %>%
    group_by(Measure, group) %>%
    summarise(N = sum(!is.na(Value)), Mean = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Measure = pretty_names[Measure]) %>%
    pivot_wider(names_from = group, values_from = c(N, Mean))

  run_kw <- function(var) {
    list(
      test_2 = kruskal.test(x = df_openings_closures[[var]], g = df_openings_closures$group),
      test_3 = kruskal.test(x = df_openings_none[[var]], g = df_openings_none$group),
      test_4 = kruskal.test(x = df_closures_none[[var]], g = df_closures_none$group)
    )
  }

  test_list <- lapply(percentile_cols, run_kw)
  names(test_list) <- pretty_names[percentile_cols]

  results_df <- map_dfr(test_list, ~ map_dfr(.x, tidy, .id = "Test"), .id = "Demographic Variable") %>%
    mutate(p.value_formatted = if_else(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))

  results_df_wide <- results_df %>%
    pivot_wider(id_cols = `Demographic Variable`, names_from = Test, values_from = c(statistic, p.value, p.value_formatted))

  merged <- summary_stats_df %>%
    rename(`Demographic Variable` = Measure) %>%
    left_join(results_df_wide, by = "Demographic Variable")

  formatted <- merged %>%
    mutate(
      test_2 = case_when(
        p.value_test_2 < 0.01 ~ paste0(sprintf("%.2f", statistic_test_2), "**"),
        p.value_test_2 < 0.05 ~ paste0(sprintf("%.2f", statistic_test_2), "*"),
        TRUE ~ sprintf("%.2f", statistic_test_2)
      ),
      test_3 = case_when(
        p.value_test_3 < 0.01 ~ paste0(sprintf("%.2f", statistic_test_3), "**"),
        p.value_test_3 < 0.05 ~ paste0(sprintf("%.2f", statistic_test_3), "*"),
        TRUE ~ sprintf("%.2f", statistic_test_3)
      ),
      test_4 = case_when(
        p.value_test_4 < 0.01 ~ paste0(sprintf("%.2f", statistic_test_4), "**"),
        p.value_test_4 < 0.05 ~ paste0(sprintf("%.2f", statistic_test_4), "*"),
        TRUE ~ sprintf("%.2f", statistic_test_4)
      )
    ) %>%
    select(
      `Demographic Variable`,
      N_Closure, Mean_Closure,
      N_Opening, Mean_Opening,
      test_4, test_3, test_2
    )

  gt_table <- formatted %>%
    gt() %>%
    tab_header(title = "Sample Descriptives Using Kruskal-Wallis Test") %>%
    tab_spanner(label = "Closure", columns = c(N_Closure, Mean_Closure)) %>%
    tab_spanner(label = "Opening", columns = c(N_Opening, Mean_Opening)) %>%
    tab_spanner(label = "Test 2", columns = test_2) %>%
    tab_spanner(label = "Test 3", columns = test_3) %>%
    tab_spanner(label = "Test 4", columns = test_4) %>%
    cols_label(
      `Demographic Variable` = "",
      N_Closure = "N", Mean_Closure = "Mean",
      N_Opening = "N", Mean_Opening = "Mean",
      test_2 = "Test Statistic", test_3 = "Test Statistic", test_4 = "Test Statistic"
    ) %>%
    fmt_number(columns = c(Mean_Closure, Mean_Opening), decimals = 2) %>%
    fmt_number(columns = c(N_Closure, N_Opening), decimals = 0) %>%
    tab_footnote(
      footnote = "**p < .01; *p < .05.",
      locations = cells_column_labels(columns = c(test_2, test_3, test_4))
    ) %>%
    cols_align(align = "center", columns = -`Demographic Variable`)

  dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
  gtsave(gt_table, file.path(out_table_dir, "brief_summary_table.tex"))
  invisible(gt_table)
}

#' Appendix tables: 2016 quantiles and longitudinal medians
run_appendix_tables <- function(
  input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
  out_table_dir = "outputs/tables"
) {
  df_all <- read_csv(input_csv, show_col_types = FALSE)

  mapping <- tibble::tibble(
    Demographic = c(
      "weighted_percent_bachelors_event",
      "weighted_median_household_income_event",
      "weighted_percent_below_poverty_line_event",
      "weighted_unemployment_rate_event",
      "weighted_SDI_score_event",
      "population_density",
      "pop_change_pct",
      "weighted_percent_any_health_insur_event",
      "weighted_percent_public_health_insur_event"
    ),
    PrettyName = c(
      "Bachelor's degree (%)",
      "Median household income",
      "Below poverty line (%)",
      "Unemployment rate (%)",
      "Social deprivation index",
      "Population density",
      "Population change (%)",
      "Any health insurance (%)",
      "Public health insurance (%)"
    ),
    Category = c(
      rep("Socioeconomic Status", 5),
      rep("Demographics", 2),
      rep("Insurance Coverage", 2)
    )
  )

  # 2016 quantile table
  target_vars <- mapping$Demographic
  quantile_table <- df_all %>%
    filter(year == 2016) %>%
    select(all_of(target_vars)) %>%
    pivot_longer(cols = everything(), names_to = "Demographic", values_to = "Value") %>%
    filter(!(Demographic %in% c("weighted_median_household_income_event", "population_density") & Value == 0)) %>%
    group_by(Demographic) %>%
    summarise(
      N_Obs = n(),
      Min = min(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Median = median(Value, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(mapping, by = "Demographic") %>%
    select(Category, PrettyName, Min, Q1, Median, Q3, Max) %>%
    arrange(factor(Category, levels = c("Socioeconomic Status", "Demographics", "Insurance Coverage")))

  gt_quantile <- quantile_table %>%
    gt(groupname_col = "Category") %>%
    fmt_number(columns = where(is.numeric), decimals = 2)

  # Longitudinal medians
  longitudinal_medians <- df_all %>%
    filter(year >= 2010 & year <= 2023) %>%
    select(year, all_of(target_vars)) %>%
    pivot_longer(cols = -year, names_to = "Demographic", values_to = "Value") %>%
    filter(!is.na(Value)) %>%
    filter(!(Demographic %in% c("weighted_median_household_income_event", "population_density") & Value <= 0)) %>%
    group_by(Demographic, year) %>%
    summarise(MedianValue = median(Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = MedianValue)

  final_time_table <- mapping %>%
    left_join(longitudinal_medians, by = "Demographic") %>%
    select(-Demographic) %>%
    arrange(factor(Category, levels = c("Socioeconomic Status", "Demographics", "Insurance Coverage"))) %>%
    relocate(Category, PrettyName, sort(names(.)[!(names(.) %in% c("Category", "PrettyName"))]))

  gt_time <- final_time_table %>%
    gt(groupname_col = "Category") %>%
    fmt_number(columns = where(is.numeric), decimals = 2)

  dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
  gtsave(gt_quantile, file.path(out_table_dir, "appendix_quantiles_2016.tex"))
  gtsave(gt_time, file.path(out_table_dir, "appendix_medians_over_time.tex"))
  invisible(list(quantiles_2016 = gt_quantile, medians_over_time = gt_time))
}
