# Generate percentile-based plots and tables for openings/closures (plus
# non-event comparisons where needed in testing/table outputs).
# Functions assume the staged file `data/interim/opening_closure_nonevent_percentiles.csv`.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
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
  out_fig_dir = "outputs/figures/percentiles",
  openings_file = "data/raw/updated_openings_august2025.csv",
  closures_file = "data/raw/updated_closures_august2025.csv",
  crosswalk_file = "data/raw/ZipHsaHrr.csv",
  ruca_file = "data/raw/RUCA2010zipcode.xlsx"
) {
  mode_value <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    tab <- sort(table(x), decreasing = TRUE)
    names(tab)[1]
  }

  df_all <- read_csv(input_csv, show_col_types = FALSE) %>%
    mutate(group = factor(group, levels = c("Opening", "Closure", "non-event")))
  df_openings_closures <- df_all %>%
    filter(group %in% c("Opening", "Closure")) %>%
    mutate(group = factor(as.character(group), levels = c("Opening", "Closure")))

  percentile_vars <- c(
    "income_percentile", "health_insurance_percentile", "public_health_insurance_percentile",
    "unemployment_rate_percentile", "bachelors_percentile", "black_percentile",
    "latino_percentile", "poverty_percentile", "SDI_percentile",
    "certbeds_per_1000_residents_percentile", "population_density_percentile", "pop_change_pct_percentile"
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
    certbeds_per_1000_residents_percentile = c("Fewer beds per capita", "More beds per capita"),
    population_density_percentile = c("Lower density", "Higher density"),
    pop_change_pct_percentile = c("Lower pop. shift", "Higher pop. shift")
  )

  six_panel_vars <- c(
    "bachelors_percentile",
    "income_percentile",
    "poverty_percentile",
    "unemployment_rate_percentile",
    "SDI_percentile",
    "health_insurance_percentile"
  )
  six_panel_titles <- c(
    "bachelors_percentile" = "Bachelor's Degree",
    "income_percentile" = "Median Household Income",
    "poverty_percentile" = "Below Poverty Line",
    "unemployment_rate_percentile" = "Unemployment Rate",
    "SDI_percentile" = "Social Deprivation Index",
    "health_insurance_percentile" = "Any Health Insurance"
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

  openings_events <- read_csv(openings_file, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(part_year),
      group = "Opening"
    ) %>%
    filter(year >= 2010, !is.na(year))

  closures_events <- read_csv(closures_file, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(term_year),
      group = "Closure"
    ) %>%
    filter(year >= 2010, !is.na(year))

  zip_hsa <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()

  ruca <- readxl::read_excel(ruca_file, sheet = "Data") %>%
    transmute(
      zip5 = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      ruca_simple = case_when(
        RUCA1 %in% c(1, 2, 3) ~ "Metropolitan",
        RUCA1 %in% c(4, 5, 6) ~ "Micropolitan",
        RUCA1 %in% c(7, 8, 9) ~ "Small Town",
        RUCA1 == 10 ~ "Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      ruca_grouped = case_when(
        ruca_simple %in% c("Rural", "Small Town") ~ "Rural & Small Town",
        TRUE ~ ruca_simple
      )
    ) %>%
    distinct(zip5, ruca_grouped)

  event_class <- bind_rows(openings_events, closures_events) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(ruca, by = "zip5") %>%
    group_by(hsanum, year, group) %>%
    summarise(ruca_grouped = mode_value(ruca_grouped), .groups = "drop")

  df_openings_closures <- df_openings_closures %>%
    left_join(event_class, by = c("hsanum", "year", "group"))

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

  build_six_panel_violin <- function(df_panel, panel_title, output_name) {
    panel_plots <- map2(
      six_panel_vars,
      seq_along(six_panel_vars),
      function(var_name, idx) {
        summary_n <- df_panel %>%
          group_by(group) %>%
          summarise(n = sum(!is.na(.data[[var_name]])), .groups = "drop") %>%
          mutate(label = paste0(group, " (n = ", n, ")"))

        p <- plot_filled_violin(df_panel, var_name, summary_n, output_path = NULL) +
          ggtitle(six_panel_titles[[var_name]]) +
          coord_cartesian(ylim = c(0, 100)) +
          theme(
            text = element_text(family = "Times New Roman"),
            plot.title = element_text(face = "bold", size = 12),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            panel.grid.minor = element_blank()
          )

        if (idx %% 2 == 0) {
          p <- p + theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
        }
        p
      }
    )

    combined_panel <- wrap_plots(panel_plots, ncol = 2, guides = "collect") +
      plot_annotation() &
      theme(
        text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )

    ggsave(
      file.path(out_fig_dir, output_name),
      combined_panel,
      width = 15,
      height = 14,
      dpi = 300
    )

    note_text <- paste(
      "Violin widths represent the relative density of observed within-year percentiles",
      "among event HSAs in the plotted subgroup. The distributions summarize the observed",
      "support for openings and closures only; they do not imply that values span the full",
      "0 to 100 percentile range."
    )

    tex_lines <- c(
      "\\documentclass[varwidth=16in, border=10pt]{standalone}",
      "\\usepackage{graphicx}",
      "\\usepackage{caption}",
      "\\usepackage{setspace}",
      "",
      "\\begin{document}",
      "\\begin{minipage}{0.98\\textwidth}",
      "  \\centering",
      sprintf("  \\includegraphics[width=\\textwidth]{%s}", output_name),
      sprintf("  \\captionof{figure}{%s}", panel_title),
      sprintf("  \\caption*{\\footnotesize \\textit{Note:} %s}", note_text),
      "\\end{minipage}",
      "\\end{document}"
    )
    writeLines(
      tex_lines,
      file.path(out_fig_dir, str_replace(output_name, "\\.png$", "_standalone.tex"))
    )
  }

  build_six_panel_violin(
    df_openings_closures %>% filter(ruca_grouped == "Metropolitan"),
    "Urban Openings and Closures: Socioeconomic Status and Insurance Coverage",
    "urban_openings_closures_six_panel_violin.png"
  )
  build_six_panel_violin(
    df_openings_closures %>% filter(ruca_grouped == "Rural & Small Town"),
    "Rural and Small Town Openings and Closures: Socioeconomic Status and Insurance Coverage",
    "rural_small_town_openings_closures_six_panel_violin.png"
  )

  invisible(NULL)
}

#' Build Kruskal-Wallis summary table (final_table)
#'
#' @param input_csv path to percentile dataset
#' @param out_table_dir directory to save table
run_kw_final_table <- function(
  input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
  out_table_dir = "outputs/tables",
  openings_file = "data/raw/updated_openings_august2025.csv",
  closures_file = "data/raw/updated_closures_august2025.csv",
  crosswalk_file = "data/raw/ZipHsaHrr.csv",
  ruca_file = "data/raw/RUCA2010zipcode.xlsx"
) {
  df_all <- read_csv(input_csv, show_col_types = FALSE) %>%
    mutate(group = factor(group, levels = c("Opening", "Closure", "non-event")))

  percentile_cols <- c(
    "income_percentile", "health_insurance_percentile", "public_health_insurance_percentile",
    "unemployment_rate_percentile", "bachelors_percentile", "poverty_percentile", "SDI_percentile",
    "certbeds_per_1000_residents_percentile", "population_density_percentile", "pop_change_pct_percentile"
  )

  pretty_names <- c(
    income_percentile = "Median household income",
    health_insurance_percentile = "Any health insurance (%)",
    public_health_insurance_percentile = "Public health insurance (%)",
    unemployment_rate_percentile = "Unemployment rate (%)",
    bachelors_percentile = "Bachelor's degree (%)",
    poverty_percentile = "Below poverty line (%)",
    SDI_percentile = "Social deprivation index",
    certbeds_per_1000_residents_percentile = "Certified beds per 1,000 residents",
    population_density_percentile = "Population density",
    pop_change_pct_percentile = "Population change (%)"
  )

  row_spec <- tibble::tribble(
    ~Category, ~`Demographic Variable`, ~row_order,
    "Socioeconomic Status", "Bachelor's degree (%)", 1L,
    "Socioeconomic Status", "Median household income", 2L,
    "Socioeconomic Status", "Below poverty line (%)", 3L,
    "Socioeconomic Status", "Unemployment rate (%)", 4L,
    "Socioeconomic Status", "Social deprivation index", 5L,
    "Demographics", "Certified beds per 1,000 residents", 6L,
    "Demographics", "Population density", 7L,
    "Demographics", "Population change (%)", 8L,
    "Insurance Coverage", "Any health insurance (%)", 9L,
    "Insurance Coverage", "Public health insurance (%)", 10L
  )

  mode_value <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    tab <- sort(table(x), decreasing = TRUE)
    names(tab)[1]
  }

  p_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    ""
  }

  safe_kw_p <- function(x, g) {
    ok <- !is.na(x) & !is.na(g)
    x2 <- x[ok]
    g2 <- as.character(g[ok])
    if (length(x2) == 0 || length(unique(g2)) < 2) return(NA_real_)
    tryCatch(kruskal.test(x = x2, g = factor(g2))$p.value, error = function(e) NA_real_)
  }

  openings_events <- read_csv(openings_file, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(part_year),
      group = "Opening"
    ) %>%
    filter(year >= 2010, !is.na(year))

  closures_events <- read_csv(closures_file, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(term_year),
      group = "Closure"
    ) %>%
    filter(year >= 2010, !is.na(year))

  zip_hsa <- read_csv(crosswalk_file, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()

  ruca <- readxl::read_excel(ruca_file, sheet = "Data") %>%
    transmute(
      zip5 = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      ruca_simple = case_when(
        RUCA1 %in% c(1, 2, 3) ~ "Metropolitan",
        RUCA1 %in% c(4, 5, 6) ~ "Micropolitan",
        RUCA1 %in% c(7, 8, 9) ~ "Small Town",
        RUCA1 == 10 ~ "Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      ruca_grouped = case_when(
        ruca_simple %in% c("Rural", "Small Town") ~ "Rural & Small Town",
        TRUE ~ ruca_simple
      )
    ) %>%
    distinct(zip5, ruca_grouped)

  event_class <- bind_rows(openings_events, closures_events) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(ruca, by = "zip5") %>%
    group_by(hsanum, year, group) %>%
    summarise(ruca_grouped = mode_value(ruca_grouped), .groups = "drop")

  hsa_ruca <- zip_hsa %>%
    left_join(ruca, by = "zip5") %>%
    group_by(hsanum) %>%
    summarise(ruca_grouped_hsa = mode_value(ruca_grouped), .groups = "drop")

  df_all <- df_all %>%
    left_join(event_class, by = c("hsanum", "year", "group")) %>%
    left_join(hsa_ruca, by = "hsanum") %>%
    mutate(
      ruca_grouped = if_else(
        as.character(group) == "non-event" & is.na(ruca_grouped),
        ruca_grouped_hsa,
        ruca_grouped
      )
    ) %>%
    select(-ruca_grouped_hsa)

  panel_stats <- function(df_panel) {
    df_openings_closures <- df_panel %>% filter(group %in% c("Opening", "Closure"))
    df_openings_none <- df_panel %>% filter(group %in% c("Opening", "non-event"))
    df_closures_none <- df_panel %>% filter(group %in% c("Closure", "non-event"))

    means <- df_openings_closures %>%
      select(group, all_of(percentile_cols)) %>%
      pivot_longer(-group, names_to = "Measure", values_to = "Value") %>%
      group_by(Measure, group) %>%
      summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(`Demographic Variable` = pretty_names[Measure]) %>%
      select(`Demographic Variable`, group, Mean) %>%
      pivot_wider(names_from = group, values_from = Mean, names_prefix = "Mean_")

    tests <- map_dfr(percentile_cols, function(v) {
      tibble::tibble(
        `Demographic Variable` = pretty_names[[v]],
        p_c_vs_ne = safe_kw_p(df_closures_none[[v]], df_closures_none$group),
        p_o_vs_ne = safe_kw_p(df_openings_none[[v]], df_openings_none$group),
        p_o_vs_c = safe_kw_p(df_openings_closures[[v]], df_openings_closures$group)
      )
    })

    row_spec %>%
      left_join(means, by = "Demographic Variable") %>%
      left_join(tests, by = "Demographic Variable") %>%
      mutate(
        sig_c_vs_ne = vapply(p_c_vs_ne, p_stars, character(1)),
        sig_o_vs_ne = vapply(p_o_vs_ne, p_stars, character(1)),
        sig_o_vs_c = vapply(p_o_vs_c, p_stars, character(1)),
        closure_mean = if_else(
          is.na(Mean_Closure),
          "",
          paste0(sprintf("%.2f", Mean_Closure), sig_c_vs_ne)
        ),
        opening_mean = if_else(
          is.na(Mean_Opening),
          "",
          paste0(sprintf("%.2f", Mean_Opening), sig_o_vs_ne)
        )
      ) %>%
      select(Category, `Demographic Variable`, row_order, closure_mean, opening_mean, sig_o_vs_c)
  }

  overall_tbl <- panel_stats(df_all)
  urban_tbl <- panel_stats(df_all %>% filter(ruca_grouped == "Metropolitan"))
  rural_tbl <- panel_stats(df_all %>% filter(ruca_grouped == "Rural & Small Town"))

  panel_counts <- function(df_panel, panel_name) {
    df_panel %>%
      select(group, all_of(percentile_cols)) %>%
      pivot_longer(-group, names_to = "Measure", values_to = "Value") %>%
      group_by(Measure, group) %>%
      summarise(N = sum(!is.na(Value)), .groups = "drop") %>%
      mutate(`Demographic Variable` = pretty_names[Measure]) %>%
      select(`Demographic Variable`, group, N) %>%
      pivot_wider(names_from = group, values_from = N, names_prefix = "N_") %>%
      mutate(panel = panel_name) %>%
      select(`Demographic Variable`, panel, N_Closure, N_Opening, `N_non-event`)
  }

  counts_overall <- panel_counts(df_all, "Overall")
  counts_urban <- panel_counts(df_all %>% filter(ruca_grouped == "Metropolitan"), "Urban")
  counts_rural <- panel_counts(df_all %>% filter(ruca_grouped == "Rural & Small Town"), "Rural & Small Town")

  combined <- overall_tbl %>%
    rename(
      overall_closure = closure_mean,
      overall_opening = opening_mean,
      overall_o_vs_c = sig_o_vs_c
    ) %>%
    left_join(
      urban_tbl %>%
        rename(
          urban_closure = closure_mean,
          urban_opening = opening_mean,
          urban_o_vs_c = sig_o_vs_c
        ) %>%
        select(`Demographic Variable`, urban_closure, urban_opening, urban_o_vs_c),
      by = "Demographic Variable"
    ) %>%
    left_join(
      rural_tbl %>%
        rename(
          rural_closure = closure_mean,
          rural_opening = opening_mean,
          rural_o_vs_c = sig_o_vs_c
        ) %>%
        select(`Demographic Variable`, rural_closure, rural_opening, rural_o_vs_c),
      by = "Demographic Variable"
    ) %>%
    arrange(row_order)

  section_order <- c("Socioeconomic Status", "Demographics", "Insurance Coverage")
  section_lines <- c()
  for (idx in seq_along(section_order)) {
    section <- section_order[[idx]]
    rows <- combined %>% filter(Category == section) %>% arrange(row_order)
    section_lines <- c(
      section_lines,
      sprintf("    \\multicolumn{10}{l}{\\textbf{%s}} \\\\", section),
      vapply(
        seq_len(nrow(rows)),
        function(i) {
          lbl <- gsub("%", "\\%", rows$`Demographic Variable`[[i]], fixed = TRUE)
          sprintf(
            paste0(
              "    \\hspace{2mm} %s & %s & %s & %s",
              " & %s & %s & %s",
              " & %s & %s & %s \\\\"
            ),
            lbl,
            rows$overall_closure[[i]], rows$overall_opening[[i]], rows$overall_o_vs_c[[i]],
            rows$urban_closure[[i]], rows$urban_opening[[i]], rows$urban_o_vs_c[[i]],
            rows$rural_closure[[i]], rows$rural_opening[[i]], rows$rural_o_vs_c[[i]]
          )
        },
        character(1)
      )
    )
    if (idx < length(section_order)) section_lines <- c(section_lines, "    \\addlinespace", "")
  }

  tex_body <- c(
    "% Requires: booktabs, threeparttable, caption, array",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\setcounter{table}{2}",
    "    \\captionof{table}{Community Characteristics by Event Type and Urbanicity}",
    "    \\begin{tabular}{lccc|ccc|ccc}",
    "    \\toprule",
    "    & \\multicolumn{3}{c}{\\textbf{Overall}} & \\multicolumn{3}{c}{\\textbf{Urban}} & \\multicolumn{3}{c}{\\textbf{Rural \\& Small Town}} \\\\",
    "    \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
    "    & Closures & Openings & \\shortstack{Closures vs.\\\\Openings} & Closures & Openings & \\shortstack{Closures vs.\\\\Openings} & Closures & Openings & \\shortstack{Closures vs.\\\\Openings} \\\\",
    "    \\midrule",
    section_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} Entries in the Closures and Openings columns report the mean percentile for the event group. Stars appended to those values indicate the significance of the event-group versus non-event Kruskal-Wallis comparison for that variable within the panel.",
    "      \\item The Closures vs. Openings column reports the significance of the Kruskal-Wallis comparison of closure and opening percentile distributions within the panel. Across the table, * denotes p$<$0.05, ** denotes p$<$0.01, and *** denotes p$<$0.001; no stars indicate the comparison is not statistically significant.",
    "      \\item Urban is Metropolitan. Rural panel includes Rural and Small Town. Overall includes all records, including rows without RUCA matches. Sample sizes vary by variable; see the appendix for variable-specific N values by panel and group.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )

  tex_standalone <- c(
    "\\documentclass[varwidth=22in, border=10pt]{standalone}",
    "\\usepackage{booktabs}",
    "\\usepackage{threeparttable}",
    "\\usepackage{caption}",
    "\\usepackage{array}",
    "",
    "\\begin{document}",
    tex_body,
    "\\end{document}"
  )

  dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(tex_body, file.path(out_table_dir, "brief_summary_table.tex"))
  writeLines(tex_standalone, file.path(out_table_dir, "brief_summary_table_standalone.tex"))

  counts_combined <- row_spec %>%
    left_join(
      counts_overall %>%
        rename(
          overall_n_closure = N_Closure,
          overall_n_opening = N_Opening,
          overall_n_nonevent = `N_non-event`
        ) %>%
        select(`Demographic Variable`, overall_n_closure, overall_n_opening, overall_n_nonevent),
      by = "Demographic Variable"
    ) %>%
    left_join(
      counts_urban %>%
        rename(
          urban_n_closure = N_Closure,
          urban_n_opening = N_Opening,
          urban_n_nonevent = `N_non-event`
        ) %>%
        select(`Demographic Variable`, urban_n_closure, urban_n_opening, urban_n_nonevent),
      by = "Demographic Variable"
    ) %>%
    left_join(
      counts_rural %>%
        rename(
          rural_n_closure = N_Closure,
          rural_n_opening = N_Opening,
          rural_n_nonevent = `N_non-event`
        ) %>%
        select(`Demographic Variable`, rural_n_closure, rural_n_opening, rural_n_nonevent),
      by = "Demographic Variable"
    ) %>%
    arrange(row_order)

  counts_section_lines <- c()
  for (idx in seq_along(section_order)) {
    section <- section_order[[idx]]
    rows <- counts_combined %>% filter(Category == section) %>% arrange(row_order)
    counts_section_lines <- c(
      counts_section_lines,
      sprintf("    \\multicolumn{10}{l}{\\textbf{%s}} \\\\", section),
      vapply(
        seq_len(nrow(rows)),
        function(i) {
          lbl <- gsub("%", "\\%", rows$`Demographic Variable`[[i]], fixed = TRUE)
          fmt_n <- function(x) ifelse(is.na(x), "", as.character(as.integer(x)))
          sprintf(
            "    \\hspace{2mm} %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            lbl,
            fmt_n(rows$overall_n_closure[[i]]), fmt_n(rows$overall_n_opening[[i]]), fmt_n(rows$overall_n_nonevent[[i]]),
            fmt_n(rows$urban_n_closure[[i]]), fmt_n(rows$urban_n_opening[[i]]), fmt_n(rows$urban_n_nonevent[[i]]),
            fmt_n(rows$rural_n_closure[[i]]), fmt_n(rows$rural_n_opening[[i]]), fmt_n(rows$rural_n_nonevent[[i]])
          )
        },
        character(1)
      )
    )
    if (idx < length(section_order)) counts_section_lines <- c(counts_section_lines, "    \\addlinespace", "")
  }

  counts_tex_body <- c(
    "% Variable-specific sample sizes by panel/group",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\captionof{table}{Variable-Specific Sample Sizes for Community Characteristics Table}",
    "    \\begin{tabular}{lccc|ccc|ccc}",
    "    \\toprule",
    "    & \\multicolumn{3}{c}{\\textbf{Overall}} & \\multicolumn{3}{c}{\\textbf{Urban}} & \\multicolumn{3}{c}{\\textbf{Rural \\& Small Town}} \\\\",
    "    \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
    "    & Closure N & Opening N & Non-event N & Closure N & Opening N & Non-event N & Closure N & Opening N & Non-event N \\\\",
    "    \\midrule",
    counts_section_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} N counts represent the number of non-missing HSA-year observations contributing to each variable within each panel/group.",
    "      \\item For the event columns, counts reflect HSA-year event observations in which an opening or closure occurred, not unique hospital CCNs. The non-event columns count HSA-year observations without an opening or closure in that year.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )

  counts_tex_standalone <- c(
    "\\documentclass[varwidth=20in, border=10pt]{standalone}",
    "\\usepackage{booktabs}",
    "\\usepackage{threeparttable}",
    "\\usepackage{caption}",
    "\\usepackage{array}",
    "",
    "\\begin{document}",
    counts_tex_body,
    "\\end{document}"
  )

  writeLines(counts_tex_body, file.path(out_table_dir, "brief_summary_table_variable_n.tex"))
  writeLines(counts_tex_standalone, file.path(out_table_dir, "brief_summary_table_variable_n_standalone.tex"))

  invisible(list(
    overall = overall_tbl,
    urban = urban_tbl,
    rural_small_town = rural_tbl,
    counts = counts_combined
  ))
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
      "certbeds_per_1000_residents_lag1",
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
      "Certified beds per 1,000 residents",
      "Population density",
      "Population change (%)",
      "Any health insurance (%)",
      "Public health insurance (%)"
    ),
    Category = c(
      rep("Socioeconomic Status", 5),
      rep("Demographics", 3),
      rep("Insurance Coverage", 2)
    ),
    row_order = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)
  )

  section_order <- c("Socioeconomic Status", "Demographics", "Insurance Coverage")
  odd_years <- c(2011L, 2013L, 2015L, 2017L, 2019L, 2021L, 2023L)

  fmt_num <- function(x, digits = 1, dollars = FALSE) {
    if (is.na(x)) return("---")
    if (dollars) return(formatC(round(x), format = "f", digits = 0, big.mark = ","))
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }

  esc_tex <- function(x) {
    x <- gsub("%", "\\%", x, fixed = TRUE)
    x
  }

  write_with_standalone <- function(body_lines, body_path, standalone_path, varwidth) {
    standalone_lines <- c(
      sprintf("\\documentclass[varwidth=%s, border=10pt]{standalone}", varwidth),
      "\\usepackage{booktabs}",
      "\\usepackage{threeparttable}",
      "\\usepackage{caption}",
      "\\usepackage{array}",
      "",
      "\\begin{document}",
      body_lines,
      "\\end{document}"
    )
    writeLines(body_lines, body_path)
    writeLines(standalone_lines, standalone_path)
  }

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
    select(Category, PrettyName, row_order, Min, Q1, Median, Q3, Max) %>%
    arrange(row_order)

  # Longitudinal medians
  longitudinal_medians <- df_all %>%
    filter(year %in% odd_years) %>%
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
    arrange(row_order) %>%
    relocate(Category, PrettyName, row_order, all_of(as.character(odd_years)))

  quant_section_lines <- c()
  for (idx in seq_along(section_order)) {
    section <- section_order[[idx]]
    rows <- quantile_table %>% filter(Category == section) %>% arrange(row_order)
    quant_section_lines <- c(
      quant_section_lines,
      sprintf("    \\textbf{%s} &  &  &  &  &  \\\\", section),
      vapply(
        seq_len(nrow(rows)),
        function(i) {
          label <- esc_tex(rows$PrettyName[[i]])
          is_income <- rows$PrettyName[[i]] == "Median household income"
          sprintf(
            "    \\hspace{2mm} %s & %s & %s & %s & %s & %s \\\\",
            label,
            fmt_num(rows$Min[[i]], dollars = is_income),
            fmt_num(rows$Q1[[i]], dollars = is_income),
            fmt_num(rows$Median[[i]], dollars = is_income),
            fmt_num(rows$Q3[[i]], dollars = is_income),
            fmt_num(rows$Max[[i]], dollars = is_income)
          )
        },
        character(1)
      )
    )
    if (idx < length(section_order)) quant_section_lines <- c(quant_section_lines, "    \\addlinespace")
  }

  time_section_lines <- c()
  for (idx in seq_along(section_order)) {
    section <- section_order[[idx]]
    rows <- final_time_table %>% filter(Category == section) %>% arrange(row_order)
    time_section_lines <- c(
      time_section_lines,
      sprintf("    \\textbf{%s} &  &  &  &  &  &  &  \\\\", section),
      vapply(
        seq_len(nrow(rows)),
        function(i) {
          label <- esc_tex(rows$PrettyName[[i]])
          is_income <- rows$PrettyName[[i]] == "Median household income"
          values <- vapply(
            as.character(odd_years),
            function(col) fmt_num(rows[[col]][[i]], dollars = is_income),
            character(1)
          )
          sprintf(
            "    \\hspace{2mm} %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            label, values[[1]], values[[2]], values[[3]], values[[4]], values[[5]], values[[6]], values[[7]]
          )
        },
        character(1)
      )
    )
    if (idx < length(section_order)) time_section_lines <- c(time_section_lines, "    \\addlinespace")
  }

  dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)

  quant_tex_body <- c(
    "% Requires: booktabs, threeparttable, caption, array",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\setcounter{table}{4}",
    "    \\captionof{table}{Descriptive Statistics of Hospital Service Area (HSA) Community Characteristics, 2016}",
    "    \\begin{tabular}{lccccc}",
    "    \\toprule",
    "    & \\multicolumn{5}{c}{\\textbf{Quantiles}} \\\\",
    "    \\cmidrule(lr){2-6}",
    "    & Min & Q1 & Median & Q3 & Max \\\\",
    "    \\midrule",
    quant_section_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} This table reports the minimum, 25th percentile, median, 75th percentile, and maximum of HSA-level community characteristics in 2016. HSA-level values are population-weighted averages of ZIP-code-level data.",
    "      \\item Certified beds per 1,000 residents is based on the prior calendar year. Population density is measured as persons per square mile, and median household income is reported in nominal dollars.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )

  time_tex_body <- c(
    "% Requires: booktabs, threeparttable, caption, array",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\setcounter{table}{5}",
    "    \\captionof{table}{Longitudinal Trends in Median Community Characteristics, 2011--2023}",
    "    \\begin{tabular}{lccccccc}",
    "    \\toprule",
    "    & \\multicolumn{7}{c}{\\textbf{Median Values (Odd Years)}} \\\\",
    "    \\cmidrule(lr){2-8}",
    "    & 2011 & 2013 & 2015 & 2017 & 2019 & 2021 & 2023 \\\\",
    "    \\midrule",
    time_section_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} This table displays longitudinal median values of HSA-level community characteristics for odd-numbered years from 2011 through 2023. HSA-level values are population-weighted averages of ZIP-code-level data, and median household income is reported in nominal dollars.",
    "      \\item Certified beds per 1,000 residents is based on the prior calendar year and therefore begins in 2012; blank entries indicate years in which the measure is unavailable. Population density is measured as persons per square mile.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )

  write_with_standalone(
    quant_tex_body,
    file.path(out_table_dir, "appendix_quantiles_2016.tex"),
    file.path(out_table_dir, "appendix_quantiles_2016_standalone.tex"),
    varwidth = "16in"
  )
  write_with_standalone(
    time_tex_body,
    file.path(out_table_dir, "appendix_medians_over_time.tex"),
    file.path(out_table_dir, "appendix_medians_over_time_standalone.tex"),
    varwidth = "22in"
  )

  invisible(list(quantiles_2016 = quantile_table, medians_over_time = final_time_table))
}
