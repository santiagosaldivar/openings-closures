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
  library(stringr)
})

shared_helper_path <- "cleaning/00_shared_urbanicity_helpers.R"
if (!exists("ocgh_attach_hsa_panel_assignment") && file.exists(shared_helper_path)) {
  source(shared_helper_path, local = FALSE)
}

build_hsa_panel_assignment <- function(
  df_all,
  crosswalk_file,
  ruca_file,
  method = c("event_zip", "hsa_zip_count", "hsa_population_weighted"),
  openings_file = "data/raw/updated_openings_august2025.csv",
  closures_file = "data/raw/updated_closures_august2025.csv",
  zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
  census_root = "data/raw/census_raw_data"
) {
  method <- match.arg(method)
  df_all <- df_all %>% select(-any_of(c("ruca_simple", "ruca_grouped", "geography_type")))
  zip_hsa <- ocgh_load_zip_hsa_lookup(crosswalk_file)
  ruca <- ocgh_load_ruca_lookup(ruca_file)

  if (method == "event_zip") {
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

    # The mode is taken over the pooled two-level `geography_type` rather than
    # the three-level `ruca_grouped`, so Metropolitan and Micropolitan event
    # ZIPs reinforce one another instead of splitting the urban vote.
    event_class <- bind_rows(openings_events, closures_events) %>%
      left_join(zip_hsa, by = "zip5") %>%
      left_join(ruca, by = "zip5") %>%
      group_by(hsanum, year, group) %>%
      summarise(geography_type = ocgh_mode_value(geography_type), .groups = "drop")

    hsa_ruca <- zip_hsa %>%
      left_join(ruca, by = "zip5") %>%
      group_by(hsanum) %>%
      summarise(geography_type_hsa = ocgh_mode_value(geography_type), .groups = "drop")

    return(
      df_all %>%
        left_join(event_class, by = c("hsanum", "year", "group")) %>%
        left_join(hsa_ruca, by = "hsanum") %>%
        mutate(
          geography_type = if_else(
            as.character(group) == "non-event" & is.na(geography_type),
            geography_type_hsa,
            geography_type
          )
        ) %>%
        select(-geography_type_hsa)
    )
  }

  if (method == "hsa_zip_count") {
    hsa_ruca <- ocgh_build_hsa_year_ruca_assignment(
      years = df_all %>% distinct(year) %>% pull(year),
      crosswalk_file = crosswalk_file,
      ruca_file = ruca_file,
      method = "hsa_zip_count",
      zip_zcta_file = zip_zcta_file,
      census_root = census_root
    ) %>%
      select(hsanum, year, geography_type)

    return(df_all %>% left_join(hsa_ruca, by = c("hsanum", "year")))
  }

  hsa_ruca <- ocgh_build_hsa_year_ruca_assignment(
    years = df_all %>% distinct(year) %>% pull(year),
    crosswalk_file = crosswalk_file,
    ruca_file = ruca_file,
    method = "hsa_population_weighted",
    zip_zcta_file = zip_zcta_file,
    census_root = census_root
  ) %>%
    select(hsanum, year, geography_type)

  df_all %>% left_join(hsa_ruca, by = c("hsanum", "year"))
}

panel_assignment_suffix <- function(method) {
  dplyr::case_when(
    method == "hsa_zip_count" ~ "_hsa_zip_count",
    method == "hsa_population_weighted" ~ "_hsa_population_weighted",
    TRUE ~ ""
  )
}

panel_assignment_note <- function(method) {
  dplyr::case_when(
    method == "hsa_zip_count" ~ "Urban pools metropolitan and micropolitan RUCA codes. Panel assignment is based on the plurality of the pooled geography among ZIP codes within each HSA.",
    method == "hsa_population_weighted" ~ paste(
      "Urban pools metropolitan and micropolitan RUCA codes.",
      "Panel assignment is based on the plurality of the pooled geography among ZIP codes within each HSA, weighted by ZIP-level population.",
      "ZIP-level population weights are drawn from the 2010 Decennial Census for 2010 and from ACS 5-year estimates for 2011-2023."
    ),
    TRUE ~ "Urban pools metropolitan and micropolitan RUCA codes. Event rows are classified using event ZIP codes, while non-event rows use the modal HSA assignment."
  )
}

resolve_panel_percentile_value <- function(df, var_name, panel_name, panel_assignment) {
  geo_var_name <- dplyr::case_when(
    panel_assignment == "hsa_population_weighted" ~ paste0(var_name, "_geo"),
    panel_assignment == "hsa_zip_count" ~ paste0(var_name, "_geo_zip_count"),
    TRUE ~ paste0(var_name, "_geo")
  )
  if (panel_name %in% c("Urban", "Rural & Small Town") && geo_var_name %in% names(df)) {
    return(df[[geo_var_name]])
  }
  df[[var_name]]
}

build_panel_long_data <- function(df_all, percentile_cols, row_spec, panel_assignment) {
  panel_frames <- list(
    Overall = df_all,
    Urban = df_all %>% filter(geography_type == "Urban"),
    `Rural & Small Town` = df_all %>% filter(geography_type == "Rural & Small Town")
  )

  bind_rows(lapply(names(panel_frames), function(panel_name) {
    df_panel <- panel_frames[[panel_name]]
    value_df <- tibble::tibble(
      panel = panel_name,
      group = df_panel$group
    )
    for (var_name in percentile_cols) {
      value_df[[var_name]] <- resolve_panel_percentile_value(
        df = df_panel,
        var_name = var_name,
        panel_name = panel_name,
        panel_assignment = panel_assignment
      )
    }
    value_df
  })) %>%
    mutate(panel = factor(panel, levels = c("Overall", "Urban", "Rural & Small Town"))) %>%
    pivot_longer(-c(panel, group), names_to = "Measure", values_to = "Value") %>%
    left_join(row_spec, by = "Measure")
}

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
  ruca_file = "data/raw/RUCA2010zipcode.xlsx",
  panel_assignment = c("event_zip", "hsa_zip_count", "hsa_population_weighted"),
  zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
  census_root = "data/raw/census_raw_data"
) {
  panel_assignment <- match.arg(panel_assignment)

  df_all <- read_csv(input_csv, show_col_types = FALSE) %>%
    mutate(group = factor(group, levels = c("Opening", "Closure", "non-event")))
  df_all <- build_hsa_panel_assignment(
    df_all = df_all,
    crosswalk_file = crosswalk_file,
    ruca_file = ruca_file,
    method = panel_assignment,
    openings_file = openings_file,
    closures_file = closures_file,
    zip_zcta_file = zip_zcta_file,
    census_root = census_root
  )
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
    "public_health_insurance_percentile",
    "SDI_percentile",
    "pop_change_pct_percentile",
    "certbeds_per_1000_residents_percentile"
  )
  six_panel_titles <- c(
    "bachelors_percentile" = "Bachelor's Degree",
    "income_percentile" = "Median Household Income",
    "SDI_percentile" = "Social Deprivation Index",
    "public_health_insurance_percentile" = "Public Health Insurance",
    "pop_change_pct_percentile" = "Population Change",
    "certbeds_per_1000_residents_percentile" = "Certified Beds per 1,000 Residents"
  )
  df_openings_closures_panel <- df_openings_closures
  if (panel_assignment %in% c("hsa_population_weighted", "hsa_zip_count")) {
    geo_suffix <- if (panel_assignment == "hsa_zip_count") "_geo_zip_count" else "_geo"
    geo_vars <- intersect(paste0(percentile_vars, geo_suffix), names(df_openings_closures_panel))
    for (geo_var in geo_vars) {
      base_var <- sub(paste0(geo_suffix, "$"), "", geo_var)
      df_openings_closures_panel[[base_var]] <- df_openings_closures_panel[[geo_var]]
    }
  }

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
      scale_fill_manual(values = c("Closure" = "#D73027", "Opening" = "#2166AC", "non-event" = "gray70"), labels = legend_labels) +
      scale_color_manual(values = c("Closure" = "#D73027", "Opening" = "#2166AC", "non-event" = "gray50"), guide = "none") +
      labs(x = "Group", y = "Within-Year Percentile", fill = "") +
      theme_minimal(base_size = 12)
    if (!is.null(output_path)) {
      ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
    }
    p
  }

  build_subplot_counts <- function(df, vars, figure_label, panel_label) {
    df %>%
      select(group, all_of(vars)) %>%
      pivot_longer(-group, names_to = "Measure", values_to = "Value") %>%
      group_by(Measure, group) %>%
      summarise(n = sum(!is.na(Value)), .groups = "drop") %>%
      mutate(
        figure = figure_label,
        panel = panel_label,
        subplot = dplyr::coalesce(unname(title_lookup[Measure]), unname(six_panel_titles[Measure]), Measure)
      ) %>%
      select(figure, panel, subplot, group, n)
  }

  max_group_labels <- function(counts_df) {
    counts_df %>%
      group_by(group) %>%
      summarise(max_n = max(n, na.rm = TRUE), .groups = "drop") %>%
      mutate(label = paste0(group, " (max n = ", max_n, ")"))
  }

  subplot_count_tables <- list()

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
  combined_counts <- build_subplot_counts(
    df_openings_closures,
    vars_to_plot,
    figure_label = "combined_3_panel_violin",
    panel_label = "Overall"
  )
  subplot_count_tables[[length(subplot_count_tables) + 1]] <- combined_counts
  combined_summary_n <- max_group_labels(combined_counts)
  for (i in seq_along(vars_to_plot)) {
    var_name <- vars_to_plot[i]
    p <- plot_filled_violin(df_openings_closures, var_name, combined_summary_n, output_path = NULL) +
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
  combined_plot <- combined_plot + plot_annotation(
    caption = "Legend labels show the largest event-group N across subplots; detailed subplot Ns are tabulated separately."
  )

  ggsave(file.path(out_fig_dir, "combined_3_panel_violin.png"), combined_plot, width = 20, height = 8, dpi = 300)

  build_six_panel_violin <- function(df_panel, panel_title, output_name) {
    panel_label <- if (grepl("^urban_", output_name)) "Urban" else "Rural & Small Town"
    panel_counts <- build_subplot_counts(df_panel, six_panel_vars, output_name, panel_label)
    subplot_count_tables[[length(subplot_count_tables) + 1]] <<- panel_counts
    summary_n_max <- max_group_labels(panel_counts)
    panel_plots <- map2(
      six_panel_vars,
      seq_along(six_panel_vars),
      function(var_name, idx) {
        p <- plot_filled_violin(df_panel, var_name, summary_n_max, output_path = NULL) +
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
      "0 to 100 percentile range. Legend labels show the largest event-group N across",
      "the six subplots; detailed subplot-specific counts are reported in the auxiliary table."
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
    df_openings_closures_panel %>% filter(geography_type == "Urban"),
    "Urban Openings and Closures: Socioeconomic Status and Insurance Coverage",
    "urban_openings_closures_six_panel_violin.png"
  )
  build_six_panel_violin(
    df_openings_closures_panel %>% filter(geography_type == "Rural & Small Town"),
    "Rural and Small Town Openings and Closures: Socioeconomic Status and Insurance Coverage",
    "rural_small_town_openings_closures_six_panel_violin.png"
  )

  subplot_counts_all <- bind_rows(subplot_count_tables)
  write_csv(subplot_counts_all, file.path(out_fig_dir, "violin_subplot_n_summary.csv"))

  violin_counts_lines <- subplot_counts_all %>%
    arrange(figure, panel, subplot, group) %>%
    split(interaction(subplot_counts_all$figure, subplot_counts_all$panel, drop = TRUE)) %>%
    purrr::map(function(df_piece) {
      header <- sprintf(
        "    \\multicolumn{4}{l}{\\textbf{%s (%s)}} \\\\",
        gsub("_", "\\\\_", df_piece$figure[[1]], fixed = TRUE),
        gsub("&", "\\\\&", df_piece$panel[[1]])
      )
      rows <- vapply(
        seq_len(nrow(df_piece)),
        function(i) {
          sprintf(
            "    \\hspace{2mm} %s & %s & %s & %s \\\\",
            gsub("%", "\\%", df_piece$subplot[[i]], fixed = TRUE),
            df_piece$group[[i]],
            as.integer(df_piece$n[[i]]),
            max(df_piece$n[df_piece$group == df_piece$group[[i]]], na.rm = TRUE)
          )
        },
        character(1)
      )
      c(header, rows, "    \\addlinespace")
    }) %>%
    unlist(use.names = FALSE)

  violin_counts_tex <- c(
    "% Subplot counts for violin figures",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\captionof{table}{Subplot-level Event Counts for Violin Figures}",
    "    \\begin{tabular}{llcc}",
    "    \\toprule",
    "    Subplot & Group & N & Figure-level max N for group \\\\",
    "    \\midrule",
    violin_counts_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} Figure legends display the largest group-specific N across subplots within each multi-panel violin figure.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )
  writeLines(
    violin_counts_tex,
    file.path(out_fig_dir, "violin_subplot_n_summary.tex")
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
  ruca_file = "data/raw/RUCA2010zipcode.xlsx",
  panel_assignment = c("event_zip", "hsa_zip_count", "hsa_population_weighted"),
  zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
  census_root = "data/raw/census_raw_data"
) {
  panel_assignment <- match.arg(panel_assignment)
  df_all <- read_csv(input_csv, show_col_types = FALSE) %>%
    mutate(group = factor(group, levels = c("Opening", "Closure", "non-event")))
  df_all <- build_hsa_panel_assignment(
    df_all = df_all,
    crosswalk_file = crosswalk_file,
    ruca_file = ruca_file,
    method = panel_assignment,
    openings_file = openings_file,
    closures_file = closures_file,
    zip_zcta_file = zip_zcta_file,
    census_root = census_root
  )

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
    ~Category, ~Measure, ~`Demographic Variable`, ~row_order,
    "Socioeconomic Status", "bachelors_percentile", "Bachelor's degree (%)", 1L,
    "Socioeconomic Status", "income_percentile", "Median household income", 2L,
    "Socioeconomic Status", "poverty_percentile", "Below poverty line (%)", 3L,
    "Socioeconomic Status", "unemployment_rate_percentile", "Unemployment rate (%)", 4L,
    "Socioeconomic Status", "SDI_percentile", "Social deprivation index", 5L,
    "Demographics", "certbeds_per_1000_residents_percentile", "Certified beds per 1,000 residents", 6L,
    "Demographics", "population_density_percentile", "Population density", 7L,
    "Demographics", "pop_change_pct_percentile", "Population change (%)", 8L,
    "Insurance Coverage", "health_insurance_percentile", "Any health insurance (%)", 9L,
    "Insurance Coverage", "public_health_insurance_percentile", "Public health insurance (%)", 10L
  )
  comparison_labels <- c(
    closure_vs_nonevent = "Closures vs. Non-event",
    opening_vs_nonevent = "Openings vs. Non-event",
    opening_vs_closure = "Openings vs. Closures"
  )
  panel_levels <- c("Overall", "Urban", "Rural & Small Town")
  panel_prefixes <- c(
    "Overall" = "overall",
    "Urban" = "urban",
    "Rural & Small Town" = "rural_small_town"
  )

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

  long_data <- build_panel_long_data(
    df_all = df_all,
    percentile_cols = percentile_cols,
    row_spec = row_spec,
    panel_assignment = panel_assignment
  )

  event_means <- long_data %>%
    filter(group %in% c("Opening", "Closure")) %>%
    group_by(panel, Measure, Category, `Demographic Variable`, row_order, group) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      N = sum(!is.na(Value)),
      .groups = "drop"
    ) %>%
    mutate(
      panel_stub = recode(as.character(panel), !!!panel_prefixes),
      group_key = recode(as.character(group), "Closure" = "closure", "Opening" = "opening")
    )

  variable_counts <- long_data %>%
    group_by(panel, Measure, Category, `Demographic Variable`, row_order, group) %>%
    summarise(N = sum(!is.na(Value)), .groups = "drop") %>%
    mutate(group_key = recode(as.character(group), "Closure" = "closure", "Opening" = "opening", "non-event" = "nonevent")) %>%
    select(-group) %>%
    pivot_wider(names_from = group_key, values_from = N, names_prefix = "n_")

  build_comparison_tests <- function(groups_to_keep, comparison_type_value) {
    long_data %>%
      filter(as.character(group) %in% groups_to_keep) %>%
      group_by(panel, Measure, Category, `Demographic Variable`, row_order) %>%
      summarise(
        raw_p = safe_kw_p(Value, group),
        .groups = "drop"
      ) %>%
      mutate(comparison_type = comparison_type_value)
  }

  comparison_tests <- bind_rows(
    build_comparison_tests(c("Closure", "non-event"), "closure_vs_nonevent"),
    build_comparison_tests(c("Opening", "non-event"), "opening_vs_nonevent"),
    build_comparison_tests(c("Opening", "Closure"), "opening_vs_closure")
  ) %>%
    group_by(panel, comparison_type, Category) %>%
    mutate(
      bh_p = p.adjust(raw_p, method = "BH"),
      stars = vapply(bh_p, p_stars, character(1))
    ) %>%
    ungroup()

  closure_comp <- comparison_tests %>%
    filter(comparison_type == "closure_vs_nonevent") %>%
    mutate(panel_stub = recode(as.character(panel), !!!panel_prefixes)) %>%
    select(panel_stub, Measure, closure_bh_p = bh_p, closure_stars = stars)
  opening_comp <- comparison_tests %>%
    filter(comparison_type == "opening_vs_nonevent") %>%
    mutate(panel_stub = recode(as.character(panel), !!!panel_prefixes)) %>%
    select(panel_stub, Measure, opening_bh_p = bh_p, opening_stars = stars)
  ovc_comp <- comparison_tests %>%
    filter(comparison_type == "opening_vs_closure") %>%
    mutate(panel_stub = recode(as.character(panel), !!!panel_prefixes)) %>%
    select(panel_stub, Measure, open_close_bh_p = bh_p, open_close_stars = stars)

  summary_table <- row_spec %>%
    left_join(
      event_means %>%
        select(panel_stub, Measure, group_key, Mean, N) %>%
        pivot_wider(
          names_from = c(panel_stub, group_key),
          values_from = c(Mean, N),
          names_glue = "{panel_stub}_{group_key}_{.value}"
        ),
      by = "Measure"
    ) %>%
    left_join(
      closure_comp %>%
        pivot_wider(
          names_from = panel_stub,
          values_from = c(closure_bh_p, closure_stars),
          names_glue = "{panel_stub}_{.value}"
        ),
      by = "Measure"
    ) %>%
    left_join(
      opening_comp %>%
        pivot_wider(
          names_from = panel_stub,
          values_from = c(opening_bh_p, opening_stars),
          names_glue = "{panel_stub}_{.value}"
        ),
      by = "Measure"
    ) %>%
    left_join(
      ovc_comp %>%
        pivot_wider(
          names_from = panel_stub,
          values_from = c(open_close_bh_p, open_close_stars),
          names_glue = "{panel_stub}_{.value}"
        ),
      by = "Measure"
    ) %>%
    arrange(row_order)

  fmt_mean_star <- function(mean_value, stars) {
    stars[is.na(stars)] <- ""
    ifelse(is.na(mean_value), "", paste0(sprintf("%.2f", mean_value), stars))
  }
  fmt_n <- function(x) ifelse(is.na(x), "", as.character(as.integer(x)))
  get_or_na <- function(df, col_name) {
    if (col_name %in% names(df)) df[[col_name]] else rep(NA_real_, nrow(df))
  }
  get_or_blank <- function(df, col_name) {
    if (col_name %in% names(df)) df[[col_name]] else rep("", nrow(df))
  }

  for (panel_name in names(panel_prefixes)) {
    prefix <- panel_prefixes[[panel_name]]
    summary_table[[paste0(prefix, "_closure_display")]] <- fmt_mean_star(
      get_or_na(summary_table, paste0(prefix, "_closure_Mean")),
      get_or_blank(summary_table, paste0(prefix, "_closure_stars"))
    )
    summary_table[[paste0(prefix, "_opening_display")]] <- fmt_mean_star(
      get_or_na(summary_table, paste0(prefix, "_opening_Mean")),
      get_or_blank(summary_table, paste0(prefix, "_opening_stars"))
    )
    summary_table[[paste0(prefix, "_ovc_display")]] <- ifelse(
      is.na(get_or_na(summary_table, paste0(prefix, "_open_close_bh_p"))),
      "",
      get_or_blank(summary_table, paste0(prefix, "_open_close_stars"))
    )
  }

  section_order <- c("Socioeconomic Status", "Demographics", "Insurance Coverage")
  section_lines <- c()
  for (idx in seq_along(section_order)) {
    section <- section_order[[idx]]
    rows <- summary_table %>% filter(Category == section) %>% arrange(row_order)
    section_lines <- c(
      section_lines,
      sprintf("    \\multicolumn{16}{l}{\\textbf{%s}} \\\\", section),
      vapply(
        seq_len(nrow(rows)),
        function(i) {
          lbl <- gsub("%", "\\%", rows$`Demographic Variable`[[i]], fixed = TRUE)
          sprintf(
            paste0(
              "    \\hspace{2mm} %s & %s & %s & %s & %s & %s",
              " & %s & %s & %s & %s & %s",
              " & %s & %s & %s & %s & %s \\\\"
            ),
            lbl,
            rows$overall_closure_display[[i]], fmt_n(rows$overall_closure_N[[i]]),
            rows$overall_opening_display[[i]], fmt_n(rows$overall_opening_N[[i]]),
            rows$overall_ovc_display[[i]],
            rows$urban_closure_display[[i]], fmt_n(rows$urban_closure_N[[i]]),
            rows$urban_opening_display[[i]], fmt_n(rows$urban_opening_N[[i]]),
            rows$urban_ovc_display[[i]],
            rows$rural_small_town_closure_display[[i]], fmt_n(rows$rural_small_town_closure_N[[i]]),
            rows$rural_small_town_opening_display[[i]], fmt_n(rows$rural_small_town_opening_N[[i]]),
            rows$rural_small_town_ovc_display[[i]]
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
    "    \\begin{tabular}{lccccc|ccccc|ccccc}",
    "    \\toprule",
    "    & \\multicolumn{5}{c}{\\textbf{Overall}} & \\multicolumn{5}{c}{\\textbf{Urban}} & \\multicolumn{5}{c}{\\textbf{Rural \\& Small Town}} \\\\",
    "    \\cmidrule(lr){2-6} \\cmidrule(lr){7-11} \\cmidrule(lr){12-16}",
    "    & Closure Mean & Closure N & Opening Mean & Opening N & \\shortstack{Open\\\\vs. Close} & Closure Mean & Closure N & Opening Mean & Opening N & \\shortstack{Open\\\\vs. Close} & Closure Mean & Closure N & Opening Mean & Opening N & \\shortstack{Open\\\\vs. Close} \\\\",
    "    \\midrule",
    section_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} Entries in the Closure Mean and Opening Mean columns report the mean within-year percentile for the event group. Stars appended to those means indicate the Benjamini-Hochberg adjusted significance of the event-group versus non-event Kruskal-Wallis comparison for that variable within the panel.",
    "      \\item The Open vs. Close column reports the Benjamini-Hochberg adjusted significance of the Kruskal-Wallis comparison of closure and opening percentile distributions within the panel. Across the table, * denotes adjusted p$<$0.05, ** denotes adjusted p$<$0.01, and *** denotes adjusted p$<$0.001; blank entries indicate adjusted p$\\geq$0.05 or unavailable tests.",
    "      \\item Benjamini-Hochberg correction is applied within each geography type $\\times$ comparison type $\\times$ measure group family. Measure groups correspond to Socioeconomic Status, Demographics, and Insurance Coverage.",
    "      \\item Urban is Metropolitan. Rural panel includes Rural and Small Town. Overall includes all records, including rows without RUCA matches. N counts are variable-specific event-group observation counts; detailed event and non-event counts appear in the appendix.",
    sprintf("      \\item %s", panel_assignment_note(panel_assignment)),
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
  suffix <- panel_assignment_suffix(panel_assignment)
  writeLines(tex_body, file.path(out_table_dir, paste0("brief_summary_table", suffix, ".tex")))
  writeLines(tex_standalone, file.path(out_table_dir, paste0("brief_summary_table", suffix, "_standalone.tex")))

  counts_combined <- row_spec %>%
    left_join(
      variable_counts %>%
        mutate(panel_stub = recode(as.character(panel), !!!panel_prefixes)) %>%
        select(Measure, panel_stub, n_closure, n_opening, n_nonevent) %>%
        pivot_wider(
          names_from = panel_stub,
          values_from = c(n_closure, n_opening, n_nonevent),
          names_glue = "{panel_stub}_{.value}"
        ),
      by = "Measure"
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
          sprintf(
            "    \\hspace{2mm} %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            lbl,
            fmt_n(rows$overall_n_closure[[i]]), fmt_n(rows$overall_n_opening[[i]]), fmt_n(rows$overall_n_nonevent[[i]]),
            fmt_n(rows$urban_n_closure[[i]]), fmt_n(rows$urban_n_opening[[i]]), fmt_n(rows$urban_n_nonevent[[i]]),
            fmt_n(rows$rural_small_town_n_closure[[i]]), fmt_n(rows$rural_small_town_n_opening[[i]]), fmt_n(rows$rural_small_town_n_nonevent[[i]])
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
    "    \\captionof{table}{Variable-Specific Sample Sizes by Event Type and Urbanicity}",
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
    "      \\item \\textit{Note:} N counts represent the number of non-missing HSA-year observations contributing to each variable within each panel/group, across the full study period (2010--2023).",
    "      \\item For the event columns, counts reflect HSA-year event observations in which an opening or closure occurred, not the number of distinct hospitals. The non-event columns count HSA-year observations without an opening or closure in that year. Urban/rural panel assignment follows the same population-weighted plurality RUCA method described in the main table.",
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

  writeLines(counts_tex_body, file.path(out_table_dir, paste0("brief_summary_table_variable_n", suffix, ".tex")))
  writeLines(counts_tex_standalone, file.path(out_table_dir, paste0("brief_summary_table_variable_n", suffix, "_standalone.tex")))

  pvalue_lines <- comparison_tests %>%
    mutate(
      panel = as.character(panel),
      comparison_label = comparison_labels[comparison_type],
      raw_p_fmt = ifelse(is.na(raw_p), "", sprintf("%.4f", raw_p)),
      bh_p_fmt = ifelse(is.na(bh_p), "", sprintf("%.4f", bh_p))
    ) %>%
    arrange(panel, row_order, comparison_type) %>%
    split(.$panel) %>%
    purrr::imap(function(panel_df, panel_name) {
      c(
        sprintf("    \\multicolumn{5}{l}{\\textbf{%s}} \\\\", gsub("&", "\\\\&", panel_name)),
        vapply(
          seq_len(nrow(panel_df)),
          function(i) {
            sprintf(
              "    \\hspace{2mm} %s & %s & %s & %s & %s \\\\",
              gsub("%", "\\%", panel_df$`Demographic Variable`[[i]], fixed = TRUE),
              panel_df$Category[[i]],
              panel_df$comparison_label[[i]],
              panel_df$raw_p_fmt[[i]],
              panel_df$bh_p_fmt[[i]]
            )
          },
          character(1)
        ),
        "    \\addlinespace"
      )
    }) %>%
    unlist(use.names = FALSE)

  pvalue_tex_body <- c(
    "% BH-adjusted p-values for community characteristics table",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\captionof{table}{Raw and Benjamini-Hochberg Adjusted P-values for Community Characteristics Comparisons}",
    "    \\begin{tabular}{llccc}",
    "    \\toprule",
    "    Variable & Measure Group & Comparison & Raw p-value & BH-adjusted p-value \\\\",
    "    \\midrule",
    pvalue_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} Benjamini-Hochberg correction is applied within each geography type $\\times$ comparison type $\\times$ measure group family.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )
  pvalue_tex_standalone <- c(
    "\\documentclass[varwidth=22in, border=10pt]{standalone}",
    "\\usepackage{booktabs}",
    "\\usepackage{threeparttable}",
    "\\usepackage{caption}",
    "",
    "\\begin{document}",
    pvalue_tex_body,
    "\\end{document}"
  )
  writeLines(pvalue_tex_body, file.path(out_table_dir, paste0("brief_summary_table_bh_pvalues", suffix, ".tex")))
  writeLines(pvalue_tex_standalone, file.path(out_table_dir, paste0("brief_summary_table_bh_pvalues", suffix, "_standalone.tex")))

  invisible(list(
    summary = summary_table,
    pvalues = comparison_tests,
    counts = counts_combined
  ))
}

#' Create a 3-panel forest-style percentile summary figure.
#'
#' @param input_csv path to percentile dataset
#' @param out_fig_dir directory for figure outputs
run_percentile_forest_plot <- function(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir = "outputs/figures/percentiles_hsa_zip_count",
    openings_file = "data/raw/updated_openings_august2025.csv",
    closures_file = "data/raw/updated_closures_august2025.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    ruca_file = "data/raw/RUCA2010zipcode.xlsx",
    panel_assignment = c("hsa_zip_count", "hsa_population_weighted"),
    zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
    census_root = "data/raw/census_raw_data"
) {
  panel_assignment <- match.arg(panel_assignment)
  
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
    ~Category, ~Measure, ~`Demographic Variable`, ~row_order,
    "Socioeconomic Status", "bachelors_percentile", "Bachelor's degree (%)", 1L,
    "Socioeconomic Status", "income_percentile", "Median household income", 2L,
    "Socioeconomic Status", "poverty_percentile", "Below poverty line (%)", 3L,
    "Socioeconomic Status", "unemployment_rate_percentile", "Unemployment rate (%)", 4L,
    "Socioeconomic Status", "SDI_percentile", "Social deprivation index", 5L,
    "Demographics", "certbeds_per_1000_residents_percentile", "Certified beds per 1,000 residents", 6L,
    "Demographics", "population_density_percentile", "Population density", 7L,
    "Demographics", "pop_change_pct_percentile", "Population change (%)", 8L,
    "Insurance Coverage", "health_insurance_percentile", "Any health insurance (%)", 9L,
    "Insurance Coverage", "public_health_insurance_percentile", "Public health insurance (%)", 10L
  )
  
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
  
  ci_bounds <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    mean_value <- if (n > 0) mean(x) else NA_real_
    sd_value <- if (n > 1) sd(x) else NA_real_
    se_value <- if (n > 1) sd_value / sqrt(n) else NA_real_
    t_value <- if (n > 1) qt(0.975, df = n - 1) else NA_real_
    moe <- if (n > 1) t_value * se_value else NA_real_
    tibble::tibble(
      n = n,
      mean_value = mean_value,
      ci_lower = if (n > 1) pmax(0, mean_value - moe) else NA_real_,
      ci_upper = if (n > 1) pmin(100, mean_value + moe) else NA_real_
    )
  }
  
  df_all <- read_csv(input_csv, show_col_types = FALSE) %>%
    mutate(group = factor(group, levels = c("Opening", "Closure", "non-event")))
  df_all <- build_hsa_panel_assignment(
    df_all = df_all,
    crosswalk_file = crosswalk_file,
    ruca_file = ruca_file,
    method = panel_assignment,
    openings_file = openings_file,
    closures_file = closures_file,
    zip_zcta_file = zip_zcta_file,
    census_root = census_root
  )
  
  panel_data <- build_panel_long_data(
    df_all = df_all,
    percentile_cols = percentile_cols,
    row_spec = row_spec,
    panel_assignment = panel_assignment
  )
  
  event_summary <- panel_data %>%
    filter(group %in% c("Opening", "Closure")) %>%
    group_by(panel, Measure, Category, `Demographic Variable`, row_order, group) %>%
    reframe(ci_bounds(Value)) %>%
    ungroup() %>%
    mutate(
      row_position = 11 - row_order,
      y = row_position + if_else(as.character(group) == "Opening", 0.16, -0.16)
    )
  
  star_summary <- panel_data %>%
    filter(group %in% c("Opening", "Closure")) %>%
    group_by(panel, Measure, Category, `Demographic Variable`, row_order) %>%
    summarise(
      raw_p_open_vs_close = safe_kw_p(Value, group),
      .groups = "drop"
    ) %>%
    group_by(panel, Category) %>%
    mutate(
      p_open_vs_close = p.adjust(raw_p_open_vs_close, method = "BH"),
      stars = vapply(p_open_vs_close, p_stars, character(1))
    ) %>%
    ungroup() %>%
    mutate(row_position = 11 - row_order)

  max_group_n <- event_summary %>%
    group_by(panel, group) %>%
    summarise(max_n = max(n, na.rm = TRUE), .groups = "drop")
  
  star_header <- tibble::tibble(
    panel = factor("Overall", levels = c("Overall", "Urban", "Rural & Small Town")),
    x = 112,
    y = 10.95,
    label = "Open vs. Close"
  )

  note_text <- paste(
    "Points show mean percentiles; horizontal lines show t-based 95% confidence intervals for mean percentiles.\n",
    "Stars denote the Benjamini-Hochberg adjusted Kruskal-Wallis significance level for the openings-versus-closures comparison within each panel.\n",
    "Legend labels show the largest event-group N across displayed subplot rows; detailed Ns are reported in the auxiliary table.\n",
    panel_assignment_note(panel_assignment)
  )
  
  # Standard (asterisk-column) palette
  palette_values <- c("Opening" = "#2166AC", "Closure" = "#D73027")
  shape_values   <- c("Opening" = 18, "Closure" = 16)
  
  label_values <- row_spec$`Demographic Variable`
  panel_file_stub <- c(
    "Overall" = "overall",
    "Urban" = "urban",
    "Rural & Small Town" = "rural_small_town"
  )
  
  build_forest_plot <- function(
    panel_filter = NULL,
    include_caption = TRUE,
    include_legend = TRUE,
    font_family = "Times",
    header_panels = NULL,
    x_max = 116,
    star_x = 112,
    y_axis_face = "plain",
    shade_by_significance = FALSE,
    y_axis_size = 11,
    strip_size = 12,
    axis_text_x_size = 11,
    axis_title_size = 12,
    legend_text_size = 11,
    point_size = 2.4,
    segment_size = 0.8,
    right_margin = 50,
    orientation = c("vertical", "horizontal")
  ) {
    orientation <- match.arg(orientation)
    plot_data <- event_summary
    star_data <- star_summary
    header_data <- star_header
    
    if (!is.null(header_panels)) {
      header_data <- header_data %>% filter(as.character(panel) %in% header_panels)
    }
    
    header_data <- header_data %>% mutate(x = star_x)
    
    if (!is.null(panel_filter)) {
      plot_data <- plot_data %>% filter(as.character(panel) == panel_filter)
      star_data <- star_data %>% filter(as.character(panel) == panel_filter)
      header_data <- header_data %>% filter(as.character(panel) == panel_filter)
    }

    legend_data <- max_group_n
    if (!is.null(panel_filter)) {
      legend_data <- legend_data %>% filter(as.character(panel) == panel_filter)
    } else {
      legend_data <- legend_data %>%
        group_by(group) %>%
        summarise(max_n = max(max_n, na.rm = TRUE), .groups = "drop")
    }
    legend_labels <- legend_data %>%
      mutate(label = paste0(group, " (max n = ", max_n, ")")) %>%
      { setNames(.$label, .$group) }
    # Alternating gray background in the plot
    shaded_rows <- tibble::tibble(row_position = c(2, 4, 6, 8, 10)) %>%
      mutate(ymin = row_position - 0.5, ymax = row_position + 0.5)

    
    p <- ggplot(plot_data, aes(x = mean_value, y = y, color = group, shape = group)) +
      geom_rect(
        data = shaded_rows,
        aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "gray92"
      ) +
      geom_vline(xintercept = 50, linetype = "dashed", color = "gray45", linewidth = 0.5) +
      geom_segment(aes(x = ci_lower, xend = ci_upper, yend = y),
                   linewidth = segment_size, na.rm = TRUE) +
      geom_point(size = point_size, na.rm = TRUE) +
      scale_color_manual(values = palette_values, name = "", labels = legend_labels) +
      scale_shape_manual(values = shape_values, name = "", labels = legend_labels)

    if (!shade_by_significance) {
      p <- p +
        geom_text(
          data = star_data,
          aes(x = star_x, y = row_position, label = stars),
          inherit.aes = FALSE,
          color = "black",
          size = 4,
          family = font_family
        ) +
        geom_text(
          data = header_data,
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          color = "black",
          size = 3.2,
          fontface = "bold",
          family = font_family
        )
    }
    
    p <- p +
      scale_x_continuous(
        limits = c(0, x_max),
        breaks = c(0, 25, 50, 75, 100),
        labels = c("0", "25", "50", "75", "100"),
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      scale_y_continuous(
        breaks = 10:1,
        labels = label_values,
        limits = c(0.4, 11.2),
        expand = expansion(mult = c(0, 0))
      ) +
      labs(
        x = "Mean Within-Year Percentile",
        y = NULL,
        caption = if (include_caption) note_text else NULL
      ) +
      theme_minimal(base_family = font_family, base_size = 12) +
      theme(
        text = element_text(family = font_family),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill = "white", color = NA),
        legend.position  = if (include_legend) "top" else "none",
        legend.text      = element_text(size = legend_text_size),
        legend.background = element_rect(color = "gray30", fill = "white", linewidth = 0.4),
        legend.margin     = margin(6, 10, 6, 10),
        legend.box.margin = margin(0, 0, 6, 0),
        panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
        strip.text   = element_text(face = "bold", size = strip_size),
        axis.text.y  = element_text(hjust = 1, face = y_axis_face, size = y_axis_size),
        axis.text.x  = element_text(size = axis_text_x_size),
        axis.title.x = element_text(size = axis_title_size),
        plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 8)),
        plot.margin  = margin(12, right_margin, 12, 12)
      )
    
    if (is.null(panel_filter)) {
      if (orientation == "horizontal") {
        # Side-by-side panels: variable labels appear only on leftmost panel
        # because the y-axis is shared via facet_wrap with free-x scales off.
        p <- p + facet_wrap(~ panel, nrow = 1)
      } else {
        p <- p + facet_wrap(~ panel, ncol = 1)
      }
    } else {
      p <- p + ggtitle(panel_filter) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))
    }
    
    p
  }
  
  # Original vertical full plot
  p <- build_forest_plot(include_caption = FALSE)
  
  # Existing vertical poster plot
  p_poster <- build_forest_plot(
    include_caption       = FALSE,
    include_legend        = TRUE,
    font_family           = "Arial",
    shade_by_significance = TRUE,
    x_max                 = 100,
    y_axis_face           = "bold",
    y_axis_size           = 16,
    strip_size            = 20,
    axis_text_x_size      = 14,
    axis_title_size       = 16,
    legend_text_size      = 14,
    point_size            = 3.4,
    segment_size          = 1.1,
    right_margin          = 18
  )
  
  # New: horizontal poster plot (wider, shorter)
  # Sized so the three sub-panels sit side-by-side, with variable labels
  # only on the leftmost panel and a single legend spanning the top.
  p_poster_wide <- build_forest_plot(
    include_caption       = FALSE,
    include_legend        = TRUE,
    font_family           = "Arial",
    shade_by_significance = TRUE,
    x_max                 = 100,
    y_axis_face           = "bold",
    y_axis_size           = 14,
    strip_size            = 20,
    axis_text_x_size      = 13,
    axis_title_size       = 15,
    legend_text_size      = 14,
    point_size            = 3.2,
    segment_size          = 1.0,
    right_margin          = 14,
    orientation           = "horizontal"
  )
  
  dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
  output_stub <- file.path(out_fig_dir, paste0("forest_plot_3panel", panel_assignment_suffix(panel_assignment)))
  
  # Existing outputs (vertical)
  ggsave(paste0(output_stub, ".png"), p, width = 12, height = 14, dpi = 300)
  ggsave(paste0(output_stub, ".pdf"), p, width = 12, height = 14)
  ggsave(paste0(output_stub, "_poster.png"),       p_poster, width = 12, height = 12.5, dpi = 300)
  ggsave(paste0(output_stub, "_poster_hires.png"), p_poster, width = 16, height = 16.7, dpi = 600)
  
  # New outputs (horizontal poster): wider aspect, same hi-res factor as
  # vertical poster (600 dpi version at 1.33x linear scale).
  ggsave(paste0(output_stub, "_poster_wide.png"),       p_poster_wide, width = 20, height = 8.5,  dpi = 300)
  ggsave(paste0(output_stub, "_poster_wide_hires.png"), p_poster_wide, width = 26.7, height = 11.3, dpi = 600)
  
  for (panel_name in levels(panel_data$panel)) {
    panel_plot <- build_forest_plot(
      panel_filter    = panel_name,
      include_caption = FALSE,
      include_legend  = identical(panel_name, "Overall")
    )
    panel_stub <- file.path(
      out_fig_dir,
      paste0(
        "forest_plot_",
        panel_file_stub[[panel_name]],
        panel_assignment_suffix(panel_assignment)
      )
    )
    ggsave(paste0(panel_stub, ".png"), panel_plot, width = 12, height = 4.8, dpi = 300)
  }

  forest_counts <- event_summary %>%
    transmute(
      panel = as.character(panel),
      subplot = `Demographic Variable`,
      group = as.character(group),
      n,
      panel_group_max_n = ave(n, panel, group, FUN = max)
    ) %>%
    arrange(panel, subplot, group)
  write_csv(forest_counts, file.path(out_fig_dir, paste0("forest_plot_subplot_n_summary", panel_assignment_suffix(panel_assignment), ".csv")))

  forest_count_lines <- forest_counts %>%
    split(.$panel) %>%
    purrr::imap(function(panel_df, panel_name) {
      c(
        sprintf("    \\multicolumn{4}{l}{\\textbf{%s}} \\\\", gsub("&", "\\\\&", panel_name)),
        vapply(
          seq_len(nrow(panel_df)),
          function(i) {
            sprintf(
              "    \\hspace{2mm} %s & %s & %s & %s \\\\",
              gsub("%", "\\%", panel_df$subplot[[i]], fixed = TRUE),
              panel_df$group[[i]],
              as.integer(panel_df$n[[i]]),
              as.integer(panel_df$panel_group_max_n[[i]])
            )
          },
          character(1)
        ),
        "    \\addlinespace"
      )
    }) %>%
    unlist(use.names = FALSE)
  forest_counts_tex <- c(
    "% Subplot counts for forest figures",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\captionof{table}{Subplot-level Event Counts for Forest Figures}",
    "    \\begin{tabular}{llcc}",
    "    \\toprule",
    "    Subplot & Group & N & Panel max N for group \\\\",
    "    \\midrule",
    forest_count_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} Forest-plot legends display the largest group-specific N within each panel.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )
  writeLines(
    forest_counts_tex,
    file.path(out_fig_dir, paste0("forest_plot_subplot_n_summary", panel_assignment_suffix(panel_assignment), ".tex"))
  )
  
  invisible(list(summary = event_summary, stars = star_summary, plot = p, plot_wide = p_poster_wide))
}

#' Appendix tables: 2016 quantiles and longitudinal medians
run_appendix_tables <- function(
  input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
  out_table_dir = "outputs/tables"
) {
  df_all <- read_csv(input_csv, show_col_types = FALSE)
  df_unique <- df_all %>% distinct(hsanum, year, .keep_all = TRUE)

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
  quantile_table <- df_unique %>%
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
  longitudinal_medians <- df_unique %>%
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
    "    \\captionof{table}{Distribution of Community Characteristics Across Hospital Service Areas, 2016}",
    "    \\begin{tabular}{lccccc}",
    "    \\toprule",
    "    & \\multicolumn{5}{c}{\\textbf{Summary Statistics}} \\\\",
    "    \\cmidrule(lr){2-6}",
    "    & Min & Q1 & Median & Q3 & Max \\\\",
    "    \\midrule",
    quant_section_lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "      \\small",
    "      \\item \\textit{Note:} This table reports the minimum, 25th percentile, median, 75th percentile, and maximum of HSA-level community characteristics in 2016, across all HSAs with available data. HSA-level values are population-weighted averages of ZIP-code-level data.",
    "      \\item Certified beds per 1,000 residents reflects 2015 certified bed counts. Population density is measured as persons per square mile, and median household income is reported in nominal dollars.",
    "    \\end{tablenotes}",
    "\\end{threeparttable}"
  )

  time_tex_body <- c(
    "% Requires: booktabs, threeparttable, caption, array",
    "\\centering",
    "\\begin{threeparttable}",
    "    \\setcounter{table}{5}",
    "    \\captionof{table}{Median Community Characteristics Across All HSAs, 2011--2023}",
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
    "      \\item \\textit{Note:} This table reports the median of each HSA-level community characteristic for odd-numbered years from 2011 through 2023, computed across all HSAs in the national sample. HSA-level values are population-weighted averages of ZIP-code-level data, and median household income is reported in nominal dollars.",
    "      \\item Certified beds per 1,000 residents uses the prior calendar year's certified bed count and is available from 2010 onward. Population denominators are lagged ACS 5-year estimates from 2012 onward; for 2010 and 2011 the denominator is the 2010 Decennial Census population, and the 2010 value pairs 2009 beds with the contemporaneous (non-lagged) 2010 Census population because no 2009 population source exists. Blank entries throughout indicate years in which data are unavailable due to ACS coverage limitations. Population density is measured as persons per square mile.",
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
