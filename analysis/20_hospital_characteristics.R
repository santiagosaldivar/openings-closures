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
#' @param out_counts_csv Output CSV path for hospital group counts and union total
#' @return final summary dataframe (invisible)
run_hospital_characteristics <- function(
  openings_path = "data/interim/openings_clean.csv",
  closures_path = "data/interim/closures_clean.csv",
  pos_path = "data/processed/pos_panel_updated.csv",
  out_tex = "outputs/tables/hospital_characteristics.tex",
  out_tex_sensitivity = "outputs/tables/hospital_characteristics_sensitivity.tex",
  out_counts_csv = "outputs/tables/hospital_group_counts.csv"
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

  active_hospitals <- subset(panel_wo_events, panel_wo_events$active == 1)
  all_active_hospitals <- subset(pos_panel_updated, pos_panel_updated$active == 1)
  n_openings <- length(unique(panel_openings$ccn))
  n_closures <- length(unique(panel_closures$ccn))
  n_nonevent <- length(unique(active_hospitals$ccn))
  n_all_active <- length(unique(all_active_hospitals$ccn))
  total_unique_hospitals <- dplyr::n_distinct(c(
    panel_openings$ccn,
    panel_closures$ccn,
    active_hospitals$ccn
  ))

  counts_df <- tibble::tibble(
    metric = c(
      "closures",
      "openings",
      "non_event_active",
      "all_active_hospitals",
      "total_unique_hospitals"
    ),
    value = c(
      n_closures,
      n_openings,
      n_nonevent,
      n_all_active,
      total_unique_hospitals
    ),
    definition = c(
      "Unique hospitals observed with a closure event in the POS-matched workflow.",
      "Unique hospitals observed with an opening event in the POS-matched workflow.",
      "Unique hospitals observed as active in years without an opening or closure event.",
      "Unique hospitals observed as active in the cleaned POS panel, regardless of event status in a given year.",
      "Unique hospitals that opened, closed, or were observed as active in non-event years."
    )
  )

  variable_specs <- tibble::tribble(
    ~key, ~section, ~label, ~var_sym, ~is_binary, ~binary_values, ~row_order,
    "for_profit", "Ownership Status (%)", "For-Profit", quo(control), TRUE, list(c(4, 9)), 1L,
    "non_profit", "Ownership Status (%)", "Non-Profit", quo(control), TRUE, list(c(1, 2)), 2L,
    "public", "Ownership Status (%)", "Public", quo(control), TRUE, list(c(5, 6, 7, 8)), 3L,
    "teach_major", "Teaching Status (%)", "Major Teaching", quo(teach_major), TRUE, list(c(1)), 4L,
    "teach_majgrad", "Teaching Status (%)", "Graduate Teaching", quo(teach_majgrad), TRUE, list(c(1)), 5L,
    "certbeds", "Capacity", "Certified Beds (Mean)", quo(certbeds), FALSE, list(NULL), 6L
  )

  calc_event_overall <- function(df, var_col_sym, is_binary, binary_values = NULL) {
    df <- df %>% filter(!is.na(!!var_col_sym))
    if (nrow(df) == 0) return(NA_real_)
    values <- df %>% pull(!!var_col_sym)
    if (is_binary) {
      mean(values %in% binary_values, na.rm = TRUE) * 100
    } else {
      mean(values, na.rm = TRUE)
    }
  }

  calc_nonevent_overall <- function(df, var_col_sym, is_binary, binary_values = NULL) {
    valid <- df %>% filter(active == 1, !is.na(!!var_col_sym))
    if (nrow(valid) == 0) return(NA_real_)
    if (is_binary) {
      mean(valid %>% pull(!!var_col_sym) %in% binary_values, na.rm = TRUE) * 100
    } else {
      mean(valid %>% pull(!!var_col_sym), na.rm = TRUE)
    }
  }

  calc_nonevent_weighted <- function(df, var_col_sym, is_binary, binary_values = NULL, weight_df) {
    yearly <- df %>%
      filter(active == 1, !is.na(!!var_col_sym)) %>%
      group_by(year) %>%
      summarise(
        yearly_value = if (is_binary) {
          mean((!!var_col_sym) %in% binary_values, na.rm = TRUE) * 100
        } else {
          mean(!!var_col_sym, na.rm = TRUE)
        },
        .groups = "drop"
      )
    if (nrow(yearly) == 0) return(NA_real_)
    yearly %>%
      left_join(weight_df, by = "year") %>%
      mutate(weight = coalesce(weight, 0)) %>%
      summarise(weighted_avg = sum(yearly_value * weight)) %>%
      pull(weighted_avg)
  }

  final_summary_df <- variable_specs %>%
    rowwise() %>%
    mutate(
      Closures = calc_event_overall(panel_closures, var_sym, is_binary, binary_values[[1]]),
      Openings = calc_event_overall(panel_openings, var_sym, is_binary, binary_values[[1]]),
      NonEvent = calc_nonevent_overall(panel_wo_events, var_sym, is_binary, binary_values[[1]]),
      NonEvent_OpeningWeighted = calc_nonevent_weighted(panel_wo_events, var_sym, is_binary, binary_values[[1]], yearly_openings_weights),
      NonEvent_ClosureWeighted = calc_nonevent_weighted(panel_wo_events, var_sym, is_binary, binary_values[[1]], yearly_closures_weights)
    ) %>%
    ungroup() %>%
    arrange(row_order)

  # Main table: Closures/Openings/Non-event (single non-event baseline)
  main_table_df <- final_summary_df %>%
    transmute(
      `Hospital Group` = label,
      Closures = round(Closures, 2),
      Openings = round(Openings, 2),
      `Non-event` = round(NonEvent, 2)
    )

  main_latex <- main_table_df %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = "lccc",
      caption = "Descriptive Characteristics of Hospitals: Openings, Closures, and Non-event Averages, 2010-2023",
      col.names = c(
        "Hospital Group",
        sprintf("\\shortstack{Closures\\\\(N = %s)}", format(n_closures, big.mark = ",")),
        sprintf("\\shortstack{Openings\\\\(N = %s)}", format(n_openings, big.mark = ",")),
        sprintf("\\shortstack{Non-event\\\\(N = %s)}", format(n_nonevent, big.mark = ","))
      )
    ) %>%
    pack_rows("Ownership Status (%)", 1, 3, bold = TRUE) %>%
    pack_rows("Teaching Status (%)", 4, 5, bold = TRUE) %>%
    pack_rows("Capacity", 6, 6, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = "This table displays descriptive statistics for hospitals categorized by operational status between 2010 and 2023. \"Openings\" and \"Closures\" are measured in the year of the respective event. The \"Non-event\" group includes active hospitals in years without opening/closure events. Ownership and teaching status are reported as percentages.",
      general_title = "Note:",
      footnote_as_chunk = TRUE
    )

  # Sensitivity table: retain both weighted non-event baselines
  sensitivity_df <- final_summary_df %>%
    transmute(
      `Hospital Group` = label,
      Closures = round(Closures, 2),
      Openings = round(Openings, 2),
      `Non-event (Opening-weighted)` = round(NonEvent_OpeningWeighted, 2),
      `Non-event (Closure-weighted)` = round(NonEvent_ClosureWeighted, 2)
    )

  get_val <- function(df, row_label, col_name) {
    out <- df %>% filter(`Hospital Group` == row_label) %>% pull(all_of(col_name))
    if (length(out) == 0) return(NA_real_)
    out[[1]]
  }

  fmt <- function(x) sprintf("%.2f", x)

  main_body <- c(
    "\\multicolumn{4}{l}{\\textbf{Ownership Status (\\%)}} \\\\",
    sprintf("\\hspace{3mm} For-Profit & %s & %s & %s \\\\",
            fmt(get_val(main_table_df, "For-Profit", "Closures")),
            fmt(get_val(main_table_df, "For-Profit", "Openings")),
            fmt(get_val(main_table_df, "For-Profit", "Non-event"))),
    sprintf("\\hspace{3mm} Non-Profit & %s & %s & %s \\\\",
            fmt(get_val(main_table_df, "Non-Profit", "Closures")),
            fmt(get_val(main_table_df, "Non-Profit", "Openings")),
            fmt(get_val(main_table_df, "Non-Profit", "Non-event"))),
    sprintf("\\hspace{3mm} Public     & %s & %s  & %s \\\\",
            fmt(get_val(main_table_df, "Public", "Closures")),
            fmt(get_val(main_table_df, "Public", "Openings")),
            fmt(get_val(main_table_df, "Public", "Non-event"))),
    "\\addlinespace",
    "",
    "\\multicolumn{4}{l}{\\textbf{Teaching Status (\\%)}} \\\\",
    sprintf("\\hspace{3mm} Major Teaching & %s & %s & %s \\\\",
            fmt(get_val(main_table_df, "Major Teaching", "Closures")),
            fmt(get_val(main_table_df, "Major Teaching", "Openings")),
            fmt(get_val(main_table_df, "Major Teaching", "Non-event"))),
    sprintf("\\hspace{3mm} Graduate Teaching & %s & %s & %s \\\\",
            fmt(get_val(main_table_df, "Graduate Teaching", "Closures")),
            fmt(get_val(main_table_df, "Graduate Teaching", "Openings")),
            fmt(get_val(main_table_df, "Graduate Teaching", "Non-event"))),
    "\\addlinespace",
    "",
    "\\multicolumn{4}{l}{\\textbf{Capacity}} \\\\",
    sprintf("\\hspace{3mm} Certified Beds (Mean) & %s & %s & %s \\\\",
            fmt(get_val(main_table_df, "Certified Beds (Mean)", "Closures")),
            fmt(get_val(main_table_df, "Certified Beds (Mean)", "Openings")),
            fmt(get_val(main_table_df, "Certified Beds (Mean)", "Non-event")))
  )

  main_doc <- c(
    "\\documentclass[varwidth=7in, border=10pt]{standalone}",
    "\\usepackage{booktabs}",
    "\\usepackage{siunitx}",
    "\\usepackage{caption}",
    "\\renewcommand{\\tablename}{Exhibit}",
    "\\usepackage{array}",
    "",
    "\\begin{document}",
    "",
    "\\centering",
    "\\setcounter{table}{1}",
    "\\captionof{table}{Descriptive Characteristics of Hospitals: Openings, Closures, and Non-event Averages, 2010-2023}",
    "",
    "\\begin{tabular}{l ",
    "    S[table-format=3.2] ",
    "    S[table-format=3.2] ",
    "    S[table-format=3.2]}",
    "\\toprule",
    "Hospital Group & \\multicolumn{1}{c}{Closures} & \\multicolumn{1}{c}{Openings} & \\multicolumn{1}{c}{Non-event} \\\\",
    sprintf(" & {(N = %s)} & {(N = %s)} & {(N = %s)} \\\\",
            format(n_closures, big.mark = ","), format(n_openings, big.mark = ","), format(n_nonevent, big.mark = ",")),
    "\\midrule",
    "",
    main_body,
    "",
    "\\bottomrule",
    "\\end{tabular}",
    "",
    "\\vspace{5pt}",
    "\\parbox{\\linewidth}{",
    "    \\footnotesize",
    "    \\textit{Note:} The table displays descriptive statistics for hospitals categorized by their operational status between 2010 and 2023. Mean values for hospital characteristics are categorized by annual event status. Means corresponding to ``openings'' and ``closures'' are measured in the year of the respective event. The ``Non-event'' group includes all hospitals that did not experience an opening or closure in a given study year. Ownership and teaching status are reported as percentages.",
    "}",
    "",
    "\\end{document}"
  )

  sens_body <- c(
    "\\multicolumn{5}{l}{\\textbf{Ownership Status (\\%)}} \\\\",
    sprintf("\\hspace{3mm} For-Profit & %s & %s & %s & %s \\\\",
            fmt(get_val(sensitivity_df, "For-Profit", "Closures")),
            fmt(get_val(sensitivity_df, "For-Profit", "Openings")),
            fmt(get_val(sensitivity_df, "For-Profit", "Non-event (Opening-weighted)")),
            fmt(get_val(sensitivity_df, "For-Profit", "Non-event (Closure-weighted)"))),
    sprintf("\\hspace{3mm} Non-Profit & %s & %s & %s & %s \\\\",
            fmt(get_val(sensitivity_df, "Non-Profit", "Closures")),
            fmt(get_val(sensitivity_df, "Non-Profit", "Openings")),
            fmt(get_val(sensitivity_df, "Non-Profit", "Non-event (Opening-weighted)")),
            fmt(get_val(sensitivity_df, "Non-Profit", "Non-event (Closure-weighted)"))),
    sprintf("\\hspace{3mm} Public     & %s & %s & %s & %s \\\\",
            fmt(get_val(sensitivity_df, "Public", "Closures")),
            fmt(get_val(sensitivity_df, "Public", "Openings")),
            fmt(get_val(sensitivity_df, "Public", "Non-event (Opening-weighted)")),
            fmt(get_val(sensitivity_df, "Public", "Non-event (Closure-weighted)"))),
    "\\addlinespace",
    "",
    "\\multicolumn{5}{l}{\\textbf{Teaching Status (\\%)}} \\\\",
    sprintf("\\hspace{3mm} Major Teaching & %s & %s & %s & %s \\\\",
            fmt(get_val(sensitivity_df, "Major Teaching", "Closures")),
            fmt(get_val(sensitivity_df, "Major Teaching", "Openings")),
            fmt(get_val(sensitivity_df, "Major Teaching", "Non-event (Opening-weighted)")),
            fmt(get_val(sensitivity_df, "Major Teaching", "Non-event (Closure-weighted)"))),
    sprintf("\\hspace{3mm} Graduate Teaching & %s & %s & %s & %s \\\\",
            fmt(get_val(sensitivity_df, "Graduate Teaching", "Closures")),
            fmt(get_val(sensitivity_df, "Graduate Teaching", "Openings")),
            fmt(get_val(sensitivity_df, "Graduate Teaching", "Non-event (Opening-weighted)")),
            fmt(get_val(sensitivity_df, "Graduate Teaching", "Non-event (Closure-weighted)"))),
    "\\addlinespace",
    "",
    "\\multicolumn{5}{l}{\\textbf{Capacity}} \\\\",
    sprintf("\\hspace{3mm} Certified Beds (Mean) & %s & %s & %s & %s \\\\",
            fmt(get_val(sensitivity_df, "Certified Beds (Mean)", "Closures")),
            fmt(get_val(sensitivity_df, "Certified Beds (Mean)", "Openings")),
            fmt(get_val(sensitivity_df, "Certified Beds (Mean)", "Non-event (Opening-weighted)")),
            fmt(get_val(sensitivity_df, "Certified Beds (Mean)", "Non-event (Closure-weighted)")))
  )

  sensitivity_doc <- c(
    "\\documentclass[varwidth=9in, border=10pt]{standalone}",
    "\\usepackage{booktabs}",
    "\\usepackage{siunitx}",
    "\\usepackage{caption}",
    "\\renewcommand{\\tablename}{Exhibit}",
    "\\usepackage{array}",
    "",
    "\\begin{document}",
    "",
    "\\centering",
    "\\setcounter{table}{1}",
    "\\captionof{table}{Descriptive Characteristics of Hospitals: Event Groups with Alternative Non-event Weighting, 2010-2023}",
    "",
    "\\begin{tabular}{l ",
    "    S[table-format=3.2] ",
    "    S[table-format=3.2] ",
    "    S[table-format=3.2] ",
    "    S[table-format=3.2]}",
    "\\toprule",
    "Hospital Group & \\multicolumn{1}{c}{Closures} & \\multicolumn{1}{c}{Openings} & \\multicolumn{1}{c}{Opening-weighted Non-event} & \\multicolumn{1}{c}{Closure-weighted Non-event} \\\\",
    sprintf(" & {(N = %s)} & {(N = %s)} & {(N = %s)} & {(N = %s)} \\\\",
            format(n_closures, big.mark = ","), format(n_openings, big.mark = ","), format(n_nonevent, big.mark = ","), format(n_nonevent, big.mark = ",")),
    "\\midrule",
    "",
    sens_body,
    "",
    "\\bottomrule",
    "\\end{tabular}",
    "",
    "\\vspace{5pt}",
    "\\parbox{\\linewidth}{",
    "    \\footnotesize",
    "    \\textit{Note:} The two non-event columns use the same active non-event hospital set and N count but different year-weighting schemes (opening-year vs closure-year event distribution).",
    "}",
    "",
    "\\end{document}"
  )

  out_dir <- dirname(out_tex)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(main_doc, out_tex)
  writeLines(sensitivity_doc, out_tex_sensitivity)
  write_csv(counts_df, out_counts_csv)

  message(
    sprintf(
      "Hospital group counts used in table headers: Closures=%s, Openings=%s, Non-event(active)=%s, Total unique=%s",
      n_closures, n_openings, n_nonevent, total_unique_hospitals
    )
  )

  invisible(list(
    summary = final_summary_df,
    counts = c(
      closures = n_closures,
      openings = n_openings,
      non_event_active = n_nonevent,
      all_active_hospitals = n_all_active,
      total_unique_hospitals = total_unique_hospitals
    ),
    counts_table = counts_df,
    outputs = c(
      main = out_tex,
      sensitivity = out_tex_sensitivity,
      counts_csv = out_counts_csv
    )
  ))
}
