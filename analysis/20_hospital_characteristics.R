# Hospital-year characteristics, with and without hospital ZIP rural location.
# Usage: source("analysis/20_hospital_characteristics.R"); run_hospital_characteristics()

# Optional project-local dependencies (git-ignored).
if (dir.exists(".Rlibs")) .libPaths(c(normalizePath(".Rlibs"), .libPaths()))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source("analysis/hospital_characteristics_notes.R")

hc_group_levels <- c("Closures", "Openings", "Non-event")
hc_ownership_levels <- c("For-profit", "Nonprofit", "Public")
hc_teaching_levels <- c("Major affiliation", "Graduate-school affiliation",
                        "Limited affiliation", "No affiliation")

hc_zip5 <- function(x) {
  x <- trimws(as.character(x))
  valid <- !is.na(x) & grepl("^[0-9]{1,5}$", x)
  out <- rep(NA_character_, length(x))
  out[valid] <- sprintf("%05d", as.integer(x[valid]))
  out
}

hc_ruca_lookup <- function(path) {
  lookup <- readxl::read_excel(path, sheet = "Data") %>%
    transmute(zip5 = hc_zip5(ZIP_CODE), rural_small_town = case_when(
      RUCA1 %in% 7:10 ~ 1L,
      RUCA1 %in% 1:6 ~ 0L,
      TRUE ~ NA_integer_
    )) %>% filter(!is.na(zip5)) %>% distinct()
  if (anyDuplicated(lookup$zip5)) stop("Conflicting RUCA assignments for a ZIP.")
  lookup
}

# CCN is the cluster across years AND groups. Unit weights preserve the
# displayed hospital-year proportions. The F statistic is the second-order
# Rao-Scott correction, not the unadjusted Pearson chi-square statistic.
hc_overall_test <- function(data, variable, categorical = TRUE) {
  dat <- data %>% transmute(ccn, group, value = .data[[variable]]) %>% filter(!is.na(value))
  dat$group <- factor(dat$group, levels = hc_group_levels)
  result <- tibble::tibble(
    key = variable,
    method = if (categorical) "Rao-Scott second-order F" else "Hospital-clustered Wald F (HC1)",
    statistic = NA_real_, df_num = NA_real_, df_den = NA_real_, p_value = NA_real_,
    n_observations = nrow(dat), n_clusters = n_distinct(dat$ccn),
    pearson_chisq_unadjusted = NA_real_, min_expected_count = NA_real_,
    cells_expected_below_5 = NA_integer_, n_cells = NA_integer_, status = "ok"
  )
  if (any(table(dat$group) == 0) || result$n_clusters < 3L) {
    result$status <- "Not estimable: missing group or fewer than three hospitals"
    return(result)
  }
  if (categorical) {
    dat$value <- droplevels(factor(dat$value))
    if (nlevels(dat$value) < 2L) {
      result$status <- "Not estimable: fewer than two observed categories"
      return(result)
    }
    cells <- table(dat$group, dat$value)
    expected <- outer(rowSums(cells), colSums(cells)) / sum(cells)
    result$pearson_chisq_unadjusted <- sum((cells - expected)^2 / expected)
    result$min_expected_count <- min(expected)
    result$cells_expected_below_5 <- sum(expected < 5)
    result$n_cells <- length(cells)
    design <- survey::svydesign(ids = ~ccn, weights = ~1, data = dat)
    test <- survey::svychisq(~group + value, design, statistic = "F")
    result$statistic <- unname(test$statistic)
    result$df_num <- unname(test$parameter["ndf"])
    result$df_den <- unname(test$parameter["ddf"])
    result$p_value <- test$p.value
  } else {
    fit <- lm(value ~ group, data = dat)
    covariance <- sandwich::vcovCL(fit, cluster = dat$ccn, type = "HC1", cadjust = TRUE)
    beta <- coef(fit)[-1L]
    covariance <- covariance[-1L, -1L, drop = FALSE]
    q <- length(beta)
    if (any(!is.finite(covariance)) || qr(covariance)$rank < q) {
      result$status <- "Not estimable: singular clustered covariance"
      return(result)
    }
    result$statistic <- as.numeric(crossprod(beta, solve(covariance, beta))) / q
    result$df_num <- q
    result$df_den <- result$n_clusters - 1L
    result$p_value <- pf(result$statistic, q, result$df_den, lower.tail = FALSE)
  }
  if (!is.finite(result$p_value) || !is.finite(result$statistic)) {
    stop("Non-finite overall test for ", variable, "; inspect category counts and clusters.")
  }
  result
}

hc_fmt_p <- function(p) {
  if (is.na(p)) return("---")
  if (p < 0.001) "<0.001" else sprintf("%.3f", p)
}

#' Generate main and year-weighted sensitivity tables, each with/without rural location.
#' The existing main filename remains the version without rural location.
#' Default POS input matches the explicit input used by run_project.R.
run_hospital_characteristics <- function(
  openings_path = "data/interim/openings_clean.csv",
  closures_path = "data/interim/closures_clean.csv",
  pos_path = "data/processed/pos_panel_reconciled.csv",
  out_tex = "outputs/tables/hospital_characteristics.tex",
  out_tex_sensitivity = "outputs/tables/hospital_characteristics_sensitivity.tex",
  out_counts_csv = "outputs/tables/hospital_group_counts.csv",
  out_tests_csv = "outputs/tables/hospital_characteristics_tests.csv",
  ruca_path = "data/raw/RUCA2010zipcode.xlsx",
  out_tex_with_rural = sub("\\.tex$", "_with_rural.tex", out_tex),
  out_tex_sensitivity_with_rural = sub("\\.tex$", "_with_rural.tex", out_tex_sensitivity),
  out_diagnostics_csv = file.path(dirname(out_tex), "hospital_characteristics_denominators.csv"),
  out_cells_csv = file.path(dirname(out_tex), "hospital_characteristics_cells.csv"),
  out_methods_md = file.path(dirname(out_tex), "hospital_characteristics_methods.md"),
  raw_pos_path = "data/raw/pos_panel_2009_2024.dta",
  include_names_path = "data/raw/pos_do_not_exclude.csv",
  exclude_names_path = "data/raw/POS_double_checking_exclude.csv",
  out_coverage_csv = file.path(dirname(out_tex), "hospital_characteristics_event_coverage.csv")
) {
  for (package in c("survey", "sandwich", "readxl", "haven")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Install required R package: ", package)
  }
  for (path in c(openings_path, closures_path, pos_path, ruca_path,
                 raw_pos_path, include_names_path, exclude_names_path)) {
    if (!file.exists(path)) stop("Input not found: ", path)
  }
  read_panel <- function(path) read_csv(path, show_col_types = FALSE,
                                      col_types = cols(ccn = col_character()))
  openings <- read_panel(openings_path) %>%
    filter(opening == 1) %>% transmute(ccn, year = part_year, opening = 1L)
  closures <- read_panel(closures_path) %>%
    filter(closure == 1) %>% transmute(ccn, year = term_year, closure = 1L)
  # 2009 exists upstream only for lagged beds, not as a study observation year.
  panel <- read_panel(pos_path) %>% filter(between(year, 2010, 2023))
  for (df in list(panel, openings, closures)) {
    if (anyNA(df$ccn) || anyNA(df$year) || anyDuplicated(df[c("ccn", "year")])) {
      stop("Inputs must have unique, nonmissing CCN/year keys.")
    }
  }
  if (any(!is.na(panel$control) & !panel$control %in% c(1, 2, 4:9))) {
    stop("Unexpected ownership code in cleaned analysis panel.")
  }
  if (any(!is.na(panel$medschool) & !panel$medschool %in% 1:4)) {
    stop("Unexpected medical-school affiliation code in analysis panel.")
  }
  # Derive the note from this run's inputs, never from saved historical counts.
  coverage <- hc_event_coverage(
    openings, closures, panel, haven::read_dta(raw_pos_path),
    include_names = read_csv(include_names_path, show_col_types = FALSE)$Name,
    exclude_names = read_csv(exclude_names_path, show_col_types = FALSE)$name
  )
  coverage$counts <- coverage$counts %>% mutate(
    pos_input_md5 = unname(tools::md5sum(pos_path)),
    raw_pos_input_md5 = unname(tools::md5sum(raw_pos_path)),
    openings_input_md5 = unname(tools::md5sum(openings_path)),
    closures_input_md5 = unname(tools::md5sum(closures_path)),
    include_names_input_md5 = unname(tools::md5sum(include_names_path)),
    exclude_names_input_md5 = unname(tools::md5sum(exclude_names_path))
  )
  panel <- panel %>% left_join(openings, by = c("ccn", "year")) %>%
    left_join(closures, by = c("ccn", "year"))
  if (any(panel$opening == 1 & panel$closure == 1, na.rm = TRUE)) {
    stop("A hospital-year has both events; resolve its group before an omnibus test.")
  }
  panel <- panel %>% mutate(
    zip5 = hc_zip5(zip5),
    group = case_when(
      closure == 1 ~ "Closures",
      opening == 1 ~ "Openings",
      active == 1 & (is.na(certbeds) | certbeds > 1) ~ "Non-event",
      TRUE ~ NA_character_
    ),
    group = factor(group, levels = hc_group_levels),
    ownership = factor(case_when(
      control %in% c(4, 9) ~ "For-profit",
      control %in% c(1, 2) ~ "Nonprofit",
      control %in% 5:8 ~ "Public"
    ), levels = hc_ownership_levels),
    # Original exclusive M11 categories, not the nested binary teaching flags.
    teaching = factor(medschool, levels = c(1, 3, 2, 4), labels = hc_teaching_levels)
  ) %>% left_join(hc_ruca_lookup(ruca_path), by = "zip5")
  dat <- panel %>% filter(!is.na(group))
  counts <- setNames(vapply(hc_group_levels, function(g) {
    n_distinct(dat$ccn[dat$group == g])
  }, integer(1)), c("closures", "openings", "non_event_active"))
  counts <- c(counts, all_active_hospitals = n_distinct(panel$ccn[which(panel$active == 1)]),
              total_unique_hospitals = n_distinct(dat$ccn))
  counts_df <- tibble::tibble(metric = names(counts), value = unname(counts), definition = c(
    "Unique hospitals observed with a closure event in the POS-matched workflow.",
    "Unique hospitals observed with an opening event in the POS-matched workflow.",
    "Unique hospitals observed as active in eligible non-event years.",
    "Unique hospitals observed as active in the cleaned POS panel, regardless of annual event status.",
    "Unique hospitals across closure, opening, and active non-event observations."
  ))
  specs <- tibble::tribble(
    ~key, ~section, ~label, ~variable, ~category,
    "for_profit", "Ownership status (\\%)", "For-profit", "ownership", "For-profit",
    "non_profit", "Ownership status (\\%)", "Nonprofit", "ownership", "Nonprofit",
    "public", "Ownership status (\\%)", "Public", "ownership", "Public",
    "teach_major", "Teaching status (\\%)", "Major affiliation", "teaching", "Major affiliation",
    "teach_graduate", "Teaching status (\\%)", "Graduate-school affiliation", "teaching", "Graduate-school affiliation",
    "teach_limited", "Teaching status (\\%)", "Limited affiliation", "teaching", "Limited affiliation",
    "teach_none", "Teaching status (\\%)", "No affiliation", "teaching", "No affiliation",
    "rural_small_town", "Location", "Rural or small-town location (\\%)", "rural_small_town", "1",
    "certbeds", "Capacity", "Certified beds (mean)", "certbeds", NA_character_
  )
  group_data <- lapply(hc_group_levels, function(g) filter(dat, group == g))
  names(group_data) <- hc_group_levels
  event_weights <- function(g) group_data[[g]] %>% count(year, name = "n_events") %>%
    mutate(weight = n_events / sum(n_events)) %>% select(year, weight)
  opening_weights <- event_weights("Openings")
  closure_weights <- event_weights("Closures")
  summarize_value <- function(df, variable, category) {
    x <- df[[variable]]
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    if (is.na(category)) mean(x) else mean(as.character(x) == category) * 100
  }
  summarize_weighted <- function(variable, category, weights) {
    yearly <- group_data[["Non-event"]] %>% group_by(year) %>%
      group_modify(~tibble::tibble(value = summarize_value(.x, variable, category))) %>%
      ungroup() %>% inner_join(weights, by = "year") %>% filter(!is.na(value))
    if (!nrow(yearly) || sum(yearly$weight) == 0) return(NA_real_)
    weighted.mean(yearly$value, yearly$weight)
  }
  final_summary_df <- bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    spec <- specs[i, ]
    spec %>% mutate(
      Closures = summarize_value(group_data[["Closures"]], variable, category),
      Openings = summarize_value(group_data[["Openings"]], variable, category),
      NonEvent = summarize_value(group_data[["Non-event"]], variable, category),
      NonEvent_OpeningWeighted = summarize_weighted(variable, category, opening_weights),
      NonEvent_ClosureWeighted = summarize_weighted(variable, category, closure_weights)
    )
  }))
  variables <- unique(specs$variable)
  tests_tidy <- bind_rows(lapply(variables, function(v) hc_overall_test(dat, v, v != "certbeds"))) %>%
    mutate(
      pos_input_path = pos_path, pos_input_md5 = unname(tools::md5sum(pos_path)),
      openings_input_md5 = unname(tools::md5sum(openings_path)),
      closures_input_md5 = unname(tools::md5sum(closures_path))
    )
  diagnostics <- bind_rows(lapply(variables, function(v) {
    dat %>% group_by(group, .drop = FALSE) %>% summarise(
      characteristic = v, hospital_years = n(), unique_hospitals = n_distinct(ccn),
      nonmissing_hospital_years = sum(!is.na(.data[[v]])),
      missing_hospital_years = sum(is.na(.data[[v]])),
      nonmissing_unique_hospitals = n_distinct(ccn[!is.na(.data[[v]])]), .groups = "drop"
    )
  }))
  cells <- bind_rows(lapply(setdiff(variables, "certbeds"), function(v) {
    observed <- dat %>% filter(!is.na(.data[[v]]))
    tab <- table(observed$group, observed[[v]])
    expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    out <- as.data.frame(tab, stringsAsFactors = FALSE)
    names(out) <- c("group", "category", "observed")
    out$expected_under_independence <- as.vector(expected)
    out$characteristic <- v
    out
  }))
  teaching_methods <- paste(
    "Teaching status uses mutually exclusive CMS medical-school affiliation categories (M11):",
    "major (code 1), graduate school (code 3), limited (code 2), and no affiliation (code 4).",
    "Graduate-school affiliation denotes the most extensive involvement, typically a hospital",
    "and medical school within a university academic medical center; major denotes the next",
    "level of involvement in affiliated graduate medical education (physician residency training)."
  )
  base_methods <- paste(
    "Hospital-year observations are grouped by annual event status in 2010--2023.",
    "Opening and closure characteristics are measured in the event year; non-event observations",
    "are active hospital-years without either event, excluding records with 1 or fewer certified beds.",
    "Header N values count unique hospitals within each group; a hospital may appear in multiple",
    "groups across years. Percentages and mean beds use available hospital-year observations",
    "for each characteristic; missing values are excluded only from that characteristic.", teaching_methods
  )
  base_note <- paste(
    "Header N counts unique hospitals; hospitals may contribute to multiple groups.",
    "Percentages and mean beds summarize hospital-years, excluding missing values by characteristic.",
    "Event characteristics use the event year; non-event observations are active years without",
    "either event, excluding records with 1 or fewer certified beds.", coverage$note,
    "Teaching categories are mutually exclusive CMS medical-school affiliations."
  )
  rural_note <- paste(
    "Rural or small-town location uses the hospital's recorded ZIP and the 2010 ZIP RUCA",
    "classification: primary codes 7--10 versus metropolitan/micropolitan codes 1--6.",
    "Unmatched or unclassified ZIPs are excluded from the location percentage and test."
  )
  rural_missing <- diagnostics %>% filter(characteristic == "rural_small_town")
  rural_missing_note <- paste0(
    "Location is missing for ", paste(rural_missing$missing_hospital_years, collapse = "/"),
    " of ", paste(rural_missing$hospital_years, collapse = "/"),
    " hospital-year observations (closures/openings/non-event)."
  )
  test_methods <- paste(
    "Overall P-values compare all three groups: hospital-clustered, second-order Rao--Scott",
    "tests (F approximation) for categorical characteristics and a hospital-clustered Wald F",
    "test of equal mean certified beds (HC1 covariance; denominator degrees of freedom =",
    "number of hospitals minus 1). One P-value applies to each complete ownership or teaching block.",
    "Tests are unadjusted for other covariates and multiple testing; they do not identify which pair differs."
  )
  test_note <- paste(
    "Overall P-values compare all three groups using hospital-clustered second-order Rao--Scott",
    "F tests for categorical blocks and a Wald F test (HC1) for mean beds;",
    "tests are unadjusted for covariates and multiple comparisons."
  )
  sensitivity_note <- paste(
    "The two non-event columns summarize the same eligible hospitals, weighting annual",
    "non-event means by the distribution of opening or closure event years, respectively.",
    "Weights are normalized over years with an available characteristic.",
    "These are descriptive sensitivity summaries; the main-table tests do not apply to these weighted columns."
  )
  get_p <- function(v) paste0("$", hc_fmt_p(tests_tidy$p_value[match(v, tests_tidy$key)]), "$")
  fmt <- function(x) if (is.na(x)) "{---}" else sprintf("%.1f", x)
  render_table <- function(include_rural = FALSE, sensitivity = FALSE) {
    rows <- final_summary_df
    if (!include_rural) rows <- rows %>% filter(variable != "rural_small_town")
    body <- character()
    for (section_name in unique(rows$section)) {
      section_rows <- rows %>% filter(section == section_name)
      block <- section_rows$variable[[1]] %in% c("ownership", "teaching")
      section_p <- if (!sensitivity && block) get_p(section_rows$variable[[1]]) else ""
      body <- c(body, if (length(body)) "\\addlinespace" else character(),
                sprintf("\\multicolumn{4}{l}{\\textbf{%s}} & %s \\\\", section_name, section_p))
      for (i in seq_len(nrow(section_rows))) {
        row <- section_rows[i, ]
        values <- if (sensitivity) {
          c(row$Closures, row$Openings, row$NonEvent_OpeningWeighted, row$NonEvent_ClosureWeighted)
        } else c(row$Closures, row$Openings, row$NonEvent)
        values <- vapply(values, fmt, character(1))
        if (!sensitivity) values <- c(values, if (block) "" else get_p(row$variable))
        body <- c(body, paste0("\\hspace{3mm} ", row$label, " & ",
                              paste(values, collapse = " & "), " \\\\"))
      }
    }
    headers <- if (sensitivity) {
      c("Characteristic", "Closures", "Openings", "\\shortstack{Opening-weighted\\\\non-event}",
        "\\shortstack{Closure-weighted\\\\non-event}")
    } else c("Characteristic", "Closures", "Openings", "Non-event", "\\shortstack{Overall\\\\$P$-value}")
    header <- paste(c(headers[1], paste0("\\multicolumn{1}{c}{", headers[-1], "}")), collapse = " & ")
    ns <- c(counts[["closures"]], counts[["openings"]], counts[["non_event_active"]])
    if (sensitivity) ns <- c(ns, counts[["non_event_active"]])
    nrow_tex <- paste(c("", paste0("{(N = ", format(ns, big.mark = ",", trim = TRUE), ")}"),
                       if (!sensitivity) "" else character()), collapse = " & ")
    note <- paste(c(base_note,
                    if (include_rural) paste(rural_note, rural_missing_note),
                    if (sensitivity) sensitivity_note else test_note), collapse = " ")
    caption <- if (sensitivity) {
      "Hospital Characteristics with Alternative Non-event Year Weighting, 2010--2023"
    } else "Characteristics of Hospitals by Annual Event Status, 2010--2023"
    c(
      "\\documentclass[border=10pt]{standalone}", "\\usepackage{booktabs,siunitx,caption,array}",
      "\\begin{document}", "\\begin{minipage}{8in}", "\\centering",
      # Keep caption settings in the body so they survive standalone inclusion.
      "\\renewcommand{\\tablename}{Table}",
      "\\captionsetup{labelfont=bf,labelsep=period}", "\\setcounter{table}{0}",
      paste0("\\captionof{table}{", caption, "}"), "\\medskip", "\\setlength{\\tabcolsep}{10pt}",
      paste0("\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}l *{", if (sensitivity) "4" else "3",
             "}{S[table-format=3.1]}", if (!sensitivity) " c" else "", "@{}}"),
      "\\toprule", paste0(header, " \\\\"), paste0(nrow_tex, " \\\\"), "\\midrule",
      body, "\\bottomrule", "\\end{tabular*}", "\\par\\medskip",
      paste0("{\\footnotesize\\raggedright \\textit{Note:} ", note, "\\par}"),
      "\\end{minipage}", "\\end{document}"
    )
  }
  outputs <- c(main = out_tex, with_rural = out_tex_with_rural,
               sensitivity = out_tex_sensitivity, sensitivity_with_rural = out_tex_sensitivity_with_rural,
               counts_csv = out_counts_csv, tests_csv = out_tests_csv,
               denominators_csv = out_diagnostics_csv, cells_csv = out_cells_csv,
               methods_md = out_methods_md, coverage_csv = out_coverage_csv)
  if (anyDuplicated(outputs)) stop("Output paths must be distinct (use .tex extensions for tables).")
  for (directory in unique(dirname(outputs))) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  writeLines(render_table(), out_tex)
  writeLines(render_table(include_rural = TRUE), out_tex_with_rural)
  writeLines(render_table(sensitivity = TRUE), out_tex_sensitivity)
  writeLines(render_table(include_rural = TRUE, sensitivity = TRUE), out_tex_sensitivity_with_rural)
  write_csv(counts_df, out_counts_csv)
  write_csv(tests_tidy, out_tests_csv)
  write_csv(diagnostics, out_diagnostics_csv)
  write_csv(cells, out_cells_csv)
  write_csv(coverage$counts, out_coverage_csv)
  writeLines(c(
    "# Hospital characteristics: manuscript methods", "", "## Text for either table version", "",
    gsub("--", "-", base_methods, fixed = TRUE), "",
    gsub("--", "-", test_methods, fixed = TRUE), "",
    "## Event coverage relative to Figure 1", "", coverage$note, "",
    "Counts and exclusion reasons are recomputed from the curated event lists, the selected",
    "analysis panel, and raw POS records at each run. The event-coverage CSV records those",
    "counts and input hashes. No adjacent-year substitution or CCN reassignment is applied.", "",
    "## Additional text for the version with rural location", "",
    gsub("--", "-", paste(rural_note, rural_missing_note), fixed = TRUE), "",
    "## Supplementary sensitivity table", "", sensitivity_note, "", "## Coding and interpretation", "",
    "Ownership: for-profit = POS control 4/9; nonprofit = 1/2; public = 5/6/7/8.", "",
    "The former Graduate Teaching row used teach_majgrad, which combines M11 codes 1 and 3.",
    "The revised Graduate-school affiliation row uses code 3 alone. The four teaching categories",
    "are exhaustive among nonmissing M11 records and do not overlap. Limited affiliation is",
    "retained separately and must not be described as non-teaching. These classifications are",
    "CMS affiliation categories, not a resident-to-bed ratio or a teaching-association membership measure.", "",
    paste0("Analysis input: `", pos_path, "`. Both main variants use identical observations for shared characteristics."), "",
    "The tests CSV contains adjusted F statistics, numerator/denominator degrees of freedom,",
    "full-precision p-values, and an explicitly labeled unadjusted Pearson statistic for diagnostics.",
    "The cells and denominators CSVs retain category counts, expected counts, and variable-specific missingness.", "",
    "## Sources", "",
    "- [CMS Hospital/CAH Database Worksheet, Exhibit 286, M11 and M13 (2012), PDF pages 3 and 15](https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/Downloads/R80SOMA-.pdf)",
    "- [R survey: Rao-Scott tests and F approximation](https://search.r-project.org/CRAN/refmans/survey/html/svychisq.html)",
    "- Local RUCA source: `data/raw/RUCA2010zipcode.xlsx`, sheet `Data`, primary code `RUCA1`; grouping matches `analysis/baseline_urbanicity_split.R`."
  ), out_methods_md)
  message("Hospital characteristics: generated both main variants and both sensitivity variants.")
  message("Unique hospitals: closures=", counts[["closures"]], ", openings=", counts[["openings"]],
          ", non-event=", counts[["non_event_active"]], ", union=", counts[["total_unique_hospitals"]])
  invisible(list(summary = final_summary_df, counts = counts, counts_table = counts_df,
                 tests = tests_tidy, denominators = diagnostics, cells = cells,
                 coverage = coverage, outputs = outputs))
}
