# Build portable manuscript LaTeX and PDF bundles from current analysis outputs.
# Run from the repository root after updating figures/tables:
# Rscript -e "source('analysis/41_build_manuscript_documents.R'); run_manuscript_documents()"

manuscript_document_assets <- function() {
  c(
    "combined_hospital_activity_2way.png" = "outputs/figures/combined_hospital_activity_2way.png",
    "forest_plot_3panel_hsa_population_weighted.png" = "outputs/figures/percentiles_hsa_population_weighted/forest_plot_3panel_hsa_population_weighted.png",
    "beds2010_decile_events_by_poverty.png" = "outputs/figures/beds2010_decile_events_by_poverty.png",
    "urban_openings_closures_six_panel_violin.png" = "outputs/figures/percentiles_hsa_population_weighted/urban_openings_closures_six_panel_violin.png",
    "opening_closure_counts_by_census_region_poster_hires.png" = "outputs/figures/census_region/opening_closure_counts_by_census_region_poster_hires.png",
    "rural_small_town_openings_closures_six_panel_violin.png" = "outputs/figures/percentiles_hsa_population_weighted/rural_small_town_openings_closures_six_panel_violin.png",
    "beds2010_baseline_histogram.png" = "outputs/figures/beds2010_baseline_histogram.png",
    "popchange_decile_events_by_poverty.png" = "outputs/figures/popchange_decile_events_by_poverty.png",
    "popchange2014_baseline_histogram.png" = "outputs/figures/popchange2014_baseline_histogram.png",
    "hospital_characteristics.tex" = "outputs/tables/hospital_characteristics.tex",
    "appendix_medians_over_time_standalone.tex" = "outputs/tables/appendix_medians_over_time_standalone.tex",
    "brief_summary_table_hsa_population_weighted_standalone.tex" = "outputs/tables/brief_summary_table_hsa_population_weighted_standalone.tex",
    "appendix_quantiles_2016_standalone.tex" = "outputs/tables/appendix_quantiles_2016_standalone.tex",
    "beds2010_decile_events_by_poverty.csv" = "outputs/tables/beds2010_decile_events_by_poverty.csv",
    "popchange_decile_events_by_poverty.csv" = "outputs/tables/popchange_decile_events_by_poverty.csv",
    "urban_openings_closures_six_panel_violin_standalone.tex" = "outputs/figures/percentiles_hsa_population_weighted/urban_openings_closures_six_panel_violin_standalone.tex",
    "rural_small_town_openings_closures_six_panel_violin_standalone.tex" = "outputs/figures/percentiles_hsa_population_weighted/rural_small_town_openings_closures_six_panel_violin_standalone.tex"
  )
}

manuscript_table_parts <- function(path) {
  lines <- readLines(path, warn = FALSE)
  title <- grep("\\captionof{table}{", lines, fixed = TRUE, value = TRUE)
  if (length(title) != 1L) stop("Expected one table caption: ", path)
  title <- sub("^.*\\\\captionof\\{table\\}\\{", "", title)
  title <- sub("\\}\\s*$", "", title)
  first <- grep("\\begin{tabular", lines, fixed = TRUE)
  last <- grep("\\end{tabular", lines, fixed = TRUE)
  if (length(first) != 1L || length(last) != 1L) stop("Expected one tabular: ", path)
  tabular <- lines[first:last]
  # Wrap column headings for the wide summary without changing their wording.
  for (label in c("Closure Mean", "Closure N", "Opening Mean", "Opening N")) {
    tabular <- gsub(label, paste0("\\shortstack{", sub(" ", "\\\\", label, fixed = TRUE), "}"),
                    tabular, fixed = TRUE)
  }
  notes <- grep("\\item ", lines, fixed = TRUE, value = TRUE)
  if (length(notes)) {
    notes <- sub("^.*\\\\item ", "", notes)
  } else {
    notes <- grep("\\textit{Note:}", lines, fixed = TRUE, value = TRUE)
    notes <- sub("^.*?(\\\\textit\\{Note:\\})", "\\1", notes, perl = TRUE)
    notes <- sub("\\\\par\\}\\s*$", "", notes)
  }
  list(title = title, tabular = tabular, notes = notes)
}

manuscript_figure_note <- function(path) {
  lines <- readLines(path, warn = FALSE)
  note <- grep("\\textit{Note:}", lines, fixed = TRUE, value = TRUE)
  note <- sub("^.*?\\\\textit\\{Note:\\} ", "", note, perl = TRUE)
  note <- sub("\\}\\s*$", "", note)
  note <- sub("the auxiliary table", "Table A2", note, fixed = TRUE)
  if (length(note) != 1L) stop("Expected one generated figure note: ", path)
  note
}

run_manuscript_documents <- function(out_dir = "outputs/manuscript",
                                     intro_path = "manuscript/appendix_intro.tex",
                                     compile = TRUE,
                                     latex_engine = Sys.getenv("MANUSCRIPT_LATEX_ENGINE", "pdflatex")) {
  assets <- manuscript_document_assets()
  required <- c(unname(assets), intro_path)
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Missing current manuscript inputs; regenerate them first:\n", paste(missing, collapse = "\n"))
  engine <- if (compile) Sys.which(latex_engine) else ""
  if (compile && !nzchar(engine)) {
    stop("LaTeX engine not found: ", latex_engine,
         ". Install a TeX distribution (e.g. MacTeX or TinyTeX), or set MANUSCRIPT_LATEX_ENGINE. ",
         "Use compile=FALSE only when intentionally generating sources without PDFs.")
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)
  # Check sources before staging, so a stale staged file cannot hide missing input.
  source("analysis/40_stage_manuscript_outputs.R", local = TRUE)
  stage_manuscript_outputs(out_dir)
  for (name in names(assets)) {
    if (!file.copy(assets[[name]], file.path(out_dir, name), overwrite = TRUE)) stop("Cannot stage ", name)
  }
  preamble <- c(
    "% Generated by analysis/41_build_manuscript_documents.R; edit the generator or source analysis, not this file.",
    "\\documentclass[11pt,letterpaper]{article}",
    "\\usepackage[margin=0.7in]{geometry}",
    "\\usepackage[T1]{fontenc}", "\\usepackage{lmodern}",
    "\\usepackage{graphicx,adjustbox,booktabs,array,siunitx,pdflscape}",
    "\\usepackage[hidelinks]{hyperref}",
    "\\setlength{\\parindent}{0pt}", "\\setlength{\\parskip}{6pt}",
    "\\setlength{\\emergencystretch}{2em}", "\\pagestyle{plain}",
    "\\begin{document}"
  )
  figure <- function(number, title, asset, height = "7.6in", notes = NULL) {
    c(paste0("{\\large\\bfseries Figure ", number, ". ", title, "\\par}"),
      "\\vspace{5pt}",
      paste0("\\begin{center}\\includegraphics[width=\\linewidth,height=", height,
             ",keepaspectratio]{", asset, "}\\end{center}"),
      if (length(notes)) paste0("{\\footnotesize\\textit{Notes:} ", notes, "\\par}"),
      "\\clearpage")
  }
  table <- function(number, asset, landscape = FALSE) {
    parts <- manuscript_table_parts(file.path(out_dir, asset))
    c(if (landscape) "\\begin{landscape}",
      paste0("{\\large\\bfseries Table ", number, ". ", parts$title, "\\par}"),
      "\\vspace{6pt}", "\\begingroup",
      if (landscape) "\\fontsize{8}{9.5}\\selectfont" else "\\small",
      "\\setlength{\\tabcolsep}{3pt}", "\\renewcommand{\\arraystretch}{1.18}",
      "\\begin{adjustbox}{max width=\\linewidth}", parts$tabular, "\\end{adjustbox}",
      "\\endgroup\\par\\medskip",
      paste0("{\\footnotesize\\raggedright ", parts$notes, "\\par}"),
      if (landscape) "\\end{landscape}" else "\\clearpage")
  }
  beds <- read.csv(file.path(out_dir, "beds2010_decile_events_by_poverty.csv"))
  pop <- read.csv(file.path(out_dir, "popchange_decile_events_by_poverty.csv"))
  metadata <- function(data, column) {
    value <- unique(data[[column]])
    if (length(value) != 1 || is.na(value)) stop("Missing or inconsistent figure metadata: ", column)
    value
  }
  # Support the historical CSV column name while new runs use poverty_threshold.
  beds_threshold <- metadata(beds, if ("poverty_threshold" %in% names(beds)) "poverty_threshold" else "poverty_national_median")
  beds_mode <- if ("poverty_threshold_mode" %in% names(beds)) metadata(beds, "poverty_threshold_mode") else "hsa_median"
  if (beds_mode != "hsa_median") stop("Main Figure 3 requires the adopted HSA-median poverty split; regenerate the default figure.")
  fmt <- function(x) formatC(x, format = "d", big.mark = ",")
  beds_note <- sprintf(paste0(
    "Hospital events in 2010--2023 are grouped by national deciles of baseline certified beds per 1,000 residents, ",
    "measured as 2009 certified beds divided by 2010 Decennial Census population. Deciles are fixed across %s HSAs with available baseline values; decile 1 has the lowest bed supply. ",
    "Each event is mapped from ZIP to HSA through the Dartmouth Atlas crosswalk and each hospital event is counted once. ",
    "Bars are split at the unweighted HSA median poverty rate (%.1f\\%%), using ACS %d--%d five-year poverty estimates aggregated with the population for whom poverty status was determined. ",
    "The threshold is fixed nationally across the eligible HSAs, rather than recalculated within deciles; values at the threshold enter the upper group. ",
    "Segment labels give counts within each poverty group and end labels give bar totals. The chart includes %s events; exclusions comprise %s events with unmatched ZIPs, %s with missing baseline bed supply, and %s with missing poverty. ",
    "Because the poverty estimation window overlaps the first event years, this split is not strictly pre-treatment."),
    fmt(metadata(beds, "n_hsas_deciled")), beds_threshold,
    metadata(beds, "poverty_source_year") - 4L, metadata(beds, "poverty_source_year"),
    fmt(sum(beds$n_events)), fmt(metadata(beds, "n_events_unmatched_zip")),
    fmt(metadata(beds, "n_events_missing_baseline")), fmt(metadata(beds, "n_events_missing_poverty")))
  pop_baseline <- metadata(pop, "baseline_population_change_year")
  pop_note <- sprintf(paste0(
    "Events during %d--%d are grouped using fixed %d HSA population-change deciles. Baseline change compares ACS five-year population estimates ending in %d and %d. ",
    "Poverty groups use ACS %d--%d estimates and the unweighted median across eligible HSAs (%.2f\\%%), fixed across deciles and event years. ",
    "Each hospital event is counted once. The chart includes %s openings and %s closures; %s events have unmatched ZIPs and %s lack eligible baseline population-change or poverty values."),
    metadata(pop, "event_year_min"), metadata(pop, "event_year_max"), pop_baseline,
    pop_baseline - 3L, pop_baseline - 1L, metadata(pop, "poverty_source_year") - 4L,
    metadata(pop, "poverty_source_year"), metadata(pop, "poverty_threshold"),
    fmt(sum(pop$n_events[pop$event_type == "Opening"])), fmt(sum(pop$n_events[pop$event_type == "Closure"])),
    fmt(metadata(pop, "n_events_unmatched_zip")),
    fmt(metadata(pop, "n_events_missing_baseline_population_change_or_poverty")))
  urban_note <- manuscript_figure_note(file.path(out_dir, "urban_openings_closures_six_panel_violin_standalone.tex"))
  rural_note <- manuscript_figure_note(file.path(out_dir, "rural_small_town_openings_closures_six_panel_violin_standalone.tex"))
  main <- c(preamble,
    figure("1", "Hospital Openings and Closures by HSA Urbanicity, 2010--2023",
           "combined_hospital_activity_2way.png", notes = "Urban and rural/small-town classifications use population-weighted HSA urbanicity in the event year."),
    table("1", "hospital_characteristics.tex"),
    figure("2", "Community Characteristics by Hospital Event Status and HSA Urbanicity",
           "forest_plot_3panel_hsa_population_weighted.png", "7.5in",
           "Points show mean within-year percentiles; horizontal lines show t-based 95\\% confidence intervals. Stars denote Benjamini--Hochberg-adjusted Kruskal--Wallis significance for opening-versus-closure comparisons within each panel. Legend labels show the largest event-group N across measures."),
    figure("3", "Hospital Openings and Closures by Baseline Bed Supply Decile and HSA Poverty, 2010--2023",
           "beds2010_decile_events_by_poverty.png", "4.6in", beds_note),
    figure("4", "Urban Openings and Closures: Socioeconomic Status and Insurance Coverage",
           "urban_openings_closures_six_panel_violin.png", "7.5in", urban_note),
    "\\end{document}")
  appendix <- c(preamble, "\\section*{Appendix}", readLines(intro_path, warn = FALSE), "\\clearpage",
    figure("A1", "Hospital Openings and Closures by Census Region, 2010--2023",
           "opening_closure_counts_by_census_region_poster_hires.png"),
    figure("A2", "Rural and Small-Town Openings and Closures: Socioeconomic Status and Insurance Coverage",
           "rural_small_town_openings_closures_six_panel_violin.png", "7.5in", rural_note),
    figure("A3", "Distribution of Baseline Hospital Bed Supply Across HSAs",
           "beds2010_baseline_histogram.png", notes = sprintf("Baseline bed supply is 2009 certified beds per 1,000 residents using 2010 Decennial Census population. The distribution includes %s HSAs with available baseline values and excludes %s with missing values. The dashed line marks the median.", fmt(metadata(beds, "n_hsas_deciled")), fmt(metadata(beds, "n_hsas_missing_baseline")))),
    figure("A4", sprintf("Hospital Openings and Closures by Population-Change Decile and HSA Poverty, %d--%d", metadata(pop, "event_year_min"), metadata(pop, "event_year_max")),
           "popchange_decile_events_by_poverty.png", notes = pop_note),
    figure("A5", "Distribution of Baseline Population Change Across HSAs",
           "popchange2014_baseline_histogram.png", notes = sprintf("Baseline population change compares the %d and %d ACS five-year population estimates, recorded at staged year %d. One value is shown for each of %s eligible HSAs. The solid line marks zero change and the dashed line marks the median.", pop_baseline - 3L, pop_baseline - 1L, pop_baseline, fmt(metadata(pop, "n_hsas_eligible")))),
    table("A1", "appendix_medians_over_time_standalone.tex"),
    table("A2", "brief_summary_table_hsa_population_weighted_standalone.tex", landscape = TRUE),
    table("A3", "appendix_quantiles_2016_standalone.tex"), "\\end{document}")
  documents <- list(figures = main, appendix = appendix)
  for (name in names(documents)) writeLines(documents[[name]], file.path(out_dir, paste0(name, ".tex")))
  manifest <- data.frame(source = c(unname(assets), intro_path, "analysis/41_build_manuscript_documents.R"),
                         staged_name = c(names(assets), NA, NA), stringsAsFactors = FALSE)
  manifest$md5 <- unname(tools::md5sum(manifest$source))
  write.csv(manifest, file.path(out_dir, "manuscript_document_inputs.csv"), row.names = FALSE)
  if (compile) {
    # Build in a temporary directory: never replace a good PDF with a partial one.
    build_dir <- tempfile("manuscript-latex-")
    dir.create(build_dir)
    on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)
    old_dir <- getwd()
    on.exit(setwd(old_dir), add = TRUE)
    setwd(out_dir)
    for (name in names(documents)) {
      log_path <- file.path(out_dir, paste0(name, "_build.log"))
      for (pass in 1:2) {
        result <- system2(engine, c("-interaction=nonstopmode", "-halt-on-error", "-file-line-error",
                                    "-no-shell-escape", shQuote(paste0("-output-directory=", build_dir)),
                                    shQuote(paste0(name, ".tex"))), stdout = TRUE, stderr = TRUE)
        write(result, log_path, append = pass > 1)
        status <- attr(result, "status")
        if (!is.null(status) && status != 0) stop("LaTeX failed for ", name, "; see ", log_path)
      }
      pdf <- file.path(build_dir, paste0(name, ".pdf"))
      if (!file.exists(pdf) || file.info(pdf)$size == 0) stop("LaTeX produced no PDF for ", name)
    }
    for (name in names(documents)) {
      if (!file.copy(file.path(build_dir, paste0(name, ".pdf")), out_dir, overwrite = TRUE)) stop("Cannot publish ", name, ".pdf")
    }
    setwd(old_dir)
  }
  message("Manuscript bundles written to ", out_dir, if (compile) " (LaTeX and PDF)." else " (LaTeX only).")
  invisible(list(tex = file.path(out_dir, paste0(names(documents), ".tex")),
                 pdf = if (compile) file.path(out_dir, paste0(names(documents), ".pdf"))))
}
