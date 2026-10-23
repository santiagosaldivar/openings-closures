# Run from the repository root: Rscript tests/test_hospital_characteristics.R
source("analysis/20_hospital_characteristics.R")

# Balanced, repeatedly observed hospitals have identical group distributions.
# Altering outcome labels must not change the omnibus test.
balanced <- expand.grid(ccn = as.character(1:30), group = hc_group_levels,
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
balanced$category <- factor(as.integer(balanced$ccn) %% 3)
balanced$value <- as.integer(balanced$ccn)
null_test <- hc_overall_test(balanced, "category")
stopifnot(abs(null_test$statistic) < 1e-10, abs(null_test$p_value - 1) < 1e-10)

# Repeating an identical observation within a hospital must not manufacture
# additional independent clusters or substantially strengthen the evidence.
clustered <- data.frame(ccn = rep(as.character(1:90), each = 5),
                        group = rep(rep(hc_group_levels, each = 30), each = 5))
clustered$category <- rep(c(rep(1, 10), rep(0, 20), rep(1, 15), rep(0, 15),
                            rep(1, 20), rep(0, 10)), each = 5)
clustered$value <- rep(1:90, each = 5) + rep(0:4, 90)
cluster_test <- hc_overall_test(clustered, "category")
relabeled <- clustered
relabeled$category <- ifelse(clustered$category == 1, "No", "Yes")
stopifnot(isTRUE(all.equal(cluster_test$p_value, hc_overall_test(relabeled, "category")$p_value)))
independent <- clustered
independent$ccn <- as.character(seq_len(nrow(independent)))
stopifnot(cluster_test$n_clusters == 90,
          cluster_test$p_value > hc_overall_test(independent, "category")$p_value)

# The continuous omnibus statistic is invariant to group order and outcome scale.
beds_test <- hc_overall_test(clustered, "value", categorical = FALSE)
scaled <- clustered
scaled$value <- 1000 * scaled$value + 17
stopifnot(isTRUE(all.equal(beds_test$p_value,
                          hc_overall_test(scaled, "value", FALSE)$p_value, tolerance = 1e-8)),
          beds_test$df_num == 2, beds_test$df_den == 89)

# Missing outcomes reduce their own denominators, not the entire analysis.
missing <- clustered
missing$category[1:5] <- NA
stopifnot(hc_overall_test(missing, "category")$n_observations == nrow(clustered) - 5,
          hc_overall_test(missing, "value", FALSE)$n_observations == nrow(clustered))
constant <- clustered
constant$category <- 1
stopifnot(is.na(hc_overall_test(constant, "category")$p_value))
stopifnot(is.na(hc_overall_test(subset(clustered, group != "Openings"), "category")$p_value))
stopifnot(identical(hc_zip5(c("1234", "01234", NA, "", "ABCDE", "123456")),
                    c("01234", "01234", NA_character_, NA_character_, NA_character_, NA_character_)))
stopifnot(hc_fmt_p(0.0001) == "<0.001", hc_fmt_p(0.001) == "0.001", hc_fmt_p(0.12) == "0.120")
message("Statistical and missing-data regression checks passed.")

inputs <- c("data/interim/openings_clean.csv", "data/interim/closures_clean.csv",
            "data/processed/pos_panel_reconciled.csv", "data/raw/RUCA2010zipcode.xlsx",
            "data/raw/pos_panel_2009_2024.dta", "data/raw/pos_do_not_exclude.csv",
            "data/raw/POS_double_checking_exclude.csv")
if (all(file.exists(inputs))) {
  out <- tempfile("hospital_characteristics_check_")
  dir.create(out)
  result <- run_hospital_characteristics(
    out_tex = file.path(out, "hospital_characteristics.tex"),
    out_tex_sensitivity = file.path(out, "hospital_characteristics_sensitivity.tex"),
    out_counts_csv = file.path(out, "counts.csv"), out_tests_csv = file.path(out, "tests.csv")
  )
  stopifnot(nrow(result$tests) == 4, all(result$tests$status == "ok"),
            all(result$tests$p_value >= 0 & result$tests$p_value <= 1),
            all(result$tests$pos_input_md5 == unname(tools::md5sum(inputs[3]))),
            all(file.exists(result$outputs)))
  # Exhaustive, exclusive categories must sum to 100 before rounding.
  for (v in c("ownership", "teaching")) {
    rows <- result$summary[result$summary$variable == v, ]
    for (column in c("Closures", "Openings", "NonEvent",
                     "NonEvent_OpeningWeighted", "NonEvent_ClosureWeighted")) {
      stopifnot(abs(sum(rows[[column]]) - 100) < 1e-8)
    }
  }
  # Same estimates and p-values in both variants; rural is the only extra block.
  main <- readLines(result$outputs[["main"]])
  rural <- readLines(result$outputs[["with_rural"]])
  # The caption settings must survive standalone's body-only inclusion in a
  # figures document with an existing table counter or caption configuration.
  for (variant in c("main", "with_rural", "sensitivity", "sensitivity_with_rural")) {
    rendered <- readLines(result$outputs[[variant]])
    begin <- which(rendered == "\\begin{document}")
    caption <- grep("\\captionof{table}{", rendered, fixed = TRUE)
    for (setting in c("\\renewcommand{\\tablename}{Table}",
                      "\\captionsetup{labelfont=bf,labelsep=period}",
                      "\\setcounter{table}{0}")) {
      position <- which(rendered == setting)
      stopifnot(length(begin) == 1L, length(caption) == 1L, length(position) == 1L,
                begin < position, position < caption)
    }
    stopifnot(any(grepl(result$coverage$note, rendered, fixed = TRUE)))
  }
  # The shorter table note retains the statistical essentials; detailed
  # affiliation definitions and degrees of freedom remain in the methods.
  note <- grep("\\textit{Note:}", main, fixed = TRUE, value = TRUE)
  methods <- readLines(result$outputs[["methods_md"]])
  for (term in c("hospital-years", "missing values by characteristic", "hospital-clustered",
                 "Rao--Scott", "Wald F", "HC1", "covariates and multiple comparisons")) {
    stopifnot(length(note) == 1L, grepl(term, note, fixed = TRUE))
  }
  for (detail in c("Graduate-school affiliation denotes the most extensive involvement",
                   "denominator degrees of freedom =", "number of hospitals minus 1")) {
    stopifnot(!grepl(detail, note, fixed = TRUE), any(grepl(detail, methods, fixed = TRUE)))
  }
  stopifnot(any(grepl(result$coverage$note, methods, fixed = TRUE)))
  shared_rows <- function(x) {
    x <- x[grepl("hspace|multicolumn\\{4\\}", x)]
    x[!grepl("Location|Rural or small-town", x)]
  }
  stopifnot(identical(shared_rows(main), shared_rows(rural)),
            !any(grepl("Rural or small-town", main)),
            any(grepl("Rural or small-town", rural)),
            sum(grepl("\\$<0.001\\$|& \\$[0-9]", main)) == 3,
            sum(grepl("\\$<0.001\\$|& \\$[0-9]", rural)) == 4)
  # One decimal per descriptive numeric cell, including a trailing zero.
  body <- rural[grepl("hspace", rural)]
  for (row in body) {
    fields <- trimws(strsplit(row, "&", fixed = TRUE)[[1]])
    stopifnot(all(grepl("^[0-9]+\\.[0-9]$", fields[2:4])))
  }
  # Independent reconstruction from raw group flags, not the returned summary.
  read_input <- function(path) read_csv(path, show_col_types = FALSE,
                                       col_types = cols(ccn = col_character()))
  p <- read_input(inputs[3]) %>% filter(between(year, 2010, 2023))
  o <- read_input(inputs[1]) %>% select(ccn, part_year, opening)
  c <- read_input(inputs[2]) %>% select(ccn, term_year, closure)
  source_events <- list(
    Opening = o %>% filter(opening == 1) %>% transmute(ccn, year = part_year),
    Closure = c %>% filter(closure == 1) %>% transmute(ccn, year = term_year)
  )
  # Independently anti-join the event lists to the selected panel so a stale
  # historical discrepancy cannot pass by matching another generated output.
  for (event_type in names(source_events)) {
    source <- source_events[[event_type]] %>% filter(between(year, 2010, 2023))
    omitted <- anti_join(source, p, by = c("ccn", "year"))
    retained <- semi_join(source, p, by = c("ccn", "year"))
    coverage <- result$coverage$events[result$coverage$events$event_type == event_type, ]
    reasons <- result$coverage$counts[result$coverage$counts$event_type == event_type, ]
    metric <- if (event_type == "Opening") "openings" else "closures"
    stopifnot(nrow(coverage) == nrow(source), sum(reasons$n) == nrow(source),
              sum(reasons$n[reasons$reason != "retained"]) == nrow(omitted),
              sum(reasons$n[reasons$reason == "retained"]) == nrow(retained),
              sum(coverage$in_characteristics_table) == nrow(retained),
              n_distinct(retained$ccn) == result$counts[[metric]],
              any(grepl(paste0("{(N = ", format(n_distinct(retained$ccn),
                                                big.mark = ",", trim = TRUE), ")}"),
                        main, fixed = TRUE)))
    excluded_keys <- with(coverage[!coverage$in_characteristics_table, ], paste(ccn, year))
    stopifnot(setequal(excluded_keys, with(omitted, paste(ccn, year))))
  }
  saved_coverage <- read_csv(result$outputs[["coverage_csv"]], show_col_types = FALSE)
  stopifnot(isTRUE(all.equal(as.data.frame(saved_coverage),
                            as.data.frame(result$coverage$counts), check.attributes = FALSE)))
  coverage_inputs <- c(pos_input_md5 = inputs[3], raw_pos_input_md5 = inputs[5],
                       openings_input_md5 = inputs[1], closures_input_md5 = inputs[2],
                       include_names_input_md5 = inputs[6], exclude_names_input_md5 = inputs[7])
  for (column in names(coverage_inputs)) {
    stopifnot(all(saved_coverage[[column]] == unname(tools::md5sum(coverage_inputs[[column]]))))
  }
  p <- p %>% left_join(o, by = c("ccn", "year" = "part_year")) %>%
    left_join(c, by = c("ccn", "year" = "term_year"))
  ne <- p %>% filter((is.na(opening) | opening != 1), (is.na(closure) | closure != 1),
                     active == 1, is.na(certbeds) | certbeds > 1)
  ev <- list(Closures = filter(p, closure == 1), Openings = filter(p, opening == 1), NonEvent = ne)
  for (g in names(ev)) {
    stopifnot(abs(result$summary[[g]][result$summary$key == "teach_graduate"] -
                    100 * mean(ev[[g]]$medschool == 3, na.rm = TRUE)) < 1e-8,
              abs(result$summary[[g]][result$summary$key == "certbeds"] -
                    mean(ev[[g]]$certbeds, na.rm = TRUE)) < 1e-8)
  }
  stopifnot(result$counts[["closures"]] == n_distinct(ev$Closures$ccn),
            result$counts[["openings"]] == n_distinct(ev$Openings$ccn),
            result$counts[["non_event_active"]] == n_distinct(ne$ccn))
  message("Study-data integration checks passed; both variants agree on all shared rows.")
} else {
  message("Study-data integration checks skipped: install the local study inputs to run them.")
}
