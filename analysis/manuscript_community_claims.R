# Reproduce manuscript community-characteristic claims without rendering figures.
# Run from the project root:
# Rscript -e "source('analysis/manuscript_community_claims.R'); run_manuscript_community_claims()"
#
# Uses the existing staged data. Rebuild upstream data first after changing raw
# inputs. National comparisons use within-year national percentiles; Urban and
# Rural & Small Town use population-weighted HSA assignments and within-year,
# within-geography percentiles. Observations are HSA-year-event-type records,
# with each measure restricted to its available values. Tests match Figure 2:
# pairwise Kruskal-Wallis with BH correction within panel x comparison x domain.
run_manuscript_community_claims <- function(
    out_dir = "outputs/manuscript",
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    national_csv = "data/interim/ntl_hsa_percentiles.csv",
    helper_path = "analysis/15_percentile_outputs.R") {
  helpers <- new.env(parent = environment())
  source(helper_path, local = helpers)
  dat <- read.csv(input_csv, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  national <- read.csv(national_csv, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  spec <- data.frame(
    measure = c("bachelors", "income", "poverty", "unemployment_rate", "SDI",
                "certbeds_per_1000_residents", "population_density", "pop_change_pct",
                "health_insurance", "public_health_insurance"),
    label = c("Bachelor's degree (%)", "Median household income", "Below poverty line (%)",
              "Unemployment rate (%)", "Social deprivation index",
              "Certified beds per 1,000 residents", "Population density",
              "Population change (%)", "Any health insurance (%)", "Public health insurance (%)"),
    category = c(rep("Socioeconomic Status", 5), rep("Demographics", 3),
                 rep("Insurance Coverage", 2)),
    stringsAsFactors = FALSE
  )
  rank_cols <- c(paste0(spec$measure, "_percentile"),
                 paste0(spec$measure, "_percentile_geo"))
  income_col <- "weighted_median_household_income_event"
  require_cols <- function(x, cols, label) {
    missing <- setdiff(cols, names(x))
    if (length(missing)) stop(label, ": missing columns: ", paste(missing, collapse = ", "))
  }
  unique_keys <- function(x, keys, label) {
    if (anyNA(x[keys]) || anyDuplicated(x[keys])) {
      stop(label, ": missing or duplicate ", paste(keys, collapse = "/"), " keys.")
    }
  }
  require_cols(dat, c("hsanum", "year", "group", "geography_type", income_col, rank_cols), input_csv)
  require_cols(national, c("hsanum", "year", income_col), national_csv)
  unique_keys(dat, c("hsanum", "year", "group"), input_csv)
  unique_keys(national, c("hsanum", "year"), national_csv)
  if (any(!dat$year %in% 2010:2023) || any(!national$year %in% 2010:2023)) {
    stop("Data extend outside the manuscript's 2010-2023 window; review the study period.")
  }
  groups <- c("Opening", "Closure", "non-event")
  geographies <- c("Urban", "Rural & Small Town")
  if (any(!dat$group %in% groups)) stop("Unexpected event group.")
  if (any(!is.na(dat$geography_type) & !dat$geography_type %in% geographies)) {
    stop("Unexpected staged HSA geography; review panel definitions.")
  }
  for (col in rank_cols) {
    x <- dat[[col]]
    if (!is.numeric(x) || any(!is.na(x) & (!is.finite(x) | x < 0 | x > 100))) {
      stop("Invalid percentile column: ", col)
    }
  }
  # A dual-event HSA-year may have two rows, but its reference data must agree.
  references <- unique(dat[c("hsanum", "year", "geography_type", income_col, rank_cols)])
  unique_keys(references, c("hsanum", "year"), "HSA-year reference values")
  national_index <- match(paste(references$hsanum, references$year),
                          paste(national$hsanum, national$year))
  if (anyNA(national_index) || !isTRUE(all.equal(
      references[[income_col]], national[[income_col]][national_index], check.attributes = FALSE))) {
    stop("National and event-panel income references disagree; rebuild staged data together.")
  }

  comparisons <- list(
    closure_vs_nonevent = c("Closure", "non-event"),
    opening_vs_nonevent = c("Opening", "non-event"),
    opening_vs_closure = c("Opening", "Closure")
  )
  availability_rows <- means_rows <- test_rows <- list()
  for (panel in c("Overall", geographies)) {
    sub <- if (panel == "Overall") dat else
      dat[!is.na(dat$geography_type) & dat$geography_type == panel, , drop = FALSE]
    for (i in seq_len(nrow(spec))) {
      # Use the same column-selection helper as the forest and summary table.
      x <- helpers$resolve_panel_percentile_value(
        sub, paste0(spec$measure[i], "_percentile"), panel, "hsa_population_weighted")
      info <- data.frame(panel = panel, measure = spec$measure[i], label = spec$label[i],
                         category = spec$category[i], stringsAsFactors = FALSE)
      for (group in groups) {
        in_group <- sub$group == group
        available <- in_group & !is.na(x)
        years <- sort(unique(sub$year[available]))
        availability_rows[[length(availability_rows) + 1L]] <- cbind(info, data.frame(
          group = group, total_hsa_years = sum(in_group), available_n = sum(available),
          missing_n = sum(in_group & is.na(x)),
          unique_hsas = length(unique(sub$hsanum[available])),
          first_year = if (length(years)) min(years) else NA_integer_,
          last_year = if (length(years)) max(years) else NA_integer_,
          available_years = paste(years, collapse = ";"), stringsAsFactors = FALSE))
        means_rows[[length(means_rows) + 1L]] <- cbind(info, data.frame(
          group = group, available_n = sum(available),
          mean_percentile = if (any(available)) mean(x[available]) else NA_real_,
          rounded_mean_percentile = if (any(available)) round(mean(x[available])) else NA_real_,
          reference_distribution = if (panel == "Overall") "National within year" else
            "Within geography and year", stringsAsFactors = FALSE))
      }
      for (comparison in names(comparisons)) {
        pair <- comparisons[[comparison]]
        keep <- sub$group %in% pair & !is.na(x)
        n_a <- sum(sub$group == pair[1] & !is.na(x))
        n_b <- sum(sub$group == pair[2] & !is.na(x))
        raw_p <- if (n_a && n_b) tryCatch(
          stats::kruskal.test(x[keep], factor(sub$group[keep]))$p.value,
          error = function(e) NA_real_) else NA_real_
        test_rows[[length(test_rows) + 1L]] <- cbind(info, data.frame(
          comparison = comparison, group_a = pair[1], group_b = pair[2], n_a = n_a, n_b = n_b,
          raw_p = raw_p, stringsAsFactors = FALSE))
      }
    }
  }
  means <- do.call(rbind, means_rows)
  availability <- do.call(rbind, availability_rows)
  tests <- do.call(rbind, test_rows)
  family <- interaction(tests$panel, tests$comparison, tests$category, drop = TRUE)
  tests$bh_p <- ave(tests$raw_p, family, FUN = function(x) stats::p.adjust(x, method = "BH"))
  tests$significant_raw_05 <- !is.na(tests$raw_p) & tests$raw_p < 0.05
  tests$significant_bh_05 <- !is.na(tests$bh_p) & tests$bh_p < 0.05
  tests$significant_bh_001 <- !is.na(tests$bh_p) & tests$bh_p < 0.001
  significance <- do.call(rbind, lapply(c("Overall", geographies), function(panel) {
    do.call(rbind, lapply(names(comparisons), function(comparison) {
      z <- tests[tests$panel == panel & tests$comparison == comparison, ]
      data.frame(panel = panel, comparison = comparison,
        tests_available = sum(!is.na(z$raw_p)), significant_raw_05 = sum(z$significant_raw_05),
        significant_bh_05 = sum(z$significant_bh_05), significant_bh_001 = sum(z$significant_bh_001),
        significant_bh_measures = paste(z$measure[z$significant_bh_05], collapse = ";"),
        nonsignificant_bh_measures = paste(z$measure[!is.na(z$bh_p) & z$bh_p >= 0.05], collapse = ";"),
        stringsAsFactors = FALSE)
    }))
  }))

  # Appendix quantiles exclude zero income; ranking currently includes it.
  # Preserve both definitions so the diagnostic does not silently change ranks.
  income_2016 <- national[[income_col]][national$year == 2016]
  if (!is.numeric(income_2016) || !any(!is.na(income_2016))) stop("No available 2016 income.")
  if (any(!is.na(income_2016) & (!is.finite(income_2016) | income_2016 < 0))) {
    stop("Invalid 2016 income values.")
  }
  income_example <- do.call(rbind, lapply(c("rank_reference_including_zero", "appendix_positive_only"), function(rule) {
    x <- income_2016[!is.na(income_2016)]
    if (rule == "appendix_positive_only") x <- x[x > 0]
    if (!length(x)) stop("No income observations for ", rule)
    data.frame(year = 2016L, inclusion_rule = rule, all_hsas = length(income_2016),
      missing_income = sum(is.na(income_2016)), zero_income = sum(income_2016 == 0, na.rm = TRUE),
      denominator = length(x), minimum_income = min(x), maximum_income = max(x),
      example_income = 50000, at_or_below_example = sum(x <= 50000),
      example_percentile = 100 * mean(x <= 50000),
      rounded_example_percentile = round(100 * mean(x <= 50000)),
      zero_income_hsas = paste(national$hsanum[national$year == 2016 &
        !is.na(national[[income_col]]) & national[[income_col]] == 0], collapse = ";"),
      stringsAsFactors = FALSE)
  }))
  metadata <- data.frame(
    source = c("Event/non-event percentile panel", "National HSA reference", "Shared percentile helper"),
    path = c(input_csv, national_csv, helper_path),
    md5 = unname(tools::md5sum(c(input_csv, national_csv, helper_path))), stringsAsFactors = FALSE)
  results <- list(means = means, tests = tests, significance_counts = significance,
                  income_example = income_example, availability = availability, metadata = metadata)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in names(results)) {
    write.csv(results[[name]], file.path(out_dir, paste0("manuscript_community_", name, ".csv")),
              row.names = FALSE, na = "NA")
  }
  message("Wrote manuscript community claims to ", out_dir)
  invisible(results)
}
