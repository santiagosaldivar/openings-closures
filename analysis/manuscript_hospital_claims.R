# Reproduce manuscript Table 1 descriptions, Figure 3 poverty shares, and
# annual certified-bed totals without rebuilding or modifying source data.
# Run from the project root:
# Rscript -e "source('analysis/manuscript_hospital_claims.R'); run_manuscript_hospital_claims()"
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

run_manuscript_hospital_claims <- function(
    out_dir = "outputs/manuscript",
    updated_path = "data/processed/pos_panel_updated.csv",
    reconciled_path = "data/processed/pos_panel_reconciled.csv",
    openings_path = "data/interim/openings_clean.csv",
    closures_path = "data/interim/closures_clean.csv",
    raw_openings_path = "data/raw/updated_openings_august2025.csv",
    raw_closures_path = "data/raw/updated_closures_august2025.csv",
    national_path = "data/interim/ntl_hsa_percentiles.csv",
    crosswalk_path = "data/raw/ZipHsaHrr.csv",
    tests_path = "outputs/tables/hospital_characteristics_tests.csv") {
  years <- 2010:2023
  groups <- c("Closures", "Openings", "Non-event")
  unique_keys <- function(dat, keys, label) {
    if (anyNA(dat[keys]) || anyDuplicated(dat[keys])) {
      stop(label, ": missing or duplicate ", paste(keys, collapse = "/"), " keys.")
    }
  }
  read_input <- function(path) read_csv(
    path, col_types = cols(ccn = col_character(), zip5 = col_character()),
    show_col_types = FALSE, progress = FALSE, name_repair = "unique_quiet"
  )
  zip5 <- function(x) {
    x <- trimws(as.character(x))
    valid <- !is.na(x) & grepl("^[0-9]{1,5}$", x)
    result <- rep(NA_character_, length(x))
    result[valid] <- sprintf("%05d", as.integer(x[valid]))
    result
  }
  read_panel <- function(path) {
    dat <- read_input(path) %>% filter(year %in% years)
    unique_keys(dat, c("ccn", "year"), path)
    if (anyNA(dat$active) || !all(dat$active %in% 0:1)) stop("Invalid active flag: ", path)
    if (any(!is.na(dat$control) & !dat$control %in% 1:9)) {
      stop("Unexpected ownership code: ", path)
    }
    if (any(!is.na(dat$medschool) & !dat$medschool %in% 1:4)) {
      stop("Unexpected medical-school affiliation code: ", path)
    }
    dat
  }
  panels <- list(updated = read_panel(updated_path), reconciled = read_panel(reconciled_path))
  openings <- read_input(openings_path) %>% filter(opening == 1, part_year %in% years) %>%
    transmute(ccn, year = part_year, opening = 1L)
  closures <- read_input(closures_path) %>% filter(closure == 1, term_year %in% years) %>%
    transmute(ccn, year = term_year, closure = 1L)
  unique_keys(openings, c("ccn", "year"), "Cleaned openings")
  unique_keys(closures, c("ccn", "year"), "Cleaned closures")

  specs <- tibble::tribble(
    ~measure, ~characteristic, ~category,
    "for_profit", "ownership", "For-profit",
    "non_profit", "ownership", "Nonprofit",
    "public", "ownership", "Public",
    "other_ownership", "ownership", "Other/unspecified",
    "teach_major", "teaching", "Major affiliation",
    "teach_graduate", "teaching", "Graduate-school affiliation",
    "teach_limited", "teaching", "Limited affiliation",
    "teach_none", "teaching", "No affiliation",
    "certbeds", "certbeds", NA_character_
  )
  grouped_panels <- lapply(panels, function(dat) {
    dat <- dat %>% left_join(openings, by = c("ccn", "year")) %>%
      left_join(closures, by = c("ccn", "year"))
    if (any(dat$opening == 1 & dat$closure == 1, na.rm = TRUE)) {
      stop("A hospital-year belongs to both event groups.")
    }
    dat %>% mutate(
      group = case_when(
        closure == 1 ~ "Closures",
        opening == 1 ~ "Openings",
        active == 1 & (is.na(certbeds) | certbeds > 1) ~ "Non-event",
        TRUE ~ NA_character_
      ),
      ownership = case_when(
        control %in% c(4, 9) ~ "For-profit",
        control %in% c(1, 2) ~ "Nonprofit",
        control %in% 5:8 ~ "Public",
        control == 3 ~ "Other/unspecified"
      ),
      teaching = case_when(
        medschool == 1 ~ "Major affiliation",
        medschool == 3 ~ "Graduate-school affiliation",
        medschool == 2 ~ "Limited affiliation",
        medschool == 4 ~ "No affiliation"
      )
    ) %>% filter(!is.na(group))
  })
  characteristics <- bind_rows(lapply(names(grouped_panels), function(panel) {
    bind_rows(lapply(groups, function(g) {
      dat <- grouped_panels[[panel]] %>% filter(group == g)
      bind_rows(lapply(seq_len(nrow(specs)), function(i) {
        spec <- specs[i, ]
        observed <- dat[[spec$characteristic]]
        available <- !is.na(observed)
        values <- observed[available]
        denominator <- length(values)
        numerator <- if (spec$characteristic == "certbeds") sum(values) else sum(values == spec$category)
        spec %>% mutate(
          panel = panel, group = g, numerator = numerator, denominator = denominator,
          value = if (denominator == 0L) NA_real_ else
            numerator / denominator * if (spec$characteristic == "certbeds") 1 else 100,
          units = if (spec$characteristic == "certbeds") "mean certified beds" else "percent",
          unique_hospitals = n_distinct(dat$ccn[available]),
          missing_hospital_years = sum(!available)
        ) %>% select(panel, group, everything())
      }))
    }))
  }))
  denominators <- bind_rows(lapply(names(grouped_panels), function(panel) {
    bind_rows(lapply(unique(specs$characteristic), function(v) {
      grouped_panels[[panel]] %>% group_by(group) %>% summarise(
        panel = panel, characteristic = v, hospital_years = n(),
        unique_hospitals = n_distinct(ccn),
        nonmissing_hospital_years = sum(!is.na(.data[[v]])),
        missing_hospital_years = sum(is.na(.data[[v]])),
        nonmissing_unique_hospitals = n_distinct(ccn[!is.na(.data[[v]])]),
        .groups = "drop"
      ) %>% select(panel, group, everything())
    }))
  }))
  bed_totals <- bind_rows(lapply(names(panels), function(panel) {
    panels[[panel]] %>% filter(active == 1, year %in% c(2010L, 2023L)) %>%
      group_by(year) %>% summarise(
        panel = panel, active_hospitals = n_distinct(ccn),
        hospitals_with_reported_beds = sum(!is.na(certbeds)),
        hospitals_missing_beds = sum(is.na(certbeds)),
        hospitals_with_beds_le_1 = sum(certbeds <= 1, na.rm = TRUE),
        certified_beds = if (all(is.na(certbeds))) NA_real_ else sum(certbeds, na.rm = TRUE),
        .groups = "drop"
      ) %>% mutate(
        definition = "All active POS hospitals in the specified year; no HSA-match or >1-bed restriction"
      ) %>% select(panel, year, everything())
  }))

  # Match Figure 3: hospital events, fixed baseline bed deciles, and one
  # national HSA median of ACS 2008--2012 poverty across the deciled HSAs.
  national <- read_csv(national_path, show_col_types = FALSE, progress = FALSE)
  unique_keys(national, c("hsanum", "year"), "National HSA panel")
  baseline_all <- national %>% filter(year == 2010L) %>%
    select(hsanum, beds2010 = certbeds_per_1000_residents_lag1)
  baseline <- baseline_all %>% filter(!is.na(beds2010)) %>%
    mutate(decile = ntile(beds2010, 10L)) %>%
    left_join(national %>% filter(year == 2012L) %>%
                select(hsanum, poverty = weighted_percent_below_poverty_line_event), by = "hsanum")
  if (nrow(baseline) < 10L || all(is.na(baseline$poverty))) stop("Insufficient baseline data.")
  threshold <- median(baseline$poverty, na.rm = TRUE)
  crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE, progress = FALSE) %>%
    transmute(zip5 = zip5(zipcode19), hsanum = as.integer(hsanum)) %>% distinct()
  unique_keys(crosswalk, "zip5", "ZIP-HSA crosswalk")
  events <- bind_rows(
    read_input(raw_openings_path) %>% transmute(ccn, zip5 = zip5(zip5), year = part_year, event_type = "Opening"),
    read_input(raw_closures_path) %>% transmute(ccn, zip5 = zip5(zip5), year = term_year, event_type = "Closure")
  ) %>% filter(year %in% years)
  unique_keys(events, c("ccn", "event_type"), "Curated raw events")
  clean_keys <- bind_rows(
    openings %>% transmute(ccn, year, event_type = "Opening"),
    closures %>% transmute(ccn, year, event_type = "Closure")
  )
  event_keys <- events %>% select(ccn, year, event_type)
  if (nrow(anti_join(clean_keys, event_keys, by = names(clean_keys))) ||
      nrow(anti_join(event_keys, clean_keys, by = names(clean_keys)))) {
    stop("Raw Figure 3 and cleaned Table 1 event keys disagree.")
  }
  events <- events %>% left_join(crosswalk, by = "zip5") %>% left_join(baseline, by = "hsanum") %>%
    mutate(exclusion = case_when(
      is.na(hsanum) ~ "unmatched_zip", is.na(decile) ~ "missing_baseline_beds",
      is.na(poverty) ~ "missing_poverty", TRUE ~ "included"
    ))
  bed_decile_poverty <- events %>% filter(exclusion == "included") %>%
    group_by(decile, event_type) %>% summarise(
      n_events = n(), n_at_or_above_median_poverty = sum(poverty >= threshold),
      n_below_median_poverty = sum(poverty < threshold), .groups = "drop"
    ) %>% complete(
      decile = 1:10, event_type = c("Closure", "Opening"),
      fill = list(n_events = 0L, n_at_or_above_median_poverty = 0L, n_below_median_poverty = 0L)
    ) %>% mutate(
      percent_at_or_above_median_poverty = if_else(
        n_events > 0L, 100 * n_at_or_above_median_poverty / n_events, NA_real_
      ),
      poverty_median = threshold, poverty_source_year = 2012L,
      baseline_definition = "2009 active-hospital beds per 1,000 residents using 2010 Census population",
      event_window = "2010-2023"
    ) %>% arrange(decile, event_type)
  bed_decile_exclusions <- events %>% group_by(event_type) %>% summarise(
    total_events = n(), plotted_events = sum(exclusion == "included"),
    unmatched_zip = sum(exclusion == "unmatched_zip"),
    missing_baseline_beds = sum(exclusion == "missing_baseline_beds"),
    missing_poverty = sum(exclusion == "missing_poverty"), .groups = "drop"
  ) %>% mutate(
    hsas_at_baseline = nrow(baseline_all), hsas_deciled = nrow(baseline),
    hsas_missing_baseline_beds = sum(is.na(baseline_all$beds2010)),
    hsas_missing_poverty = sum(is.na(baseline$poverty)),
    hsas_below_median_poverty = sum(baseline$poverty < threshold, na.rm = TRUE),
    hsas_at_or_above_median_poverty = sum(baseline$poverty >= threshold, na.rm = TRUE)
  )

  # Preserve the available omnibus evidence separately. This audit does not
  # rerun hypothesis tests or infer pairwise significance from these P-values.
  omnibus_evidence <- tibble::tibble(key = character(), method = character(), p_value = double())
  if (file.exists(tests_path)) {
    omnibus_evidence <- read_csv(tests_path, show_col_types = FALSE, progress = FALSE) %>%
      filter(key %in% unique(specs$characteristic)) %>% mutate(
        source_file = tests_path, reference_panel = "reconciled",
        evidence_scope = "Previously generated omnibus three-group tests; not recomputed; no pairwise conclusion"
      )
    current_sample <- bind_rows(lapply(unique(specs$characteristic), function(v) {
      dat <- grouped_panels$reconciled %>% filter(!is.na(.data[[v]]))
      tibble::tibble(key = v, current_n_observations = nrow(dat), current_n_clusters = n_distinct(dat$ccn))
    }))
    omnibus_evidence <- omnibus_evidence %>% left_join(current_sample, by = "key") %>% mutate(
      sample_counts_match = n_observations == current_n_observations & n_clusters == current_n_clusters
    )
    input_hash_columns <- c("pos_input_md5", "openings_input_md5", "closures_input_md5")
    if (all(input_hash_columns %in% names(omnibus_evidence))) {
      omnibus_evidence <- omnibus_evidence %>% mutate(
        source_inputs_match = pos_input_md5 == unname(tools::md5sum(reconciled_path)) &
          openings_input_md5 == unname(tools::md5sum(openings_path)) &
          closures_input_md5 == unname(tools::md5sum(closures_path))
      )
    } else {
      # Legacy test files have no input hashes; matching sample sizes alone
      # cannot establish which POS panel generated their P-values.
      omnibus_evidence$source_inputs_match <- NA
    }
  }
  paths <- c(updated_path, reconciled_path, openings_path, closures_path,
             raw_openings_path, raw_closures_path, national_path, crosswalk_path)
  if (file.exists(tests_path)) paths <- c(paths, tests_path)
  inputs <- tibble::tibble(path = paths, md5 = unname(tools::md5sum(paths)))
  outputs <- list(
    hospital_characteristics = characteristics, hospital_denominators = denominators,
    bed_decile_poverty = bed_decile_poverty, bed_decile_exclusions = bed_decile_exclusions,
    certified_bed_totals = bed_totals, hospital_omnibus_evidence = omnibus_evidence,
    hospital_inputs = inputs
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in names(outputs)) write_csv(outputs[[name]], file.path(out_dir, paste0("manuscript_", name, ".csv")))
  message("Wrote manuscript hospital-characteristic and bed-decile claims to ", out_dir)
  invisible(outputs)
}
