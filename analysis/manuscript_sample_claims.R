# Reproduce the manuscript's sample, Figure 1, and baseline event claims.
# Primary manuscript definition: closure-reconciled POS and population-weighted
# HSA urbanicity. Original POS and hospital-ZIP results are audit comparisons.
# Run from the project root:
# Rscript -e "source('analysis/manuscript_sample_claims.R'); run_manuscript_sample_claims()"
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

run_manuscript_sample_claims <- function(
    updated_path = "data/processed/pos_panel_updated.csv",
    reconciled_path = "data/processed/pos_panel_reconciled.csv",
    openings_path = "data/interim/openings_clean.csv",
    closures_path = "data/interim/closures_clean.csv",
    raw_openings_path = "data/raw/updated_openings_august2025.csv",
    raw_closures_path = "data/raw/updated_closures_august2025.csv",
    ruca_path = "data/raw/RUCA2010zipcode.xlsx",
    crosswalk_path = "data/raw/ZipHsaHrr.csv",
    zip_zcta_path = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
    census_root = "data/raw/census_raw_data",
    out_dir = "outputs/manuscript") {
  source("cleaning/00_shared_urbanicity_helpers.R", local = TRUE)
  years <- 2010:2023
  primary_panel <- "reconciled"
  primary_classification <- "hsa_population_weighted"
  groups <- c("Urban", "Rural & Small Town", "Unmatched")
  methods <- c("hospital_zip", "hsa_population_weighted")
  unique_keys <- function(dat, keys, label) {
    if (anyNA(dat[keys]) || anyDuplicated(dat[keys])) {
      stop(label, ": missing or duplicate keys (", paste(keys, collapse = ", "), ").")
    }
  }
  read_input <- function(path) read_csv(
    path, col_types = cols(ccn = col_character(), zip5 = col_character()),
    show_col_types = FALSE, progress = FALSE, name_repair = "unique_quiet"
  )
  read_panel <- function(path) {
    dat <- read_input(path) %>% filter(year %in% years)
    unique_keys(dat, c("ccn", "year"), path)
    if (anyNA(dat$active) || !all(dat$active %in% 0:1)) stop("Invalid active flag: ", path)
    expected_active <- as.integer(!is.na(dat$part_year) & dat$year > dat$part_year &
                                   (is.na(dat$term_year) | dat$year < dat$term_year))
    if (!identical(as.integer(dat$active), expected_active)) stop("Activity formula changed: ", path)
    if (!all(dat$hosptype[dat$active == 1] %in% c(1, 11))) stop("Unexpected active hospital type: ", path)
    dat
  }
  panels <- list(updated = read_panel(updated_path), reconciled = read_panel(reconciled_path))
  if (!setequal(paste(panels$updated$ccn, panels$updated$year),
                paste(panels$reconciled$ccn, panels$reconciled$year))) {
    stop("Updated and reconciled panels have different hospital-year keys; rebuild upstream data.")
  }
  read_events <- function(open_path, close_path) {
    events <- bind_rows(
      read_input(open_path) %>% transmute(ccn, zip5, event_year = part_year, event_type = "Opening"),
      read_input(close_path) %>% transmute(ccn, zip5, event_year = term_year, event_type = "Closure")
    ) %>% filter(event_year >= min(years))
    # Figure 1 currently has no upper-year filter: fail if it no longer matches this window.
    if (any(!events$event_year %in% years)) stop("Events extend beyond 2010-2023; review the manuscript window.")
    unique_keys(events, c("ccn", "event_type"), "Curated events")
    events
  }
  events <- read_events(raw_openings_path, raw_closures_path)
  cleaned_events <- read_events(openings_path, closures_path)
  if (nrow(anti_join(events, cleaned_events, by = names(events))) ||
      nrow(anti_join(cleaned_events, events, by = names(events)))) {
    stop("Raw Figure 1 events and cleaned events disagree; rebuild upstream data.")
  }
  # Refuse an unreconciled or stale input labeled as the adopted panel.
  closure_lookup <- read_input(closures_path) %>%
    distinct(ccn, curated_term_year = term_year)
  unique_keys(closure_lookup, "ccn", "Closure reconciliation lookup")
  # Reconciliation guards use every source year, including the 2009 lag year.
  expected <- read_input(updated_path) %>% left_join(closure_lookup, by = "ccn")
  conflict_ccns <- expected %>%
    filter(!is.na(curated_term_year), !is.na(term_year), term_year != curated_term_year) %>%
    pull(ccn) %>% unique()
  expected <- expected %>% mutate(
    term_year = if_else(!ccn %in% conflict_ccns & is.na(term_year) & !is.na(curated_term_year),
                        curated_term_year, term_year),
    active = as.integer(!is.na(part_year) & year > part_year &
                          (is.na(term_year) | year < term_year))
  ) %>% filter(year %in% years) %>% arrange(ccn, year)
  actual <- panels$reconciled %>% arrange(ccn, year)
  if (!isTRUE(all.equal(expected$term_year, actual$term_year, check.attributes = FALSE)) ||
      !identical(as.integer(expected$active), as.integer(actual$active))) {
    stop("Reconciled panel does not match current curated-closure reconciliation; rebuild it before reporting.")
  }
  ruca <- ocgh_load_ruca_lookup(ruca_path) %>% select(zip5, geography_type)
  zip_hsa <- ocgh_load_zip_hsa_lookup(crosswalk_path)
  unique_keys(zip_hsa, "zip5", "ZIP-HSA crosswalk")
  hsa <- ocgh_build_hsa_year_ruca_assignment(
    years, crosswalk_file = crosswalk_path, ruca_file = ruca_path,
    method = "hsa_population_weighted", zip_zcta_file = zip_zcta_path,
    census_root = census_root
  ) %>% select(hsanum, year, geography_type)
  unique_keys(hsa, c("hsanum", "year"), "HSA urbanicity")
  classify <- function(dat, method) {
    if (method == "hospital_zip") {
      dat <- left_join(dat, ruca, by = "zip5")
    } else {
      dat <- dat %>% left_join(zip_hsa, by = "zip5") %>%
        left_join(hsa, by = c("hsanum", "year"))
    }
    dat %>% mutate(geography_type = coalesce(geography_type, "Unmatched"))
  }
  event_geographies <- bind_rows(lapply(methods, function(method) {
    events %>% rename(year = event_year) %>% classify(method) %>%
      mutate(classification = method)
  }))
  event_counts <- event_geographies %>% count(classification, event_type, geography_type, name = "events") %>%
    complete(classification = methods, event_type = c("Opening", "Closure"),
             geography_type = groups, fill = list(events = 0L))
  event_counts_by_year <- event_geographies %>%
    count(classification, year, event_type, geography_type, name = "events") %>%
    complete(classification = methods, year = years, event_type = c("Opening", "Closure"),
             geography_type = groups, fill = list(events = 0L))
  sample_counts <- bind_rows(lapply(names(panels), function(label) {
    dat <- panels[[label]] %>% filter(active == 1)
    tibble(panel = label, unique_active_2010_2023 = n_distinct(dat$ccn),
           active_2010 = n_distinct(dat$ccn[dat$year == 2010]),
           active_2023 = n_distinct(dat$ccn[dat$year == 2023]),
           opening_ccns_outside_active_sample = sum(!events$ccn[events$event_type == "Opening"] %in% dat$ccn),
           closure_ccns_outside_active_sample = sum(!events$ccn[events$event_type == "Closure"] %in% dat$ccn),
           union_active_and_curated_event_ccns = length(union(dat$ccn, events$ccn)))
  }))
  annual_counts <- bind_rows(lapply(names(panels), function(label) {
    panels[[label]] %>% filter(active == 1) %>%
      group_by(year) %>% summarise(active_hospitals = n_distinct(ccn), .groups = "drop") %>%
      mutate(panel = label, .before = 1)
  }))
  closure_years <- events %>% filter(event_type == "Closure") %>% select(ccn, closure_year = event_year)
  cohort_members <- bind_rows(lapply(names(panels), function(label) {
    baseline <- panels[[label]] %>% filter(active == 1, year == 2010) %>% select(ccn, zip5, year)
    bind_rows(lapply(methods, function(method) {
      classify(baseline, method) %>% left_join(closure_years, by = "ccn") %>%
        mutate(panel = label, classification = method, closed = !is.na(closure_year))
    }))
  }))
  baseline_rates <- cohort_members %>% group_by(panel, classification, geography_type) %>%
    summarise(baseline_hospitals = n(), baseline_cohort_closures = sum(closed), .groups = "drop") %>%
    complete(panel = names(panels), classification = methods, geography_type = groups,
             fill = list(baseline_hospitals = 0L, baseline_cohort_closures = 0L)) %>%
    left_join(event_counts %>% filter(event_type == "Opening") %>%
                select(classification, geography_type, all_openings = events),
              by = c("classification", "geography_type")) %>%
    left_join(event_counts %>% filter(event_type == "Closure") %>%
                select(classification, geography_type, all_closures = events),
              by = c("classification", "geography_type")) %>%
    mutate(closure_pct_baseline_cohort = if_else(baseline_hospitals > 0,
             100 * baseline_cohort_closures / baseline_hospitals, NA_real_),
           openings_pct_baseline = if_else(baseline_hospitals > 0,
             100 * all_openings / baseline_hospitals, NA_real_),
           all_closures_pct_baseline = if_else(baseline_hospitals > 0,
             100 * all_closures / baseline_hospitals, NA_real_))
  sample_counts <- sample_counts %>% mutate(manuscript_primary = panel == primary_panel)
  annual_counts <- annual_counts %>% mutate(manuscript_primary = panel == primary_panel)
  event_counts <- event_counts %>% mutate(manuscript_primary = classification == primary_classification)
  baseline_rates <- baseline_rates %>% mutate(
    manuscript_primary = panel == primary_panel & classification == primary_classification)
  # Every classified baseline numerator must be a subset of its own denominator.
  stopifnot(all(baseline_rates$baseline_cohort_closures <= baseline_rates$baseline_hospitals))
  census_paths <- c(file.path(census_root, "DECENNIALSF12010", "DECENNIALSF12010.P1-Data.csv"),
    file.path(census_root, "B01003", paste0("B01003_", 2011:2023),
              paste0("ACSDT5Y", 2011:2023, ".B01003-Data.csv")))
  input_paths <- c(updated_path, reconciled_path, openings_path, closures_path,
                   raw_openings_path, raw_closures_path, ruca_path, crosswalk_path, zip_zcta_path,
                   census_paths, "cleaning/00_shared_urbanicity_helpers.R", "cleaning/02_clean_pos.R",
                   "cleaning/02b_reconcile_pos_panel.R", "cleaning/07_stage_urban_rural_activity.R",
                   "analysis/manuscript_sample_claims.R")
  provenance <- tibble(path = input_paths, md5 = unname(tools::md5sum(input_paths)))
  if (anyNA(provenance$md5)) stop("Missing provenance input.")
  fmt <- function(x) formatC(x, format = "d", big.mark = ",")
  sample_for <- function(panel) sample_counts[sample_counts$panel == panel, ]
  rates_for <- function(panel, method, group) baseline_rates[
    baseline_rates$panel == panel & baseline_rates$classification == method &
      baseline_rates$geography_type == group, ]
  events_for <- function(method, type, group = groups) sum(event_counts$events[
    event_counts$classification == method & event_counts$event_type == type &
      event_counts$geography_type %in% group])
  updated <- sample_for("updated")
  reconciled <- sample_for("reconciled")
  zip_urban <- rates_for("updated", "hospital_zip", "Urban")
  zip_rural <- rates_for("updated", "hospital_zip", "Rural & Small Town")
  zip_unknown <- rates_for("updated", "hospital_zip", "Unmatched")
  claim <- function(label, cited, observed, definition, digits = 0L) tibble(
    claim = label, manuscript_value = cited, reproduced_value = observed,
    recommended_value = round(observed, digits),
    matches_at_reported_precision = round(observed, digits) == cited,
    definition_tested = definition
  )
  draft_claim_checks <- bind_rows(
    claim("Unique active hospitals", 4716, updated$unique_active_2010_2023, "Updated POS, active in at least one year"),
    claim("Openings", 175, events_for("hospital_zip", "Opening"), "All curated 2010-2023 openings"),
    claim("Closures", 320, events_for("hospital_zip", "Closure"), "All curated 2010-2023 closures"),
    claim("Urban openings (ZIP)", 160, zip_urban$all_openings, "Hospital ZIP RUCA"),
    claim("Rural openings (ZIP)", 15, zip_rural$all_openings, "Hospital ZIP RUCA"),
    claim("Urban closures (ZIP)", 221, zip_urban$all_closures, "Hospital ZIP RUCA"),
    claim("Rural closures (ZIP)", 99, zip_rural$all_closures, "Hospital ZIP RUCA"),
    claim("Urban openings (Figure 1)", 160, events_for("hsa_population_weighted", "Opening", "Urban"), "Event-year population-weighted HSA"),
    claim("Rural openings (Figure 1)", 15, events_for("hsa_population_weighted", "Opening", "Rural & Small Town"), "Event-year population-weighted HSA"),
    claim("Urban closures (Figure 1)", 221, events_for("hsa_population_weighted", "Closure", "Urban"), "Event-year population-weighted HSA"),
    claim("Rural closures (Figure 1)", 99, events_for("hsa_population_weighted", "Closure", "Rural & Small Town"), "Event-year population-weighted HSA"),
    claim("Baseline urban hospitals", 2688, zip_urban$baseline_hospitals, "Updated POS, 2010 ZIP RUCA"),
    claim("Baseline rural hospitals", 1160, zip_rural$baseline_hospitals, "Updated POS, 2010 ZIP RUCA"),
    claim("Baseline urban hospitals closing", 220, zip_urban$baseline_cohort_closures, "CCNs active in 2010 joined to curated closures; baseline ZIP RUCA"),
    claim("Baseline rural hospitals closing", 99, zip_rural$baseline_cohort_closures, "CCNs active in 2010 joined to curated closures; baseline ZIP RUCA"),
    claim("Baseline urban closure percentage", 8.2, zip_urban$closure_pct_baseline_cohort, "Baseline urban hospitals closing / baseline urban hospitals", 1L),
    claim("Baseline rural closure percentage", 8.5, zip_rural$closure_pct_baseline_cohort, "Baseline rural hospitals closing / baseline rural hospitals", 1L),
    claim("Urban openings percentage", 6.0, zip_urban$openings_pct_baseline, "All ZIP-urban openings / ZIP-urban baseline hospitals", 1L),
    claim("Rural openings percentage", 1.3, zip_rural$openings_pct_baseline, "All ZIP-rural openings / ZIP-rural baseline hospitals", 1L)
  )
  adopted_urban <- rates_for(primary_panel, primary_classification, "Urban")
  adopted_rural <- rates_for(primary_panel, primary_classification, "Rural & Small Town")
  adopted_unknown <- rates_for(primary_panel, primary_classification, "Unmatched")
  claim_checks <- bind_rows(
    claim("Unique active hospitals, 2010-2023", 4716, reconciled$unique_active_2010_2023,
          "Distinct CCNs active in at least one eligible annual observation in reconciled POS"),
    claim("X: active hospitals in 2010", NA_real_, reconciled$active_2010, "Distinct active CCNs in reconciled POS in 2010"),
    claim("Y: active hospitals in 2023", NA_real_, reconciled$active_2023, "Distinct active CCNs in reconciled POS in 2023"),
    claim("All curated openings", 175, events_for(primary_classification, "Opening"), "Curated opening CCNs in 2010-2023"),
    claim("Urban openings", 160, adopted_urban$all_openings, "Event-year population-weighted HSA urbanicity"),
    claim("Rural/small-town openings", 15, adopted_rural$all_openings, "Event-year population-weighted HSA urbanicity"),
    claim("All curated closures", 320, events_for(primary_classification, "Closure"), "Curated closure CCNs in 2010-2023"),
    claim("Urban closures", 221, adopted_urban$all_closures, "Event-year population-weighted HSA urbanicity"),
    claim("Rural/small-town closures", 99, adopted_rural$all_closures, "Event-year population-weighted HSA urbanicity"),
    claim("Urban baseline hospitals", 2688, adopted_urban$baseline_hospitals, "Reconciled active CCNs in 2010; 2010 population-weighted HSA urbanicity"),
    claim("Rural/small-town baseline hospitals", 1160, adopted_rural$baseline_hospitals, "Reconciled active CCNs in 2010; 2010 population-weighted HSA urbanicity"),
    claim("Baseline hospitals with unclassified urbanicity", NA_real_, adopted_unknown$baseline_hospitals, "Included in X, excluded from urban/rural comparisons"),
    claim("Urban baseline hospitals closing", 220, adopted_urban$baseline_cohort_closures, "CCNs active in 2010 intersected with curated 2010-2023 closures; geography fixed in 2010"),
    claim("Rural/small-town baseline hospitals closing", 99, adopted_rural$baseline_cohort_closures, "CCNs active in 2010 intersected with curated 2010-2023 closures; geography fixed in 2010"),
    claim("Urban baseline closure percentage", 8.2, adopted_urban$closure_pct_baseline_cohort, "Urban baseline hospitals closing / urban baseline hospitals", 1L),
    claim("Rural/small-town baseline closure percentage", 8.5, adopted_rural$closure_pct_baseline_cohort, "Rural/small-town baseline hospitals closing / rural/small-town baseline hospitals", 1L),
    claim("Urban openings percentage of baseline", 6.0, adopted_urban$openings_pct_baseline, "All urban opening events / urban baseline hospitals", 1L),
    claim("Rural/small-town openings percentage of baseline", 1.3, adopted_rural$openings_pct_baseline, "All rural/small-town opening events / rural/small-town baseline hospitals", 1L)
  ) %>% mutate(panel = primary_panel, classification = primary_classification)
  markdown_table <- function(dat) {
    dat[] <- lapply(dat, function(x) if (is.numeric(x)) format(x, trim = TRUE, digits = 7) else x)
    c(paste0("| ", paste(names(dat), collapse = " | "), " |"),
      paste0("| ", paste(rep("---", ncol(dat)), collapse = " | "), " |"),
      apply(dat, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")))
  }
  paragraph <- function(panel, method) {
    sample <- sample_counts[sample_counts$panel == panel, ]
    rates <- baseline_rates[baseline_rates$panel == panel & baseline_rates$classification == method, ]
    urban <- rates[rates$geography_type == "Urban", ]
    rural <- rates[rates$geography_type == "Rural & Small Town", ]
    unknown <- rates[rates$geography_type == "Unmatched", ]
    counts <- event_counts[event_counts$classification == method, ]
    count_event <- function(type, group = NULL) sum(counts$events[counts$event_type == type &
      (is.null(group) | counts$geography_type %in% group)])
    geo <- if (method == "hospital_zip") "hospital ZIP RUCA classification" else "population-weighted HSA classification"
    panel_description <- if (panel == "reconciled") "The closure-reconciled Provider of Services panel" else
      "The original cleaned Provider of Services panel"
    event_description <- if (method == primary_classification) "Figure 1 displays the separately curated event series, which recorded " else
      "Separately, the curated event series recorded "
    c(paste0(panel_description, " included ", fmt(sample$unique_active_2010_2023),
       " unique hospital identifiers (CMS Certification Numbers) for short-term general acute care and critical access hospitals classified as active in at least one annual observation during 2010-2023, including ",
       fmt(sample$active_2010), " in 2010 and ", fmt(sample$active_2023),
       " in 2023. ", event_description, count_event("Opening"),
       " openings (", count_event("Opening", "Urban"), " urban and ", count_event("Opening", "Rural & Small Town"),
       " rural/small-town) and ", count_event("Closure"), " closures (", count_event("Closure", "Urban"),
       " urban and ", count_event("Closure", "Rural & Small Town"), " rural/small-town), using ", geo, "."),
      sprintf(paste0("Among hospitals classified as active in 2010, %.1f%% of hospitals in urban HSAs (%s/%s) and %.1f%% in rural/small-town HSAs (%s/%s) appeared in the curated closure series during 2010-2023. Urbanicity for this cohort comparison was fixed at baseline. Over the study period, openings classified by event-year HSA urbanicity were equivalent to %.1f%% (%s/%s) of the urban baseline hospital count and %.1f%% (%s/%s) of the rural/small-town baseline count. These geographic comparisons excluded %s baseline hospitals with unclassified urbanicity."),
        urban$closure_pct_baseline_cohort, fmt(urban$baseline_cohort_closures), fmt(urban$baseline_hospitals),
        rural$closure_pct_baseline_cohort, fmt(rural$baseline_cohort_closures), fmt(rural$baseline_hospitals),
        urban$openings_pct_baseline, fmt(urban$all_openings), fmt(urban$baseline_hospitals),
        rural$openings_pct_baseline, fmt(rural$all_openings), fmt(rural$baseline_hospitals), fmt(unknown$baseline_hospitals)))
  }
  adopted_paragraphs <- paragraph(primary_panel, primary_classification)
  methods_text <- c(
    "Hospitals were identified by CMS Certification Number and included in the active POS cohort if they had at least one eligible annual observation classified as active during 2010-2023. Missing POS termination years were reconciled to the curated closure year before activity was determined. Observations were classified as active only after the participation year and before a known termination year; records with no known termination year remained eligible after participation. Opening and closure events were analyzed separately using the curated event lists.",
    "For the main geographic comparisons, each HSA-year was classified as urban or rural/small-town by comparing the pooled population of its metropolitan and micropolitan ZIP codes (RUCA 1-6) with that of its small-town and rural ZIP codes (RUCA 7-10), with the manual HSA overrides used in the figure analyses. Baseline-cohort closure comparisons used 2010 HSA urbanicity, whereas all-event counts used event-year HSA urbanicity. Hospital-ZIP RUCA, when reported as a hospital-location characteristic, was labeled separately."
  )
  lines <- c(
    "# Manuscript claims: hospital sample, activity, and baseline comparisons", "",
    "Generated from the local study inputs. Full-precision counts and input MD5 hashes accompany this report. No analytical inputs or figures are changed by this audit.", "",
    "## Adopted manuscript definition", "",
    paste0("Primary POS input: `", reconciled_path, "`. Primary urbanicity: population-weighted HSA classification, matching Figure 1. The original POS panel and hospital-ZIP classification below are historical comparisons, not alternative manuscript defaults."), "",
    "## Manuscript replacement text", "", adopted_paragraphs[1], "", adopted_paragraphs[2], "",
    "## Methods replacement text", "", methods_text[1], "", methods_text[2], "",
    "## Adopted numbers and changes from the supplied draft", "",
    markdown_table(claim_checks %>% select(claim, manuscript_value, recommended_value, definition_tested)), "",
    "`manuscript_sample_claim_checks.csv` retains full-precision values as well as manuscript-rounded values. NA draft values represent the original X/Y placeholders or previously unreported quantities.", "",
    "## Sample comparison for the audit trail", "", markdown_table(sample_counts), "",
    sprintf("The updated (pre-reconciliation) POS panel yields %s unique active CCNs: X = %s, Y = %s. The closure-reconciled panel corrects missing termination years and yields %s unique active CCNs: X = %s, Y = %s. These are separate coherent sets of values; do not mix their denominators.",
      fmt(updated$unique_active_2010_2023), fmt(updated$active_2010), fmt(updated$active_2023),
      fmt(reconciled$unique_active_2010_2023), fmt(reconciled$active_2010), fmt(reconciled$active_2023)), "",
    "## Definitions", "",
    "- A hospital is a distinct CMS Certification Number (CCN), not an independently deduplicated physical facility. The period count requires at least one eligible active POS observation in 2010-2023. X and Y require an active observation in 2010 and 2023, respectively. No bed-availability or HSA-match restriction is added.",
    "- POS cleaning excludes specialty hospitals except manually retained names, ownership/control code 10 (tribal; code 3, other, is retained), and manually excluded hospital names. All active observations in these inputs are type 1 (short-term) or 11 (critical access). The 2009 lag-source records are excluded from all counts here.",
    "- Active means a nonmissing participation year, observation year > participation year, and either a missing termination year or observation year < termination year. This excludes participation years and known termination years; it does not establish 365 days of operation or count every hospital operating for any part of a calendar year.",
    "- The reconciled panel fills missing termination years from curated closures before recomputing active. The original updated panel can therefore count closure-year or later observations as active when their termination year is missing.",
    "- Hospital ZIP urbanicity: RUCA 2010 primary codes 1-6 are urban (metropolitan plus micropolitan); codes 7-10 are rural/small-town. Baseline hospitals use their 2010 POS ZIP; events use their curated event ZIP.",
    "- Figure 1 currently uses population-weighted HSA urbanicity from the shared helper, not hospital ZIP urbanicity. Each HSA-year pools metropolitan and micropolitan ZIP populations as urban versus rural/small-town populations; the larger pool determines the class. Weights use the 2010 Decennial Census and 2011-2023 ACS population data; tie rules and manual HSA overrides are inherited from the figure's helper.",
    "- Baseline-cohort closures join curated 2010-2023 closures to CCNs active in 2010, retaining each hospital's 2010 geography. All-event counts instead classify events in their event year and include hospitals outside the baseline cohort. Opening percentages divide all curated openings by the corresponding baseline hospital count; they are cumulative event-to-baseline ratios, not probabilities that baseline hospitals open or annual incidence rates.",
    "- The active POS cohort and curated event series have different inclusion rules. Counts outside the active sample and their union are reported above; do not assume that the active POS sample includes every Figure 1 event hospital.", "",
    "## Event counts: draft ZIP definition versus current Figure 1", "", markdown_table(event_counts), "",
    sprintf("Hospital ZIP urbanicity yields %s/%s urban/rural openings and %s/%s urban/rural closures. Current Figure 1 HSA urbanicity yields %s/%s urban/rural openings and %s/%s urban/rural closures. Figure 1 excludes %s events lacking HSA classification; the full totals here retain them in the Unmatched category.",
      zip_urban$all_openings, zip_rural$all_openings, zip_urban$all_closures, zip_rural$all_closures,
      events_for("hsa_population_weighted", "Opening", "Urban"), events_for("hsa_population_weighted", "Opening", "Rural & Small Town"),
      events_for("hsa_population_weighted", "Closure", "Urban"), events_for("hsa_population_weighted", "Closure", "Rural & Small Town"),
      events_for("hsa_population_weighted", "Opening", "Unmatched") + events_for("hsa_population_weighted", "Closure", "Unmatched")), "",
    "## Baseline denominators and closure numerators", "", markdown_table(baseline_rates), "",
    sprintf("Under the updated-panel/hospital-ZIP definitions, %s/%s (%.1f%%) urban and %s/%s (%.1f%%) rural baseline hospitals closed. Additional baseline closures with unmatched geography: %s. The geographic denominators omit %s baseline hospitals with unmatched RUCA codes. Dividing all %s urban and %s rural closures by the baseline populations instead gives %.1f%%/%.1f%%, but these all-event ratios do not measure the share of baseline hospitals that closed. The draft's written urban numerator of 220 should also be checked against the reproduced all-event and baseline-cohort counts rather than inferred from a rounded percentage.",
      fmt(zip_urban$baseline_cohort_closures), fmt(zip_urban$baseline_hospitals), zip_urban$closure_pct_baseline_cohort,
      fmt(zip_rural$baseline_cohort_closures), fmt(zip_rural$baseline_hospitals), zip_rural$closure_pct_baseline_cohort,
      zip_unknown$baseline_cohort_closures, zip_unknown$baseline_hospitals,
      zip_urban$all_closures, zip_rural$all_closures,
      zip_urban$all_closures_pct_baseline, zip_rural$all_closures_pct_baseline), "",
    "## Figure and table implications", "",
    "The current HSA bed-supply inputs already use the reconciled panel, so adopting it for the manuscript sample does not itself change current main Figures 2-4 or supplementary Figures A2-A3. Figure 1 and Figure A1 use curated event lists; Figures A4-A5 use population-change measures. Those event counts and measures are not changed by the POS activity correction. The referenced historical figure assets must still be checked against the current outputs before submission.", "",
    "The main runner and standalone Table 1 generator use the reconciled panel for non-event observations and regenerated tests, including the sensitivity versions. The accompanying hospital-claims audit reproduces both sets of descriptive values and checks standard test evidence against the corrected panel and source hashes. The pipeline then builds figures.tex/figures.pdf and appendix.tex/appendix.pdf from current outputs. 'Similar' closure proportions remain descriptive; no equivalence test was performed here.", "",
    "## Historical draft checks", "", markdown_table(draft_claim_checks), "",
    "This historical check explains which original numbers came from ZIP classification and the original POS panel. Use the adopted values at the top of this report for manuscript updates.", "",
    "## Recreate", "", "```sh",
    "Rscript -e \"source('analysis/manuscript_sample_claims.R'); run_manuscript_sample_claims()\"", "```", "",
    "The master `analysis/manuscript_claims.R` also exposes `run_all_manuscript_claims()` for this report, the Figure 4 urban violin report, and the community and hospital-characteristic audit CSVs. The full pipeline runs these after manuscript staging. Rebuild upstream data with `Rscript run_project.R` after raw input changes.", "",
    "Companion CSVs: `manuscript_sample_counts.csv`, `manuscript_sample_claim_checks.csv` (adopted values), `manuscript_draft_claim_checks.csv` (historical comparisons), `manuscript_active_by_year.csv`, `manuscript_event_counts.csv`, `manuscript_events_by_year.csv`, `manuscript_baseline_rates.csv`, `manuscript_baseline_cohort.csv` (CCN-level audit), and `manuscript_sample_inputs.csv` (source hashes). `manuscript_sample_replacement_text.txt` contains only the adopted methods and results paragraphs. All outputs remain under the repository's ignored outputs directory."
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  outputs <- list(sample_counts = sample_counts, sample_claim_checks = claim_checks,
                  draft_claim_checks = draft_claim_checks, active_by_year = annual_counts,
                  event_counts = event_counts, events_by_year = event_counts_by_year,
                  baseline_rates = baseline_rates, baseline_cohort = cohort_members, sample_inputs = provenance)
  for (name in names(outputs)) write_csv(outputs[[name]], file.path(out_dir, paste0("manuscript_", name, ".csv")))
  writeLines(lines, file.path(out_dir, "manuscript_sample_claims.md"))
  writeLines(c("METHODS", methods_text[1], "", methods_text[2], "", "RESULTS",
               adopted_paragraphs[1], "", adopted_paragraphs[2]),
             file.path(out_dir, "manuscript_sample_replacement_text.txt"))
  message("Wrote hospital sample and event claims to ", out_dir)
  invisible(outputs)
}
