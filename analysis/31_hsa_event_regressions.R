# ==============================================================================
# 31_hsa_event_regressions.R
#
# Community poverty and hospital entry/exit, 2012-2023.
# See estimand_memo_hsa_event_regressions.md for the estimand.
#
# ------------------------------------------------------------------------------
# WHY BOTH A LOGISTIC AND A POISSON SPECIFICATION
#
# The staged file (opening_closure_nonevent_percentiles.csv) uses
# distinct(hsanum, event_year) when building its event tables, so it carries
# BINARY any-event indicators. Two closures in one HSA-year look identical to
# one. That supports a logistic model but not a count model, which is why event
# counts are re-derived here from the raw event files.
#
# The Poisson specification is preferable on four grounds, and is treated as
# primary when the raw event files are supplied:
#
#   1. It uses the multiplicity the binary indicator discards. Multi-event
#      HSA-years are concentrated in large HSAs, so collapsing them is a
#      non-random loss that correlates with the at-risk denominator itself.
#   2. The offset constrains the denominator coefficient to 1, making the
#      estimand an explicit RATE (closures per hospital at risk, openings per
#      1,000 residents). The logistic model instead estimates a free
#      coefficient on log(hospitals), which is more flexible but leaves the
#      denominator entangled with the covariates.
#   3. Incidence rate ratios answer Ateev's net-impact question directly: a
#      fitted rate multiplied by mean beds per event gives implied net bed
#      change per stratum. Odds ratios do not map onto that quantity.
#   4. Poisson quasi-MLE with cluster-robust standard errors is consistent for
#      the conditional mean without requiring equidispersion, so the usual
#      objection to Poisson on non-count-like data does not apply here.
#
# Logistic is retained because events are rare (roughly 1% of HSA-years), so
# ORs and RRs are close in magnitude, and because "did this community
# experience a closure" is the framing most readers will arrive with. Reporting
# both, with agreement between them, is stronger than either alone.
#
# If path_openings/path_closures are left NULL, only the logistic models run
# and the audit log records that the Poisson models were skipped.
# ------------------------------------------------------------------------------
#
# For each outcome (closure, opening), fits:
#   M1  total association  : poverty | urbanicity, region, year, at-risk term
#   M2  direct association : M1 + certbeds per 1,000 at t-1   <- David's control
#   M3  poverty x urbanicity interaction, on the adjusted model
#
# Cluster-robust SEs by HSA throughout. LPM reported as sensitivity.
#
# Writes:
#   event_regressions_main.csv         Poisson IRRs + logistic ORs
#   event_regressions_sensitivity.csv  LPM risk differences per 1,000 HSA-years
#   event_rates_descriptive.csv        crude rates by poverty quintile x urbanicity
#   analysis_sample_audit.csv          sample construction and attrition
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(sandwich)
  library(lmtest)
})

# ------------------------------------------------------------------------------
# CONFIG
#
# NOTE: `urbanicity` and `poverty` are named against the conventions in script
# 06. `path_geo` is optional and supplies census region; if left NULL the models
# run without regional fixed effects and say so in the audit log.
# ------------------------------------------------------------------------------

.oc_cols <- list(
  hsa        = "hsanum",
  year       = "year",
  opening    = "opening",
  closure    = "closure",
  poverty    = "weighted_percent_below_poverty_line_event",
  pop        = "sum_total_pop_event",
  beds_lag   = "certbeds_per_1000_residents_lag1",
  facilities = "n_facilities",
  urbanicity = "geography_type"
)

# ------------------------------------------------------------------------------
# Cluster-robust tidy output
# ------------------------------------------------------------------------------

# NOTE: the fitted object is named `fit`, not `model`. Inside dplyr::mutate()
# the name `model` would resolve to the `model` COLUMN of `out` (a character
# label) rather than to the function argument, and nobs() would fail.
tidy_cluster <- function(fit, cluster_var, exponentiate = TRUE,
                         model_label = NA_character_) {
  vc <- sandwich::vcovCL(fit, cluster = cluster_var, type = "HC1")
  ct <- lmtest::coeftest(fit, vcov. = vc)
  ci <- lmtest::coefci(fit, vcov. = vc, level = 0.95)
  
  # Computed before the tibble is built so that data masking cannot reach them.
  n_obs_val      <- stats::nobs(fit)
  n_clusters_val <- dplyr::n_distinct(cluster_var)
  
  out <- tibble::tibble(
    model     = model_label,
    term      = rownames(ct),
    estimate  = ct[, 1],
    std.error = ct[, 2],
    statistic = ct[, 3],
    p.value   = ct[, 4],
    conf.low  = ci[, 1],
    conf.high = ci[, 2]
  )
  
  if (exponentiate) {
    out <- dplyr::mutate(out,
                         estimate  = exp(.data$estimate),
                         conf.low  = exp(.data$conf.low),
                         conf.high = exp(.data$conf.high)
    )
  }
  
  out$n_obs      <- n_obs_val
  out$n_clusters <- n_clusters_val
  out
}

# ------------------------------------------------------------------------------
# Collapse the event-combination file to one row per HSA-year
# ------------------------------------------------------------------------------

collapse_to_hsa_year <- function(df, cols = .oc_cols, log_fn = message) {
  
  missing <- setdiff(unname(unlist(cols)), names(df))
  if (length(missing) > 0) {
    stop("Panel source is missing expected column(s): ",
         paste(missing, collapse = ", "),
         "\n  Edit .oc_cols to match the actual header.", call. = FALSE)
  }
  
  d <- df %>%
    dplyr::rename(
      hsa        = !!cols$hsa,
      yr         = !!cols$year,
      opening    = !!cols$opening,
      closure    = !!cols$closure,
      poverty    = !!cols$poverty,
      pop        = !!cols$pop,
      beds_lag   = !!cols$beds_lag,
      facilities = !!cols$facilities,
      urbanicity = !!cols$urbanicity
    ) %>%
    dplyr::mutate(hsa = as.integer(.data$hsa), yr = as.integer(.data$yr))
  
  # The 9 duplicated HSA-years should carry identical covariates on both rows,
  # since both came from the same join against ntl. Verify rather than assume.
  conflicts <- d %>%
    dplyr::group_by(.data$hsa, .data$yr) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::summarise(
      n_distinct_cov = dplyr::n_distinct(
        paste(.data$poverty, .data$pop, .data$beds_lag,
              .data$facilities, .data$urbanicity)
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_distinct_cov > 1)
  
  if (nrow(conflicts) > 0) {
    stop(nrow(conflicts), " duplicated HSA-year(s) carry conflicting covariate ",
         "values across their opening and closure rows.", call. = FALSE)
  }
  
  out <- d %>%
    dplyr::group_by(.data$hsa, .data$yr) %>%
    dplyr::summarise(
      # NA in `opening`/`closure` means "not this event type", not missing.
      any_opening = as.integer(any(!is.na(.data$opening) & .data$opening == 1)),
      any_closure = as.integer(any(!is.na(.data$closure) & .data$closure == 1)),
      poverty     = dplyr::first(.data$poverty),
      pop         = dplyr::first(.data$pop),
      beds_lag    = dplyr::first(.data$beds_lag),
      facilities  = dplyr::first(.data$facilities),
      urbanicity  = dplyr::first(.data$urbanicity),
      .groups = "drop"
    ) %>%
    dplyr::rename(year = .data$yr)
  
  log_fn(sprintf("Collapsed %d rows to %d HSA-years (%d dual-event HSA-years).",
                 nrow(d), nrow(out), nrow(d) - nrow(out)))
  out
}

# ------------------------------------------------------------------------------
# Re-derive event COUNTS from the raw event files
#
# Mirrors the aggregation in script 06 exactly, except that count() replaces
# distinct(). Any divergence between the binary indicators already in the panel
# and (count > 0) is a hard stop: it would mean the two paths disagree about
# which HSA-years had events, which invalidates both.
# ------------------------------------------------------------------------------

attach_event_counts <- function(panel,
                                path_openings,
                                path_closures,
                                path_crosswalk,
                                event_year_min = 2010,
                                log_row = function(...) NULL) {
  
  zip_hsa <- readr::read_csv(path_crosswalk, show_col_types = FALSE,
                             progress = FALSE) %>%
    dplyr::transmute(
      zip5 = stringr::str_pad(as.character(.data$zipcode19), width = 5,
                              side = "left", pad = "0"),
      hsa  = as.integer(.data$hsanum)
    ) %>%
    dplyr::distinct()
  
  count_events <- function(path, year_col, label) {
    raw <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    if (!all(c("zip5", year_col) %in% names(raw))) {
      stop("Event file '", label, "' must contain 'zip5' and '", year_col, "'.",
           call. = FALSE)
    }
    out <- raw %>%
      dplyr::transmute(
        zip5 = stringr::str_pad(as.character(.data$zip5), width = 5,
                                side = "left", pad = "0"),
        year = as.integer(.data[[year_col]])
      ) %>%
      dplyr::filter(!is.na(.data$year), .data$year >= event_year_min) %>%
      dplyr::left_join(zip_hsa, by = "zip5")
    
    log_row(paste0("events_unmatched_zip_", label), sum(is.na(out$hsa)),
            "events whose ZIP has no HSA in the crosswalk")
    
    out %>%
      dplyr::filter(!is.na(.data$hsa)) %>%
      dplyr::count(hsa, year, name = "n")
  }
  
  ope <- count_events(path_openings, "part_year", "openings") %>%
    dplyr::rename(n_openings = n)
  clo <- count_events(path_closures, "term_year", "closures") %>%
    dplyr::rename(n_closures = n)
  
  out <- panel %>%
    dplyr::left_join(ope, by = c("hsa", "year")) %>%
    dplyr::left_join(clo, by = c("hsa", "year")) %>%
    dplyr::mutate(
      # Absent from an event file means zero events, not missing.
      n_openings = tidyr::replace_na(.data$n_openings, 0L),
      n_closures = tidyr::replace_na(.data$n_closures, 0L)
    )
  
  # Consistency with the binary indicators already in the panel.
  bad_o <- sum(out$any_opening != as.integer(out$n_openings > 0))
  bad_c <- sum(out$any_closure != as.integer(out$n_closures > 0))
  log_row("count_binary_mismatch_openings", bad_o)
  log_row("count_binary_mismatch_closures", bad_c)
  if (bad_o + bad_c > 0) {
    stop("Re-derived event counts disagree with the binary indicators in the ",
         "staged panel (", bad_o, " openings, ", bad_c, " closures). The two ",
         "aggregations are not describing the same events.", call. = FALSE)
  }
  
  # How much multiplicity the binary indicator was discarding.
  log_row("hsa_years_multi_opening", sum(out$n_openings > 1))
  log_row("hsa_years_multi_closure", sum(out$n_closures > 1))
  log_row("total_openings", sum(out$n_openings))
  log_row("total_closures", sum(out$n_closures))
  
  out
}

# ------------------------------------------------------------------------------
# Build the t-1 facility count
#
# `n_facilities_lag1` from script 04 conflates two kinds of NA: an HSA-year with
# no active hospitals (structural zero) and the first year of an HSA's panel
# (lag boundary). The at-risk restriction needs those distinguished, so the lag
# is rebuilt here by an explicit year-1 join with a structural zero-fill.
#
# `beds_lag` is taken as supplied and left NA-preserving, matching the existing
# convention for that variable.
# ------------------------------------------------------------------------------

attach_facilities_lag <- function(panel) {
  lagged <- panel %>%
    dplyr::transmute(
      hsa,
      year = .data$year + 1L,
      facilities_tm1 = tidyr::replace_na(as.integer(.data$facilities), 0L)
    )
  dplyr::left_join(panel, lagged, by = c("hsa", "year"))
}

# ------------------------------------------------------------------------------
# Analysis sample
# ------------------------------------------------------------------------------

prepare_sample <- function(panel, outcome = c("closure", "opening"),
                           year_min = 2012, year_max = 2023,
                           poverty_per_pp = 5) {
  outcome <- match.arg(outcome)
  
  d <- panel %>%
    dplyr::filter(.data$year >= year_min, .data$year <= year_max) %>%
    dplyr::filter(
      !is.na(.data$poverty),
      !is.na(.data$pop), .data$pop > 0,
      !is.na(.data$beds_lag),
      !is.na(.data$urbanicity),
      !is.na(.data$facilities_tm1)
    )
  
  if (outcome == "closure") {
    # An HSA with no active hospital at t-1 cannot record a closure. Retaining
    # these rows would put structural zeros in the risk set, and log(0) in the
    # Poisson offset is undefined.
    d <- dplyr::filter(d, .data$facilities_tm1 >= 1)
    d$y       <- d$any_closure
    d$y_count <- if ("n_closures" %in% names(d)) d$n_closures else NA_integer_
    # Rate per hospital at risk.
    d$log_offset <- log(d$facilities_tm1)
  } else {
    d$y       <- d$any_opening
    d$y_count <- if ("n_openings" %in% names(d)) d$n_openings else NA_integer_
    # Rate per 1,000 residents. Zero-hospital HSAs stay in: they can still
    # gain a facility.
    d$log_offset <- log(d$pop / 1000)
  }
  
  d %>%
    dplyr::mutate(
      poverty_scaled = .data$poverty / poverty_per_pp,
      log_pop        = log(.data$pop),
      log_atrisk     = log(.data$facilities_tm1 + 1),
      year_f         = factor(.data$year),
      # Reference level is set explicitly rather than inherited from
      # alphabetical ordering. With `Urban` as the baseline, the poverty
      # coefficient is the poverty slope among urban HSAs and the M3
      # interaction is the rural deviation from that slope.
      urban_f        = factor(
        .data$urbanicity,
        levels = c("Urban", "Rural & Small Town")
      )
    )
}

# ------------------------------------------------------------------------------
# Model formulas
#
# Poisson: the at-risk denominator sits in the offset, so its coefficient is
# constrained to 1 and the estimand is an explicit rate.
# Logistic: no offset is available, so the denominator enters as a free
# covariate instead.
# ------------------------------------------------------------------------------

build_formula <- function(outcome, adjusted_for_beds, use_region,
                          model_type = c("poisson", "logit")) {
  
  model_type <- match.arg(model_type)
  
  if (model_type == "poisson") {
    lhs <- "y_count"
    rhs <- c("poverty_scaled", "urban_f", "year_f")
    # Closures: offset is log(hospitals at risk), so no separate supply term.
    # Openings: offset is log(population), so existing supply stays a covariate.
    if (outcome == "opening") rhs <- c(rhs, "log_atrisk")
    offset_term <- " + offset(log_offset)"
  } else {
    lhs <- "y"
    rhs <- c("poverty_scaled", "log_atrisk", "urban_f", "year_f")
    if (outcome == "opening") rhs <- c(rhs, "log_pop")
    offset_term <- ""
  }
  
  if (use_region)        rhs <- c(rhs, "region_f")
  if (adjusted_for_beds) rhs <- c(rhs, "beds_lag")
  
  stats::as.formula(paste0(lhs, " ~ ", paste(rhs, collapse = " + "),
                           offset_term))
}

# ------------------------------------------------------------------------------
# Main estimation
# ------------------------------------------------------------------------------

run_event_regressions <- function(
    path_panel     = "data/interim/opening_closure_nonevent_percentiles.csv",
    path_openings  = "data/raw/updated_openings_august2025.csv",
    path_closures  = "data/raw/updated_closures_august2025.csv",
    path_crosswalk = "data/raw/ZipHsaHrr.csv",
    path_geo       = NULL,   # optional CSV with hsanum + region for regional FE
    dir_out        = "output/tables",
    dir_checks     = "checks",
    year_min       = 2012,
    year_max       = 2023,
    poverty_per_pp = 5,
    verbose        = TRUE
) {
  
  dir.create(dir_out,    showWarnings = FALSE, recursive = TRUE)
  dir.create(dir_checks, showWarnings = FALSE, recursive = TRUE)
  
  audit <- list()
  log_row <- function(check, value, note = NA_character_) {
    audit[[length(audit) + 1]] <<- tibble::tibble(
      check = check, value = as.character(value), note = note
    )
    if (verbose) message(sprintf("  [%s] %s", check, value))
    invisible(NULL)
  }
  on.exit({
    if (length(audit) > 0) {
      readr::write_csv(dplyr::bind_rows(audit),
                       file.path(dir_checks, "analysis_sample_audit.csv"))
    }
  }, add = TRUE)
  
  raw <- readr::read_csv(path_panel, show_col_types = FALSE, progress = FALSE)
  log_row("rows_in_source", nrow(raw))
  
  panel <- collapse_to_hsa_year(
    raw, log_fn = if (verbose) message else function(...) NULL
  )
  log_row("hsa_years_after_collapse", nrow(panel))
  log_row("dual_event_hsa_years", nrow(raw) - nrow(panel))
  
  panel <- attach_facilities_lag(panel)
  
  # ---- event counts (enables the Poisson models) ----
  use_counts <- !is.null(path_openings) && !is.null(path_closures) &&
    !is.null(path_crosswalk)
  
  if (use_counts) {
    panel <- attach_event_counts(panel, path_openings, path_closures,
                                 path_crosswalk, log_row = log_row)
  } else {
    log_row("poisson_models", "SKIPPED",
            "raw event files not supplied; only binary logistic models run")
  }
  
  # ---- optional regional fixed effects ----
  use_region <- !is.null(path_geo)
  if (use_region) {
    geo <- readr::read_csv(path_geo, show_col_types = FALSE, progress = FALSE)
    if (!all(c("hsanum", "region") %in% names(geo))) {
      stop("path_geo must contain columns 'hsanum' and 'region'.", call. = FALSE)
    }
    geo <- dplyr::distinct(
      dplyr::transmute(geo, hsa = as.integer(.data$hsanum), region = .data$region)
    )
    if (any(duplicated(geo$hsa))) {
      stop("path_geo has non-unique HSAs.", call. = FALSE)
    }
    panel <- dplyr::left_join(panel, geo, by = "hsa")
    log_row("hsas_missing_region", sum(is.na(panel$region)))
    if (any(is.na(panel$region))) {
      stop("Some HSAs have no region assignment.", call. = FALSE)
    }
    panel$region_f <- factor(panel$region)
  } else {
    log_row("region_fixed_effects", "OMITTED",
            "path_geo was NULL; models run without regional adjustment")
  }
  
  main <- list(); sens <- list()
  
  for (outcome in c("closure", "opening")) {
    
    d <- prepare_sample(panel, outcome, year_min, year_max, poverty_per_pp)
    
    log_row(paste0("n_obs_", outcome), nrow(d))
    log_row(paste0("n_hsas_", outcome), dplyr::n_distinct(d$hsa))
    log_row(paste0("n_hsa_years_with_event_", outcome), sum(d$y))
    if (use_counts) {
      log_row(paste0("n_events_", outcome), sum(d$y_count))
      log_row(paste0("events_lost_to_binary_", outcome),
              sum(d$y_count) - sum(d$y),
              "extra events the binary indicator would discard")
    }
    log_row(paste0("event_rate_per_1000_hsa_years_", outcome),
            sprintf("%.2f", 1000 * mean(d$y)))
    
    if (sum(d$y) < 30) {
      log_row(paste0("WARNING_sparse_", outcome), sum(d$y),
              "fewer than 30 events; estimates will be unstable")
    }
    
    for (adj in c(FALSE, TRUE)) {
      
      suffix <- if (adj) "M2_direct" else "M1_total"
      
      # ---- primary: Poisson rate model, IRRs ----
      if (use_counts) {
        label_p <- sprintf("%s_%s_poisson", outcome, suffix)
        f_p <- build_formula(outcome, adj, use_region, "poisson")
        m_p <- stats::glm(f_p, data = d, family = stats::poisson(link = "log"))
        main[[label_p]] <- tidy_cluster(m_p, d$hsa, TRUE, label_p)
      }
      
      # ---- secondary: logistic on the binary indicator, ORs ----
      label_l <- sprintf("%s_%s_logit", outcome, suffix)
      f_l <- build_formula(outcome, adj, use_region, "logit")
      m_l <- stats::glm(f_l, data = d, family = stats::binomial(link = "logit"))
      main[[label_l]] <- tidy_cluster(m_l, d$hsa, TRUE, label_l)
      
      # ---- sensitivity: LPM, risk differences per 1,000 HSA-years ----
      m_lpm <- stats::lm(f_l, data = d)
      sens[[paste0(label_l, "_lpm")]] <-
        tidy_cluster(m_lpm, d$hsa, FALSE, sprintf("%s_%s_lpm", outcome, suffix)) %>%
        dplyr::mutate(dplyr::across(
          c("estimate", "std.error", "conf.low", "conf.high"), ~ .x * 1000
        ))
    }
    
    # ---- M3: poverty x urbanicity, on the beds-adjusted model ----
    int_type <- if (use_counts) "poisson" else "logit"
    f_int <- stats::as.formula(gsub(
      "poverty_scaled", "poverty_scaled * urban_f",
      paste(deparse(build_formula(outcome, TRUE, use_region, int_type)),
            collapse = ""),
      fixed = TRUE
    ))
    fam <- if (use_counts) stats::poisson(link = "log") else
      stats::binomial(link = "logit")
    m_int <- stats::glm(f_int, data = d, family = fam)
    main[[paste0(outcome, "_M3_interaction_", int_type)]] <-
      tidy_cluster(m_int, d$hsa, TRUE,
                   paste0(outcome, "_M3_interaction_", int_type))
  }
  
  main_tbl <- dplyr::bind_rows(main) %>%
    dplyr::mutate(region_fe = use_region, poverty_units_pp = poverty_per_pp)
  sens_tbl <- dplyr::bind_rows(sens) %>%
    dplyr::mutate(region_fe = use_region, poverty_units_pp = poverty_per_pp)
  
  readr::write_csv(main_tbl, file.path(dir_out, "event_regressions_main.csv"))
  readr::write_csv(sens_tbl, file.path(dir_out, "event_regressions_sensitivity.csv"))
  
  contrast <- main_tbl %>%
    dplyr::filter(.data$term == "poverty_scaled",
                  grepl("M1_total|M2_direct", .data$model)) %>%
    dplyr::select(model, estimate, conf.low, conf.high, p.value, n_obs) %>%
    dplyr::arrange(.data$model)
  
  if (verbose) {
    message(sprintf("\nPoverty (per %d pp). Poisson rows are IRRs, logit rows are ORs:",
                    poverty_per_pp))
    print(as.data.frame(contrast), digits = 3)
    message("\nM1 -> M2 attenuation is the mediation finding, not a null result.")
    if (use_counts) {
      message("Poisson and logit estimates should be close; large divergence ",
              "points at multiplicity doing real work.")
    }
  }
  
  invisible(list(main = main_tbl, sensitivity = sens_tbl,
                 contrast = contrast, panel = panel))
}

# ------------------------------------------------------------------------------
# Descriptive layer
#
# Read this BEFORE interpreting any regression. If the crude gradient is flat,
# an adjusted model that finds one is being driven by the specification.
# ------------------------------------------------------------------------------

run_event_rate_descriptives <- function(
    path_panel     = "data/interim/opening_closure_nonevent_percentiles.csv",
    path_openings  = "data/raw/updated_openings_august2025.csv",
    path_closures  = "data/raw/updated_closures_august2025.csv",
    path_crosswalk = "data/raw/ZipHsaHrr.csv",
    dir_out        = "output/tables",
    year_min       = 2012,
    year_max       = 2023,
    baseline_year  = 2012
) {
  
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  
  panel <- readr::read_csv(path_panel, show_col_types = FALSE, progress = FALSE) %>%
    collapse_to_hsa_year(log_fn = function(...) NULL) %>%
    attach_facilities_lag()
  
  use_counts <- !is.null(path_openings) && !is.null(path_closures)
  if (use_counts) {
    panel <- attach_event_counts(panel, path_openings, path_closures,
                                 path_crosswalk)
  } else {
    panel <- dplyr::mutate(panel,
                           n_closures = .data$any_closure, n_openings = .data$any_opening)
  }
  
  # Baseline-fixed quintiles: HSAs classified once, so a change in the gradient
  # cannot reflect HSAs moving between strata over time.
  base <- panel %>%
    dplyr::filter(.data$year == baseline_year, !is.na(.data$poverty)) %>%
    dplyr::mutate(pov_q = dplyr::ntile(.data$poverty, 5)) %>%
    dplyr::select(hsa, pov_q)
  
  out <- panel %>%
    dplyr::filter(.data$year >= year_min, .data$year <= year_max,
                  !is.na(.data$urbanicity)) %>%
    dplyr::inner_join(base, by = "hsa") %>%
    dplyr::group_by(.data$pov_q, .data$urbanicity) %>%
    dplyr::summarise(
      n_hsas             = dplyr::n_distinct(.data$hsa),
      hsa_years          = dplyr::n(),
      hosp_years_at_risk = sum(.data$facilities_tm1, na.rm = TRUE),
      person_years       = sum(.data$pop, na.rm = TRUE),
      n_closures         = sum(.data$n_closures),
      n_openings         = sum(.data$n_openings),
      closures_per_1000_hosp_years =
        1000 * sum(.data$n_closures) /
        dplyr::na_if(sum(.data$facilities_tm1, na.rm = TRUE), 0),
      openings_per_million_person_years =
        1e6 * sum(.data$n_openings) /
        dplyr::na_if(sum(.data$pop, na.rm = TRUE), 0),
      .groups = "drop"
    )
  
  readr::write_csv(out, file.path(dir_out, "event_rates_descriptive.csv"))
  invisible(out)
}

# ------------------------------------------------------------------------------
# Call from run_project.R, e.g.:
#
#if (exists("run_event_rate_descriptives")) {
#  run_event_rate_descriptives(
#    path_panel     = file.path("data/interim/opening_closure_nonevent_percentiles.csv"),
#    path_openings  = file.path("data/raw/updated_openings_august2025.csv"),
#    path_closures  = file.path("data/raw/updated_closures_august2025.csv"),
#    path_crosswalk = file.path("data/raw/ZipHsaHrr.csv"),
#    dir_out        = "outputs/tables",
#    year_min       = 2012,
#    year_max       = 2023,
#    baseline_year  = 2012
#  )
#}

#if (exists("run_event_regressions")) {
#  run_event_regressions(
#    path_panel     = file.path("data/interim/opening_closure_nonevent_percentiles.csv"),
#    path_openings  = file.path("data/raw/updated_openings_august2025.csv"),
#    path_closures  = file.path("data/raw/updated_closures_august2025.csv"),
#    path_crosswalk = file.path("data/raw/ZipHsaHrr.csv"),
#    path_geo       = NULL,
#    dir_out        = "outputs/tables",
#    dir_checks     = "checks/output",
#    year_min       = 2012,
#    year_max       = 2023
#  )
#}

# ------------------------------------------------------------------------------