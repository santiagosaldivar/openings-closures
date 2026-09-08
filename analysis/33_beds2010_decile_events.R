# Hospital events (2010-2023) by decile of the baseline 2010 beds-per-1,000
# distribution, plus a histogram of the baseline distribution itself.
# Usage: source("analysis/33_beds2010_decile_events.R"); run_beds2010_decile_events()
#
# DESIGN
#   Baseline: certbeds_per_1000_residents_lag1 at staged year 2010, i.e. 2009
#   certified beds over the 2010 Decennial Census population (contemporaneous
#   denominator; see docs/codebook.md and 04_stage_national_percentiles.R).
#   Deciles are national ntile(., 10) across HSAs with a non-missing baseline;
#   decile 1 = fewest beds per capita. Deciles are FIXED at baseline; bars
#   count hospital-level openings and closures over 2010-2023 in those HSAs
#   (each opening/closing hospital counts once, matching the curated files).
#
#   POVERTY SPLIT (split_by_poverty = TRUE)
#   Each bar is stacked into HSAs below vs. at/above the national median of
#   weighted_percent_below_poverty_line_event at year `poverty_year` (default
#   2012 = ACS 2008-2012 5-year, midpoint 2010, the vintage aligned with the
#   2010 Decennial denominator in the beds baseline; see the same midpoint
#   argument in docs/codebook.md). This is the identical variable
#   (S1701_C03_001E) and weight (S1701_C01_001E) used everywhere else in the
#   project; note that 2010 and 2011 are NA by construction because the S1701
#   extracts begin at 2012. The threshold is FIXED at baseline and applied
#   nationally, independent of bed decile.
#
#   threshold_mode = "hsa_median" (default): the median of the HSA-level values
#   across the deciled HSAs, unweighted, so each HSA counts once regardless of
#   population. Splits the HSAs exactly in half by construction (14.94% at
#   2012, giving 1,362 / 1,362).
#
#   threshold_mode = "national_rate": the population-weighted national poverty
#   rate across all HSAs, i.e. an external benchmark rather than a within-
#   sample median, so the split is not 50/50 (14.91% at 2012, giving
#   1,358 / 1,366). The two agree closely: switching modes moves 1 of the 430
#   plotted events, which is worth citing as a robustness check because it
#   shows the median split is not an arbitrary internal convention.
#
#   CAVEAT: the 2008-2012 window overlaps event years 2010-2012, so the split
#   variable is not strictly pre-treatment.
#
#   Exclusions (reported, not plotted): events with a ZIP unmatched in the
#   ZIP-HSA crosswalk, and events in HSAs with a missing baseline value. When
#   splitting, events in HSAs with a missing poverty value are also excluded.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tidyr)
})

resolve_input_path <- function(primary_path, label) {
  if (file.exists(primary_path)) return(primary_path)
  stop(label, " not found at: ", primary_path)
}

#' Bar chart of openings/closures by baseline-2010 beds-per-1,000 decile
#'
#' @param national_percentiles_file Staged national percentile file (from 04)
#' @param openings_file,closures_file Curated event files (raw)
#' @param crosswalk_file ZIP -> HSA crosswalk
#' @param out_fig Destination PNG (decile bar chart)
#' @param out_hist Destination PNG (histogram of the 2010 baseline values)
#' @param out_csv Destination CSV of the plotted counts
#' @param event_year_min,event_year_max Event window (default 2010-2023)
#' @param split_by_poverty Stack each bar by national-median poverty half
#' @param poverty_year Year of the staged poverty value (2012 = ACS 2008-2012)
#' @param threshold_mode "hsa_median" (median across deciled HSAs, the default)
#'   or "national_rate" (population-weighted national poverty rate)
#' @param write_histogram Whether to (re)write the baseline histogram
#' @return tibble of decile-level counts invisibly
run_beds2010_decile_events <- function(
    national_percentiles_file = "data/interim/ntl_hsa_percentiles.csv",
    openings_file = "data/raw/updated_openings_august2025.csv",
    closures_file = "data/raw/updated_closures_august2025.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    out_fig = "outputs/figures/beds2010_decile_events.png",
    out_hist = "outputs/figures/beds2010_baseline_histogram.png",
    out_csv = "outputs/tables/beds2010_decile_events.csv",
    event_year_min = 2010L,
    event_year_max = 2023L,
    font_family = "Times",
    split_by_poverty = FALSE,
    poverty_year = 2012L,
    threshold_mode = c("hsa_median", "national_rate"),
    write_histogram = !split_by_poverty
) {
  threshold_mode <- match.arg(threshold_mode)

  national_path <- resolve_input_path(national_percentiles_file, "National percentile file")
  openings_path <- resolve_input_path(openings_file, "Openings file")
  closures_path <- resolve_input_path(closures_file, "Closures file")
  crosswalk_path <- resolve_input_path(crosswalk_file, "ZIP-HSA crosswalk")

  # --- Baseline 2010 beds-per-1,000 and deciles ------------------------------
  ntl <- read_csv(national_path, show_col_types = FALSE)

  baseline <- ntl %>%
    filter(year == 2010L) %>%
    select(hsanum, beds2010 = certbeds_per_1000_residents_lag1)

  if (nrow(baseline) == 0) {
    stop("No 2010 rows in the national percentile file. ",
         "Re-run 04_stage_national_percentiles.R with the 2010/2011 addition.")
  }
  dup_hsa <- baseline %>% count(hsanum) %>% filter(n > 1) %>% nrow()
  if (dup_hsa > 0) stop(dup_hsa, " duplicate hsanum row(s) at year 2010.")

  n_hsa_total <- nrow(baseline)
  n_hsa_missing <- sum(is.na(baseline$beds2010))

  baseline_deciled <- baseline %>%
    filter(!is.na(beds2010)) %>%
    mutate(decile = ntile(beds2010, 10L))

  message("Baseline 2010 HSAs: ", n_hsa_total, " total; ",
          n_hsa_missing, " excluded for missing beds-per-1,000 (",
          nrow(baseline_deciled), " deciled).")

  # --- Optional poverty half (threshold fixed at baseline, applied nationally)
  poverty_levels <- if (threshold_mode == "national_rate") {
    c("Below US poverty rate", "At or above US poverty rate")
  } else {
    c("Below median poverty", "At or above median poverty")
  }
  pov_threshold <- NA_real_
  n_hsa_missing_poverty <- 0L

  if (split_by_poverty) {
    poverty <- ntl %>%
      filter(year == poverty_year) %>%
      select(hsanum,
             poverty = weighted_percent_below_poverty_line_event,
             pov_weight = sum_total_pop_event)

    if (nrow(poverty) == 0) {
      stop("No year ", poverty_year, " rows in the national percentile file.")
    }
    if (all(is.na(poverty$poverty))) {
      stop("weighted_percent_below_poverty_line_event is entirely missing at ",
           "year ", poverty_year, ". The S1701 extracts begin at 2012, so ",
           "2010 and 2011 are NA by construction.")
    }

    baseline_deciled <- baseline_deciled %>%
      left_join(poverty %>% select(hsanum, poverty), by = "hsanum")

    n_hsa_missing_poverty <- sum(is.na(baseline_deciled$poverty))

    if (threshold_mode == "national_rate") {
      # Population-weighted national poverty rate across ALL HSAs: an external
      # benchmark rather than a within-sample median, so it does not split the
      # HSAs in half. The staged file does not carry the S1701 poverty universe
      # (S1701_C01_001E), so total population is used as the weight instead.
      # That approximation lands within 0.02 points of the universe-weighted
      # figure recomputed from the raw ZCTA file (14.905 vs 14.884 at 2012).
      w <- poverty %>%
        filter(!is.na(poverty), !is.na(pov_weight), pov_weight > 0)
      pov_threshold <- 100 * sum(w$poverty / 100 * w$pov_weight) / sum(w$pov_weight)
      thr_label <- "national population-weighted rate"
    } else {
      # Median of the HSA-level values across the deciled HSAs, unweighted, so
      # each HSA counts once regardless of population. Splits 50/50 by
      # construction. Ties, of which there are none at 2012, go to the upper
      # half.
      pov_threshold <- median(baseline_deciled$poverty, na.rm = TRUE)
      thr_label <- "median across deciled HSAs"
    }

    baseline_deciled <- baseline_deciled %>%
      mutate(poverty_half = factor(
        dplyr::if_else(poverty < pov_threshold, poverty_levels[1], poverty_levels[2]),
        levels = poverty_levels
      ))

    message("Poverty split: ACS ", poverty_year - 4L, "-", poverty_year,
            " 5-year; ", thr_label, " = ", sprintf("%.2f", pov_threshold),
            "%; ", n_hsa_missing_poverty, " deciled HSA(s) missing poverty.")
  }

  # --- Hospital-level events, 2010-2023 --------------------------------------
  zip_hsa <- read_csv(crosswalk_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()

  openings <- read_csv(openings_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      event_year = as.integer(part_year),
      event_type = "Opening"
    )
  closures <- read_csv(closures_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      event_year = as.integer(term_year),
      event_type = "Closure"
    )

  events <- bind_rows(openings, closures) %>%
    filter(!is.na(event_year),
           event_year >= event_year_min, event_year <= event_year_max) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(
      baseline_deciled %>% select(hsanum, decile, any_of("poverty_half")),
      by = "hsanum"
    )

  n_events <- nrow(events)
  n_unmatched_zip <- sum(is.na(events$hsanum))
  n_missing_baseline <- sum(!is.na(events$hsanum) & is.na(events$decile))
  n_missing_poverty <- if (split_by_poverty) {
    sum(!is.na(events$decile) & is.na(events$poverty_half))
  } else {
    0L
  }

  message("Events ", event_year_min, "-", event_year_max, ": ", n_events,
          " hospital-level rows; excluded ", n_unmatched_zip,
          " with unmatched ZIP and ", n_missing_baseline,
          " in HSAs with missing baseline beds-per-1,000.")
  if (split_by_poverty) {
    message("Additionally excluded ", n_missing_poverty,
            " event(s) in HSAs with missing poverty at year ", poverty_year, ".")
  }

  # --- Counts on a complete decile x type (x poverty half) grid --------------
  decile_labels <- setNames(as.character(1:10), as.character(1:10))
  decile_labels["1"] <- "1 (fewest beds)"
  decile_labels["10"] <- "10 (most beds)"

  if (split_by_poverty) {
    counts <- events %>%
      filter(!is.na(decile), !is.na(poverty_half)) %>%
      count(decile, event_type, poverty_half, name = "n_events") %>%
      complete(
        decile = 1:10,
        event_type = c("Opening", "Closure"),
        poverty_half = poverty_levels,
        fill = list(n_events = 0L)
      ) %>%
      arrange(decile, event_type, poverty_half)
  } else {
    counts <- events %>%
      filter(!is.na(decile)) %>%
      count(decile, event_type, name = "n_events") %>%
      complete(
        decile = 1:10,
        event_type = c("Opening", "Closure"),
        fill = list(n_events = 0L)
      ) %>%
      arrange(decile, event_type)
  }

  x_lab <- paste0("Hospital events, ", event_year_min, "-", event_year_max)
  y_lab <- "Decile of baseline beds per 1,000 residents (2010)"

  if (split_by_poverty) {
    plot_df <- counts %>%
      mutate(
        decile_lab = factor(
          decile_labels[as.character(decile)],
          levels = decile_labels[as.character(1:10)]
        ),
        event_type = factor(event_type, levels = c("Closure", "Opening")),
        poverty_half = factor(poverty_half, levels = poverty_levels),
        fill_key = factor(
          paste(event_type, poverty_half, sep = ": "),
          levels = c(
            paste("Opening", poverty_levels, sep = ": "),
            paste("Closure", poverty_levels, sep = ": ")
          )
        ),
        # Manual dodge: each (decile, event_type) pair gets its own numeric y,
        # so the two bars sit side by side within a decile band and each one's
        # poverty segments stack along x. ggplot cannot dodge and stack on the
        # same axis in one call; doing the geometry by hand avoids that and
        # avoids an extra package dependency. Opening sits just above Closure.
        y_num = as.numeric(decile_lab) +
          dplyr::if_else(event_type == "Opening", 0.19, -0.19)
      ) %>%
      # Explicit segment extents. NOTE: do not switch these back to geom_col +
      # position_stack. Because y_num is continuous rather than a discrete
      # factor, ggplot infers a vertical orientation and stacks along the wrong
      # axis, and geom_text has no `orientation` argument to correct it with.
      arrange(decile, event_type, poverty_half) %>%
      group_by(decile, event_type) %>%
      mutate(
        x_max = cumsum(n_events),
        x_min = x_max - n_events
      ) %>%
      ungroup()

    totals <- plot_df %>%
      group_by(decile_lab, event_type, y_num) %>%
      summarise(n_total = sum(n_events), .groups = "drop")

    fill_values <- setNames(
      c("#92C5DE", "#2166AC", "#F4A582", "#D73027"),
      levels(plot_df$fill_key)
    )
    text_values <- setNames(
      c("grey15", "white", "grey15", "white"),
      levels(plot_df$fill_key)
    )

    bar_half <- 0.17

    p <- ggplot(plot_df) +
      geom_rect(
        aes(
          xmin = x_min, xmax = x_max,
          ymin = y_num - bar_half, ymax = y_num + bar_half,
          fill = fill_key
        )
      ) +
      geom_text(
        data = plot_df %>% filter(n_events > 0),
        aes(x = (x_min + x_max) / 2, y = y_num,
            label = n_events, color = fill_key),
        size = 2.5, family = font_family, show.legend = FALSE
      ) +
      geom_text(
        data = totals,
        aes(x = n_total, y = y_num, label = n_total),
        hjust = -0.3, size = 3, family = font_family
      ) +
      scale_fill_manual(values = fill_values, name = NULL) +
      scale_color_manual(values = text_values, guide = "none") +
      scale_y_continuous(
        breaks = 1:10,
        labels = unname(decile_labels[as.character(1:10)]),
        expand = expansion(add = 0.6)
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
      # Title and subtitle intentionally omitted; captioning is done in Overleaf.
      labs(x = x_lab, y = y_lab) +
      theme_minimal(base_family = font_family, base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )
  } else {
    plot_df <- counts %>%
      mutate(
        decile_lab = factor(
          decile_labels[as.character(decile)],
          levels = decile_labels[as.character(1:10)]
        ),
        event_type = factor(event_type, levels = c("Closure", "Opening"))
      )

    p <- ggplot(plot_df, aes(x = n_events, y = decile_lab, fill = event_type)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = n_events),
        position = position_dodge(width = 0.8),
        hjust = -0.25, size = 3, family = font_family
      ) +
      scale_fill_manual(
        values = c("Opening" = "#2166AC", "Closure" = "#D73027"),
        breaks = c("Opening", "Closure"),
        name = NULL
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
      # Title and subtitle intentionally omitted; captioning is done in Overleaf.
      labs(x = x_lab, y = y_lab) +
      theme_minimal(base_family = font_family, base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )
  }

  # --- Histogram of the 2010 baseline distribution ---------------------------
  hist_median <- median(baseline_deciled$beds2010)
  p_hist <- ggplot(baseline_deciled, aes(x = beds2010)) +
    geom_histogram(bins = 50, fill = "grey55", color = "white", linewidth = 0.2) +
    geom_vline(xintercept = hist_median, linetype = "dashed",
               color = "#D73027", linewidth = 0.5) +
    annotate(
      "text",
      x = hist_median, y = Inf,
      label = sprintf("Median = %.2f", hist_median),
      hjust = -0.1, vjust = 1.5, size = 3, color = "#D73027", 
      family = font_family
    ) +
    labs(
      x = "Certified beds per 1,000 residents, 2010",
      y = "Number of HSAs"
    ) +
    theme_minimal(base_family=font_family,base_size = 11) +
    theme(
      panel.grid.minor = element_blank()
    )

  dir.create(dirname(out_fig), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_fig, p, width = 8, height = 6, dpi = 300, bg = "white")
  if (write_histogram) {
    dir.create(dirname(out_hist), recursive = TRUE, showWarnings = FALSE)
    ggsave(out_hist, p_hist, width = 8, height = 5, dpi = 300, bg = "white")
  }

  counts_out <- counts %>%
    mutate(
      n_hsas_deciled = nrow(baseline_deciled),
      n_hsas_missing_baseline = n_hsa_missing,
      n_events_unmatched_zip = n_unmatched_zip,
      n_events_missing_baseline = n_missing_baseline
    )
  if (split_by_poverty) {
    counts_out <- counts_out %>%
      mutate(
        poverty_source_year = poverty_year,
        poverty_threshold_mode = threshold_mode,
        poverty_threshold = pov_threshold,
        n_hsas_missing_poverty = n_hsa_missing_poverty,
        n_events_missing_poverty = n_missing_poverty
      )
  }
  write_csv(counts_out, out_csv)

  message("Wrote: ", out_fig)
  if (write_histogram) message("Wrote: ", out_hist)
  message("Wrote: ", out_csv)
  invisible(counts_out)
}

#' Bar chart of openings/closures by baseline population-change decile
#'
#' This is the bar-chart counterpart to the population-change/poverty decile
#' grid, but follows the baseline design of the beds-decile appendix figure:
#' both the population-change decile and poverty half are fixed before the
#' event window. It deliberately reads the raw event files (rather than the
#' staged event rows), so multiple hospital events in one HSA-year each remain
#' an event in the plotted count.
#'
#' @param input_csv Staged HSA-year panel used by the decile-grid figure
#' @param openings_file,closures_file Curated raw event files
#' @param crosswalk_file ZIP -> HSA crosswalk
#' @param out_fig,out_hist,out_csv Destination bar figure, baseline-distribution
#'   histogram, and count audit
#' @param event_year_min,event_year_max Event window; 2014 is the first year
#'   with a valid population-change percentile
#' @param baseline_popchange_year Staged year defining the fixed population-
#'   change deciles (2014 is the 2011--2013 population-change measure)
#' @param poverty_year Staged year defining the fixed poverty split
#' @return Decile-level event counts invisibly
run_popchange_decile_events <- function(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    openings_file = "data/raw/updated_openings_august2025.csv",
    closures_file = "data/raw/updated_closures_august2025.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    out_fig = "outputs/figures/popchange_decile_events_by_poverty.png",
    out_hist = "outputs/figures/popchange2014_baseline_histogram.png",
    out_csv = "outputs/tables/popchange_decile_events_by_poverty.csv",
    event_year_min = 2014L,
    event_year_max = 2023L,
    baseline_popchange_year = 2014L,
    poverty_year = 2012L,
    font_family = "Times"
) {
  staged_path <- resolve_input_path(input_csv, "Staged percentile file")
  openings_path <- resolve_input_path(openings_file, "Openings file")
  closures_path <- resolve_input_path(closures_file, "Closures file")
  crosswalk_path <- resolve_input_path(crosswalk_file, "ZIP-HSA crosswalk")

  staged <- read_csv(staged_path, show_col_types = FALSE)
  required_cols <- c(
    "hsanum", "year", "weighted_percent_below_poverty_line_event",
    "pop_change_pct", "pop_change_pct_percentile"
  )
  missing_cols <- setdiff(required_cols, names(staged))
  if (length(missing_cols) > 0) {
    stop("Staged percentile file is missing: ", paste(missing_cols, collapse = ", "))
  }

  # Same conversion from percentile to decile as analysis/22_decile_grid_heatmaps.R.
  # Unlike the grid, the bar chart holds these classifications fixed at baseline.
  to_decile <- function(p) {
    max_p <- suppressWarnings(max(p, na.rm = TRUE))
    if (is.finite(max_p) && max_p <= 1) p <- p * 100
    cut(p, breaks = seq(0, 100, by = 10), include.lowest = TRUE, labels = FALSE)
  }

  staged_lookup <- staged %>%
    transmute(
      hsanum = as.integer(hsanum),
      year = as.integer(year),
      poverty = weighted_percent_below_poverty_line_event,
      pop_change_pct,
      pop_change_pct_percentile
    ) %>%
    distinct(hsanum, year, .keep_all = TRUE)

  baseline_popchange <- staged_lookup %>%
    filter(year == baseline_popchange_year) %>%
    transmute(
      hsanum,
      pop_change_pct,
      pop_change_decile = to_decile(pop_change_pct_percentile)
    ) %>%
    filter(!is.na(pop_change_decile))

  baseline_poverty <- staged_lookup %>%
    filter(year == poverty_year) %>%
    select(hsanum, poverty)

  if (nrow(baseline_popchange) == 0) {
    stop("No population-change percentiles at baseline year ",
         baseline_popchange_year, ".")
  }
  if (nrow(baseline_poverty) == 0 || all(is.na(baseline_poverty$poverty))) {
    stop("No poverty values at baseline year ", poverty_year, ".")
  }

  n_hsas_missing_poverty <- baseline_popchange %>%
    anti_join(baseline_poverty %>% filter(!is.na(poverty)), by = "hsanum") %>%
    nrow()

  pov_threshold <- baseline_popchange %>%
    left_join(baseline_poverty, by = "hsanum") %>%
    pull(poverty) %>%
    median(na.rm = TRUE)

  lookup <- baseline_popchange %>%
    left_join(baseline_poverty, by = "hsanum") %>%
    filter(!is.na(poverty)) %>%
    mutate(
      poverty_half = factor(
        if_else(
          poverty < pov_threshold,
          "Below median poverty",
          "At or above median poverty"
        ),
        levels = c("Below median poverty", "At or above median poverty")
      ),
      in_lookup = TRUE
    )

  if (nrow(lookup) == 0) {
    stop("No HSAs have both baseline population-change and poverty values.")
  }

  poverty_levels <- levels(lookup$poverty_half)

  zip_hsa <- read_csv(crosswalk_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsanum = as.integer(hsanum)
    ) %>%
    distinct()

  read_events <- function(path, year_col, event_type) {
    read_csv(path, show_col_types = FALSE) %>%
      transmute(
        zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
        year = as.integer(.data[[year_col]]),
        event_type = event_type
      ) %>%
      filter(!is.na(year), year >= event_year_min, year <= event_year_max)
  }

  events_raw <- bind_rows(
    read_events(openings_path, "part_year", "Opening"),
    read_events(closures_path, "term_year", "Closure")
  ) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(
      lookup %>%
        select(hsanum, pop_change_decile, poverty_half, in_lookup),
      by = "hsanum"
    ) %>%
    mutate(
      drop_cause = case_when(
        is.na(hsanum) ~ "zip_not_in_crosswalk",
        is.na(in_lookup) ~ "missing_baseline_population_change_or_poverty",
        TRUE ~ "kept"
      )
    )

  events <- events_raw %>% filter(drop_cause == "kept")
  if (nrow(events) == 0) stop("No events could be placed in a decile.")

  dropped <- events_raw %>%
    filter(drop_cause != "kept") %>%
    count(event_type, drop_cause, name = "n_events")
  n_unmatched_zip <- sum(events_raw$drop_cause == "zip_not_in_crosswalk")
  n_missing_lookup <- sum(
    events_raw$drop_cause == "missing_baseline_population_change_or_poverty"
  )

  counts <- events %>%
    count(pop_change_decile, event_type, poverty_half, name = "n_events") %>%
    complete(
      pop_change_decile = 1:10,
      event_type = c("Opening", "Closure"),
      poverty_half = poverty_levels,
      fill = list(n_events = 0L)
    ) %>%
    arrange(pop_change_decile, event_type, poverty_half)

  decile_labels <- setNames(as.character(1:10), as.character(1:10))
  decile_labels["1"] <- "1 (least change)"
  decile_labels["10"] <- "10 (most change)"

  plot_df <- counts %>%
    mutate(
      decile_lab = factor(
        decile_labels[as.character(pop_change_decile)],
        levels = decile_labels[as.character(1:10)]
      ),
      event_type = factor(event_type, levels = c("Closure", "Opening")),
      poverty_half = factor(poverty_half, levels = poverty_levels),
      fill_key = factor(
        paste(event_type, poverty_half, sep = ": "),
        levels = c(
          paste("Opening", poverty_levels, sep = ": "),
          paste("Closure", poverty_levels, sep = ": ")
        )
      ),
      y_num = as.numeric(decile_lab) +
        if_else(event_type == "Opening", 0.19, -0.19)
    ) %>%
    arrange(pop_change_decile, event_type, poverty_half) %>%
    group_by(pop_change_decile, event_type) %>%
    mutate(x_max = cumsum(n_events), x_min = x_max - n_events) %>%
    ungroup()

  totals <- plot_df %>%
    group_by(decile_lab, event_type, y_num) %>%
    summarise(n_total = sum(n_events), .groups = "drop")

  fill_values <- setNames(
    c("#92C5DE", "#2166AC", "#F4A582", "#D73027"),
    levels(plot_df$fill_key)
  )
  text_values <- setNames(
    c("grey15", "white", "grey15", "white"),
    levels(plot_df$fill_key)
  )

  p <- ggplot(plot_df) +
    geom_rect(
      aes(
        xmin = x_min, xmax = x_max,
        ymin = y_num - 0.17, ymax = y_num + 0.17,
        fill = fill_key
      )
    ) +
    geom_text(
      data = plot_df %>% filter(n_events > 0),
      aes(x = (x_min + x_max) / 2, y = y_num,
          label = n_events, color = fill_key),
      size = 2.5, family = font_family, show.legend = FALSE
    ) +
    geom_text(
      data = totals,
      aes(x = n_total, y = y_num, label = n_total),
      hjust = -0.3, size = 3, family = font_family
    ) +
    scale_fill_manual(values = fill_values, name = NULL) +
    scale_color_manual(values = text_values, guide = "none") +
    scale_y_continuous(
      breaks = 1:10,
      labels = unname(decile_labels[as.character(1:10)]),
      expand = expansion(add = 0.6)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
    labs(
      x = paste0("Hospital events, ", event_year_min, "-", event_year_max),
      y = paste0("Decile of baseline population change (", baseline_popchange_year, ")")
    ) +
    theme_minimal(base_family = font_family, base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )

  # Distribution of the same fixed 2014 population-change measure used to
  # assign the event-chart deciles. A one-percentage-point bin width preserves
  # the full observed range while keeping the zero-change reference legible.
  hist_median <- median(baseline_popchange$pop_change_pct)
  p_hist <- ggplot(baseline_popchange, aes(x = pop_change_pct)) +
    geom_histogram(
      binwidth = 1, boundary = 0,
      fill = "grey55", color = "white", linewidth = 0.2
    ) +
    geom_vline(xintercept = 0, color = "grey35", linewidth = 0.45) +
    geom_vline(
      xintercept = hist_median, linetype = "dashed",
      color = "#2166AC", linewidth = 0.5
    ) +
    annotate(
      "text",
      x = hist_median, y = Inf,
      label = sprintf("Median = %.2f%%", hist_median),
      hjust = -0.1, vjust = 1.5, size = 3,
      color = "#2166AC", family = font_family
    ) +
    labs(
      x = "Population change, 2011-2013 (%)",
      y = "Number of HSAs"
    ) +
    theme_minimal(base_family = font_family, base_size = 11) +
    theme(panel.grid.minor = element_blank())

  dir.create(dirname(out_fig), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_hist), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_fig, p, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(out_hist, p_hist, width = 8, height = 5, dpi = 300, bg = "white")

  counts_out <- counts %>%
    mutate(
      event_year_min = event_year_min,
      event_year_max = event_year_max,
      baseline_population_change_year = baseline_popchange_year,
      poverty_source_year = poverty_year,
      poverty_threshold_mode = "baseline_hsa_median",
      poverty_threshold = pov_threshold,
      n_hsas_eligible = n_distinct(lookup$hsanum),
      n_hsas_missing_baseline_poverty = n_hsas_missing_poverty,
      n_events_total = nrow(events_raw),
      n_events_unmatched_zip = n_unmatched_zip,
      n_events_missing_baseline_population_change_or_poverty = n_missing_lookup
    )
  write_csv(counts_out, out_csv)

  message(
    "Population-change figure: placed ", sum(counts$n_events), " of ",
    nrow(events_raw), " raw events (", n_unmatched_zip, " unmatched ZIP; ",
    n_missing_lookup, " missing baseline population change or poverty)."
  )
  message(
    "Baseline population-change year ", baseline_popchange_year,
    "; poverty: ACS ", poverty_year - 4L, "-", poverty_year,
    " median across baseline-deciled HSAs = ", sprintf("%.2f", pov_threshold), "%."
  )
  message("Wrote: ", out_fig)
  message("Wrote: ", out_hist)
  message("Wrote: ", out_csv)
  if (nrow(dropped) > 0) print(as.data.frame(dropped), row.names = FALSE)
  invisible(counts_out)
}
