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
#   Exclusions (reported, not plotted): events with a ZIP unmatched in the
#   ZIP-HSA crosswalk, and events in HSAs with a missing baseline value.

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
    event_year_max = 2023L
) {
  national_path <- resolve_input_path(national_percentiles_file, "National percentile file")
  openings_path <- resolve_input_path(openings_file, "Openings file")
  closures_path <- resolve_input_path(closures_file, "Closures file")
  crosswalk_path <- resolve_input_path(crosswalk_file, "ZIP-HSA crosswalk")

  # --- Baseline 2010 beds-per-1,000 and deciles ------------------------------
  baseline <- read_csv(national_path, show_col_types = FALSE) %>%
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
    left_join(baseline_deciled %>% select(hsanum, decile), by = "hsanum")

  n_events <- nrow(events)
  n_unmatched_zip <- sum(is.na(events$hsanum))
  n_missing_baseline <- sum(!is.na(events$hsanum) & is.na(events$decile))

  message("Events ", event_year_min, "-", event_year_max, ": ", n_events,
          " hospital-level rows; excluded ", n_unmatched_zip,
          " with unmatched ZIP and ", n_missing_baseline,
          " in HSAs with missing baseline beds-per-1,000.")

  # --- Counts on a complete decile x type grid -------------------------------
  counts <- events %>%
    filter(!is.na(decile)) %>%
    count(decile, event_type, name = "n_events") %>%
    complete(
      decile = 1:10,
      event_type = c("Opening", "Closure"),
      fill = list(n_events = 0L)
    ) %>%
    arrange(decile, event_type)

  decile_labels <- setNames(as.character(1:10), as.character(1:10))
  decile_labels["1"] <- "1 (fewest beds)"
  decile_labels["10"] <- "10 (most beds)"

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
      hjust = -0.25, size = 3
    ) +
    scale_fill_manual(
      values = c("Opening" = "#2166AC", "Closure" = "#D73027"),
      breaks = c("Opening", "Closure"),
      name = NULL
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
    # Title and subtitle intentionally omitted; captioning is done in Overleaf.
    labs(
      x = paste0("Hospital events, ", event_year_min, "-", event_year_max),
      y = "Decile of baseline beds per 1,000 residents (2010)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )

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
      hjust = -0.1, vjust = 1.5, size = 3, color = "#D73027"
    ) +
    labs(
      x = "Certified beds per 1,000 residents, 2010",
      y = "Number of HSAs",
      title = "Distribution of HSA hospital-bed supply at 2010 baseline",
      subtitle = paste0(
        "2009 certified beds over 2010 Decennial Census population; ",
        nrow(baseline_deciled), " HSAs with non-missing values."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    )

  dir.create(dirname(out_fig), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_hist), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_fig, p, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(out_hist, p_hist, width = 8, height = 5, dpi = 300, bg = "white")

  counts_out <- counts %>%
    mutate(
      n_hsas_deciled = nrow(baseline_deciled),
      n_hsas_missing_baseline = n_hsa_missing,
      n_events_unmatched_zip = n_unmatched_zip,
      n_events_missing_baseline = n_missing_baseline
    )
  write_csv(counts_out, out_csv)

  message("Wrote: ", out_fig)
  message("Wrote: ", out_hist)
  message("Wrote: ", out_csv)
  invisible(counts_out)
}
