# Descriptive bed-count statistics: openings, closures, and system capacity by year.
# Usage: source("R/analysis/21_descriptive.R"); run_descriptive()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

#' Descriptive bed-count statistics
#'
#' Reports total and mean certified beds for hospitals that opened and closed
#' over the study period (measured in the event year), plus certified-bed
#' capacity across active hospitals by year (with mean bed size and both
#' year-over-year and cumulative-from-baseline growth). No <= 1 bed exclusion is
#' applied to the headline figures; instead a diagnostic count of small (<= 1)
#' and NA-bed hospitals is reported per group.
#'
#' Two "non-event" definitions are reported separately:
#'   (a) Hospital-year: exclude only event-year rows (a hospital that closes in
#'       2020 still counts in 2010-2019). Matches run_hospital_characteristics.
#'   (b) Hospital-level: exclude any hospital that ever opened or closed during
#'       the study window (never appears in any year).
#'
#' Tables 1-5 are also rendered to a single PDF (one table per page) when the
#' 'gridExtra' package is available.
#'
#' @param openings_path Clean openings CSV
#' @param closures_path Clean closures CSV
#' @param pos_path Cleaned POS panel CSV
#' @param out_dir Output directory for CSV outputs
#' @param out_pdf Output PDF path for rendered tables
#' @param snapshot_years Years for the focused capacity snapshot table
#' @param year_range Years to include in the by-year tables
#' @return list of result frames (invisible)
run_descriptive <- function(
    openings_path = "data/interim/openings_clean.csv",
    closures_path = "data/interim/closures_clean.csv",
    pos_path = "data/processed/pos_panel_reconciled.csv",
    out_dir = "outputs/tables",
    out_pdf = file.path(out_dir, "descriptive_tables.pdf"),
    snapshot_years = c(2010, 2023),
    year_range = 2010:2023
) {
  if (!file.exists(openings_path)) stop("Openings file not found: ", openings_path)
  if (!file.exists(closures_path)) stop("Closures file not found: ", closures_path)
  if (!file.exists(pos_path)) stop("POS panel not found: ", pos_path)
  
  openings <- read_csv(openings_path, show_col_types = FALSE) %>%
    select(opening, ccn, part_year)
  closures <- read_csv(closures_path, show_col_types = FALSE) %>%
    select(closure, ccn, term_year)
  pos_panel_updated <- read_csv(pos_path, show_col_types = FALSE)
  
  # Hospitals that ever experienced an event (used for non-event definition (b))
  ever_event_ccns <- union(openings$ccn, closures$ccn)
  
  # Build event panel using the same merge logic as run_hospital_characteristics
  pos_panel_updated <- merge(
    pos_panel_updated, openings,
    by.x = c("ccn", "year"), by.y = c("ccn", "part_year"), all.x = TRUE
  )
  pos_panel_updated <- merge(
    pos_panel_updated, closures,
    by.x = c("ccn", "year"), by.y = c("ccn", "term_year"), all.x = TRUE
  )
  
  panel_openings <- subset(pos_panel_updated, opening == 1)
  panel_closures <- subset(pos_panel_updated, closure == 1)
  
  # --- Event bed counts (measured in the event year) ---
  # Total/mean computed over rows with non-missing certbeds; N = distinct ccn.
  # No <= 1 exclusion applied here.
  event_stats <- function(df, group_label) {
    valid <- df %>% filter(!is.na(certbeds))
    tibble(
      Group = group_label,
      n_hospitals = n_distinct(valid$ccn),
      total_beds = sum(valid$certbeds, na.rm = TRUE),
      mean_beds = mean(valid$certbeds, na.rm = TRUE)
    )
  }
  
  event_table <- bind_rows(
    event_stats(panel_openings, "Openings"),
    event_stats(panel_closures, "Closures")
  )
  
  # --- Certified-bed capacity by year (reusable helper) ---
  # Aggregates active hospitals per year, then adds mean bed size and growth.
  # Growth is computed across the table's own ordered years; the cumulative
  # baseline is the first year present (first row after arrange).
  beds_by_year <- function(df) {
    df %>%
      filter(active == 1, !is.na(certbeds), year %in% year_range) %>%
      group_by(year) %>%
      summarise(
        n_active_hospitals = n_distinct(ccn),
        total_beds = sum(certbeds, na.rm = TRUE),
        mean_beds = mean(certbeds, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(year) %>%
      mutate(
        total_yoy_pct = (total_beds / lag(total_beds) - 1) * 100,
        total_cum_pct = (total_beds / first(total_beds) - 1) * 100,
        mean_yoy_pct  = (mean_beds  / lag(mean_beds)  - 1) * 100,
        mean_cum_pct  = (mean_beds  / first(mean_beds) - 1) * 100
      )
  }
  
  # All active hospitals, by year
  year_table_all <- beds_by_year(pos_panel_updated)
  
  # Non-event (a): exclude only event-year rows. Comma = AND; each OR is wrapped
  # to avoid the &/| precedence issue in the characteristics-script filter.
  panel_nonevent_a <- pos_panel_updated %>%
    filter(
      (is.na(opening) | opening != 1),
      (is.na(closure) | closure != 1)
    )
  year_table_nonevent_a <- beds_by_year(panel_nonevent_a)
  
  # Non-event (b): exclude any hospital that ever opened or closed in the window
  panel_nonevent_b <- pos_panel_updated %>%
    filter(!(ccn %in% ever_event_ccns))
  year_table_nonevent_b <- beds_by_year(panel_nonevent_b)
  
  # Focused snapshot table (subset of the all-active by-year table)
  year_table_snapshot <- year_table_all %>%
    filter(year %in% snapshot_years)
  
  # --- Diagnostic: small (<= 1) and NA-bed hospitals per group ---
  # <= 1 hospitals are RETAINED in the headline figures; this just records how
  # many there are (they pull the mean down). NA-bed hospitals are excluded
  # from sums/means by na.rm and are counted separately here.
  diag_stats <- function(df, group_label) {
    tibble(
      Group = group_label,
      n_le1_beds = df %>% filter(certbeds <= 1) %>% summarise(n = n_distinct(ccn)) %>% pull(n),
      n_na_beds  = df %>% filter(is.na(certbeds)) %>% summarise(n = n_distinct(ccn)) %>% pull(n)
    )
  }
  diag_table <- bind_rows(
    diag_stats(panel_openings, "Openings"),
    diag_stats(panel_closures, "Closures"),
    bind_rows(lapply(sort(snapshot_years), function(yr) {
      df <- pos_panel_updated %>% filter(active == 1, year == yr)
      diag_stats(df, paste0("Active ", yr))
    }))
  )
  
  # --- Display formatters ---
  fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
  fmt_num <- function(x) formatC(x, format = "f", digits = 1)
  fmt_pct <- function(x) ifelse(is.na(x), "\u2014", sprintf("%+.1f%%", x))
  
  event_display <- event_table %>%
    transmute(
      `Event Group` = Group,
      `N Hospitals` = fmt_int(n_hospitals),
      `Total Beds`  = fmt_int(total_beds),
      `Mean Beds`   = fmt_num(mean_beds)
    )
  
  # Simple 3-column view for the snapshot table (Table 2)
  year_display_simple <- function(df) {
    df %>%
      transmute(
        Year = year,
        `N Active Hospitals` = fmt_int(n_active_hospitals),
        `Total Beds`         = fmt_int(total_beds)
      )
  }
  
  # Full view with mean and growth columns (Tables 3-5)
  year_display_full <- function(df) {
    df %>%
      transmute(
        Year = year,
        `N Active`    = fmt_int(n_active_hospitals),
        `Total Beds`  = fmt_int(total_beds),
        `Mean Beds`   = fmt_num(mean_beds),
        `Total YoY %` = fmt_pct(total_yoy_pct),
        `Total Cum %` = fmt_pct(total_cum_pct),
        `Mean YoY %`  = fmt_pct(mean_yoy_pct),
        `Mean Cum %`  = fmt_pct(mean_cum_pct)
      )
  }
  
  diag_display <- diag_table %>%
    transmute(
      Group,
      `N (certbeds <= 1)` = fmt_int(n_le1_beds),
      `N (certbeds NA)`   = fmt_int(n_na_beds)
    )
  
  # --- Assemble table specs (shared by terminal + PDF) ---
  tables <- list(
    list(title = "Table 1. Certified beds in event hospitals (event-year)",
         df = event_display),
    list(title = "Table 2. System certified-bed capacity, active hospitals (snapshot)",
         df = year_display_simple(year_table_snapshot)),
    list(title = "Table 3. Certified-bed capacity by year, all active hospitals",
         df = year_display_full(year_table_all)),
    list(title = "Table 4. Certified-bed capacity by year, non-event (excl. event-year rows)",
         df = year_display_full(year_table_nonevent_a)),
    list(title = "Table 5. Certified-bed capacity by year, non-event (excl. ever-event hospitals)",
         df = year_display_full(year_table_nonevent_b))
  )
  
  # --- Terminal display ---
  print_table <- function(df, title) {
    cat("\n", title, "\n", sep = "")
    cat(strrep("-", nchar(title)), "\n", sep = "")
    print(as.data.frame(df), row.names = FALSE)
  }
  for (t in tables) print_table(t$df, t$title)
  print_table(diag_display, "Diagnostic. Small (<= 1) and missing-bed hospitals by group")
  cat("\n")
  
  # --- Render Tables 1-5 to a single PDF (one per page) ---
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)
    grDevices::pdf(out_pdf, width = 11, height = 8.5)  # landscape letter
    on.exit(grDevices::dev.off(), add = TRUE)
    theme_tbl <- gridExtra::ttheme_default(
      base_size = 9,
      core = list(fg_params = list(hjust = 1, x = 0.95)),
      colhead = list(fg_params = list(fontface = "bold"))
    )
    for (t in tables) {
      title_grob <- grid::textGrob(
        t$title, gp = grid::gpar(fontsize = 13, fontface = "bold")
      )
      tbl_grob <- gridExtra::tableGrob(t$df, rows = NULL, theme = theme_tbl)
      gridExtra::grid.arrange(
        title_grob, tbl_grob, ncol = 1,
        heights = grid::unit(c(0.07, 0.93), "npc")
      )
    }
    message("Rendered tables to ", out_pdf)
  } else {
    message("Skipping PDF render: install 'gridExtra' to enable.")
  }
  
  # --- Write CSV outputs ---
  write_csv(event_table,           file.path(out_dir, "descriptive_event_beds.csv"))
  write_csv(year_table_snapshot,   file.path(out_dir, "descriptive_year_capacity_snapshot.csv"))
  write_csv(year_table_all,        file.path(out_dir, "descriptive_year_capacity_all.csv"))
  write_csv(year_table_nonevent_a, file.path(out_dir, "descriptive_year_capacity_nonevent_eventyear.csv"))
  write_csv(year_table_nonevent_b, file.path(out_dir, "descriptive_year_capacity_nonevent_everevent.csv"))
  write_csv(diag_table,            file.path(out_dir, "descriptive_bed_diagnostics.csv"))
  
  invisible(list(
    event_beds            = event_table,
    year_capacity_snapshot = year_table_snapshot,
    year_capacity_all     = year_table_all,
    year_capacity_nonevent_eventyear = year_table_nonevent_a,
    year_capacity_nonevent_everevent = year_table_nonevent_b,
    diagnostics           = diag_table
  ))
}