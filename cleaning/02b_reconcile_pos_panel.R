# =============================================================================
# 02b_reconcile_pos_panel.R  (v2: row-level imputation)
# -----------------------------------------------------------------------------
# PURPOSE
#   Repairs the active-flag inconsistency caused by missing POS term_year on
#   individual panel rows. POS records active row-wise via:
#
#     active = !is.na(part_year) & year > part_year &
#              (is.na(term_year) | year < term_year)
#
#   term_year is ROW-VARYING (panel file vintages: termination is typically
#   recorded only on rows at/after the event, and sometimes not at all). Rows
#   whose vintage lacks the termination fall through as active == 1, producing
#   89 closure-year misfires and 60 post-closure phantom-active rows (90 CCNs,
#   149 rows) that inflate capacity and leak into non-event pools.
#
#   Empirical basis (checks/output audits, Jul 2026):
#     - termdate is unpopulated wherever term_year is NA -> no derivation
#       repair available; impute from the curated closure file.
#     - POS term_year == curated term_year for all 212 closure-year rows where
#       it is recorded (offset 0), and populated later-vintage values match
#       the curated year -> imputation value is the curated term_year.
#
#   FIX (row-level): for CCNs in the curated closure file, fill term_year with
#   the curated term_year ON ROWS WHERE IT IS NA, leaving populated rows
#   untouched; then recompute active from the formula for the whole panel.
#   Filling pre-closure rows is harmless (year < term_year keeps active == 1);
#   filling closure-year and later rows is the fix (active becomes 0).
#
#   GUARD: CCNs with any populated term_year != curated term_year (prior
#   stint / possible re-entry) are NOT imputed; they are logged and written to
#   an audit file for manual review.
#
# PIPELINE POSITION
#   After cleaning/01_clean_open_close.R and cleaning/02_clean_pos.R,
#   before cleaning/04_stage_national_percentiles.R.
#
# OUTPUTS
#   data/processed/pos_panel_reconciled.csv
#   checks/output/reconciliation_audit.csv        (changed cells, old vs new)
#   checks/output/reconciliation_log.txt          (written even on failure)
#   checks/output/reconciliation_conflicts.csv    (only if guard trips)
#   checks/output/reconciliation_FAILED_rows.csv  (only if invariants fail)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

run_reconcile_pos_panel <- function(
    pos_path      = "data/processed/pos_panel_updated.csv",
    closures_path = "data/interim/closures_clean.csv",
    openings_path = "data/interim/openings_clean.csv",
    out_path      = "data/processed/pos_panel_reconciled.csv",
    audit_dir     = "checks/output"
) {
  for (p in c(pos_path, closures_path, openings_path))
    if (!file.exists(p)) stop("Not found: ", p)
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  
  log_lines <- character(0)
  logmsg <- function(...) {
    msg <- paste0(...)
    cat(msg, "\n")
    log_lines <<- c(log_lines, msg)
  }
  # Ensure the log is written even if we stop() partway through.
  on.exit(writeLines(log_lines, file.path(audit_dir, "reconciliation_log.txt")),
          add = TRUE)
  
  pos <- read_csv(pos_path, show_col_types = FALSE)
  closures <- read_csv(closures_path, show_col_types = FALSE) %>%
    select(ccn, curated_term_year = term_year) %>%
    distinct()
  openings <- read_csv(openings_path, show_col_types = FALSE) %>%
    select(ccn, curated_part_year = part_year) %>%
    distinct()
  
  dup <- closures %>% count(ccn) %>% filter(n > 1)
  if (nrow(dup) > 0)
    stop("Curated closures have multiple term_year values for ",
         nrow(dup), " CCN(s); resolve before reconciling: ",
         paste(head(dup$ccn), collapse = ", "))
  
  logmsg("== POS panel reconciliation (v2, row-level) ==")
  logmsg("Input rows: ", nrow(pos), " | closure CCNs: ", nrow(closures))
  
  # ---- GUARD: populated term_year values that disagree with curated ---------
  conflicts <- pos %>%
    inner_join(closures, by = "ccn") %>%
    filter(!is.na(term_year), term_year != curated_term_year) %>%
    distinct(ccn, term_year, curated_term_year)
  conflict_ccns <- unique(conflicts$ccn)
  logmsg("Guard: closure CCNs with populated term_year != curated (excluded ",
         "from imputation, review manually): ", length(conflict_ccns))
  if (length(conflict_ccns) > 0)
    write_csv(conflicts, file.path(audit_dir, "reconciliation_conflicts.csv"))
  
  # ---- Row-level imputation -------------------------------------------------
  pos_new <- pos %>%
    left_join(closures %>% filter(!ccn %in% conflict_ccns), by = "ccn") %>%
    mutate(
      term_year_imputed = !is.na(curated_term_year) & is.na(term_year),
      term_year = if_else(term_year_imputed, curated_term_year, term_year)
    ) %>%
    select(-curated_term_year)
  
  # ---- Recompute active from the exact formula, whole panel -----------------
  pos_new <- pos_new %>%
    mutate(
      active_old = active,
      active = as.integer(
        !is.na(part_year) & year > part_year &
          (is.na(term_year) | year < term_year)
      )
    )
  
  # ---- Audit ----------------------------------------------------------------
  seed_ccns <- closures$ccn
  audit <- pos_new %>%
    filter(term_year_imputed | active != active_old) %>%
    transmute(
      ccn, year, certbeds,
      term_year_new = term_year,
      term_year_was_na = term_year_imputed,
      active_old, active_new = active,
      active_changed = active != active_old,
      is_closure_ccn = ccn %in% seed_ccns
    ) %>%
    arrange(ccn, year)
  
  n_flip <- sum(audit$active_changed)
  n_flip_outside <- sum(audit$active_changed & !audit$is_closure_ccn)
  logmsg("Rows with term_year imputed: ", sum(audit$term_year_was_na),
         " across ", n_distinct(audit$ccn[audit$term_year_was_na]), " CCNs ",
         "(includes harmless pre-closure fills)")
  logmsg("Rows with active flipped 1 -> 0: ", n_flip,
         "  (expected 149: 89 closure-year + 60 post-closure)")
  logmsg("Active flips outside closure CCNs (regression check, expect 0): ",
         n_flip_outside)
  
  # ---- Diagnostics, no behavior change --------------------------------------
  pna <- pos_new %>% filter(is.na(part_year))
  logmsg("Diagnostic: rows with part_year NA (never active by formula): ",
         nrow(pna), " rows, ", n_distinct(pna$ccn), " CCNs, ",
         sum(pna$certbeds, na.rm = TRUE), " beds.")
  
  open_align <- pos_new %>%
    select(ccn, pos_part_year = part_year) %>%
    distinct() %>%
    inner_join(openings, by = "ccn") %>%
    filter(is.na(pos_part_year) | pos_part_year != curated_part_year)
  logmsg("Diagnostic: opening CCNs with POS part_year NA or != curated ",
         "(expect 0): ", n_distinct(open_align$ccn))
  if (nrow(open_align) > 0)
    write_csv(open_align, file.path(audit_dir, "opening_alignment_mismatch.csv"))
  
  # ---- Post-fix invariants (hard stops; conflict CCNs exempt) ---------------
  chk <- pos_new %>%
    inner_join(closures, by = "ccn") %>%
    filter(!ccn %in% conflict_ccns)
  inv1 <- chk %>% filter(year == curated_term_year, active == 1)
  inv2 <- chk %>% filter(year > curated_term_year, active == 1)
  logmsg("Invariant 1: closure-year rows with active == 1 (must be 0): ", nrow(inv1))
  logmsg("Invariant 2: post-closure rows with active == 1 (must be 0): ", nrow(inv2))
  if (nrow(inv1) + nrow(inv2) > 0) {
    write_csv(bind_rows(inv1, inv2),
              file.path(audit_dir, "reconciliation_FAILED_rows.csv"))
    stop("Reconciliation invariants failed; see reconciliation_FAILED_rows.csv")
  }
  
  # ---- Write outputs --------------------------------------------------------
  pos_out <- pos_new %>% select(-active_old, -term_year_imputed)
  write_csv(pos_out, out_path)
  write_csv(audit, file.path(audit_dir, "reconciliation_audit.csv"))
  logmsg("Wrote: ", out_path)
  logmsg("Audit: ", file.path(audit_dir, "reconciliation_audit.csv"))
  
  invisible(pos_out)
}

if (sys.nframe() == 0) run_reconcile_pos_panel()