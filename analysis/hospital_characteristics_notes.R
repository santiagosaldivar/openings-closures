# Explain coverage of the curated events in an exact-year POS table without
# changing any analytical records. This helper uses base R so it can also be
# exercised independently of the statistical table generation.
hc_event_coverage <- function(openings, closures, panel, raw_pos,
                              include_names = character(), exclude_names = character()) {
  validate <- function(x, name, required = c("ccn", "year")) {
    if (!is.data.frame(x) || !all(required %in% names(x))) {
      stop(name, " must contain: ", paste(required, collapse = ", "), ".")
    }
    if (!is.character(x$ccn) || anyNA(x$ccn) || any(!nzchar(trimws(x$ccn)))) {
      stop(name, " CCNs must be nonmissing character identifiers (preserve leading zeros).")
    }
    if (!is.numeric(x$year) || anyNA(x$year) ||
        any(!is.finite(x$year) | x$year != floor(x$year))) {
      stop(name, " years must be nonmissing integer values.")
    }
    if (anyDuplicated(x[c("ccn", "year")])) {
      stop(name, " must have unique CCN/year keys.")
    }
    invisible(NULL)
  }
  validate(openings, "Openings")
  validate(closures, "Closures")
  validate(panel, "Analysis panel")
  validate(raw_pos, "Raw POS panel",
           c("ccn", "year", "control", "specialty_hosp", "hospname"))
  key <- function(x) paste(x$ccn, x$year, sep = "\r")
  as_events <- function(x, event_type) {
    x <- as.data.frame(x[c("ccn", "year")], stringsAsFactors = FALSE)
    x <- x[x$year >= 2010L & x$year <= 2023L, , drop = FALSE]
    x$event_type <- rep(event_type, nrow(x))
    x[c("event_type", "ccn", "year")]
  }
  events <- rbind(as_events(openings, "Opening"), as_events(closures, "Closure"))
  rownames(events) <- NULL
  raw_index <- match(key(events), key(raw_pos))
  raw_rows <- raw_pos[raw_index, , drop = FALSE]
  events$in_characteristics_table <- key(events) %in% key(panel)
  # Precedence mirrors cleaning/02_clean_pos.R: specialty, ownership, then
  # the manual name list. Retention is always determined by the actual panel.
  specialty_excluded <- !raw_rows$hospname %in% include_names &
    (is.na(raw_rows$specialty_hosp) | raw_rows$specialty_hosp == 1)
  reason <- rep("other_eligibility", nrow(events))
  reason[raw_rows$hospname %in% exclude_names] <- "manual_filter"
  reason[raw_rows$control %in% 10] <- "ownership_filter"
  reason[specialty_excluded] <- "specialty_filter"
  reason[is.na(raw_index)] <- "absent_event_year"
  reason[!events$ccn %in% raw_pos$ccn] <- "absent_ccn"
  reason[events$in_characteristics_table] <- "retained"
  events$reason <- reason
  counts <- if (nrow(events)) {
    aggregate(list(n = rep(1L, nrow(events))), events[c("event_type", "reason")], sum)
  } else {
    data.frame(event_type = character(), reason = character(), n = integer())
  }
  count <- function(type, reason = NULL) {
    selected <- counts$event_type == type
    if (!is.null(reason)) selected <- selected & counts$reason == reason
    sum(counts$n[selected])
  }
  format_n <- function(n) format(n, scientific = FALSE, big.mark = ",", trim = TRUE)
  label <- function(n, type) paste(format_n(n), if (n == 1L) tolower(type) else
                                    paste0(tolower(type), "s"))
  total_openings <- count("Opening")
  total_closures <- count("Closure")
  # Figure 1 also requires an urbanicity match, so curated-event totals must
  # not be labelled as figure totals: they can diverge on future inputs.
  note <- paste0(
    "Of ", label(total_openings, "Opening"), " and ", label(total_closures, "Closure"),
    " identified during 2010--2023, this table includes ", format_n(count("Opening", "retained")),
    " and ", format_n(count("Closure", "retained")),
    ", respectively, with eligible event-year CMS Provider of Services (POS) records. ",
    "Figure 1 does not require event-year POS records."
  )
  descriptions <- c(
    ownership_filter = "excluded by the ownership filter (POS control code 10)",
    absent_event_year = "without an event-year record",
    absent_ccn = "with a CCN absent from POS",
    specialty_filter = "excluded by the specialty-hospital filter",
    manual_filter = "excluded by the hospital-name list",
    other_eligibility = "not retained under other POS eligibility criteria"
  )
  clauses <- vapply(names(descriptions), function(reason) {
    ns <- c(Opening = count("Opening", reason), Closure = count("Closure", reason))
    if (!any(ns)) return("")
    types <- names(ns)[ns > 0L]
    quantities <- vapply(types, function(type) label(ns[[type]], type), character(1))
    paste(paste(quantities, collapse = " and "), descriptions[[reason]])
  }, character(1))
  clauses <- clauses[nzchar(clauses)]
  if (length(clauses)) note <- paste0(note, " Exclusions: ", paste(clauses, collapse = "; "), ".")
  # All generated prose is fixed ASCII and formatted numbers, with no raw
  # hospital identifiers/names interpolated into the LaTeX note.
  stopifnot(sum(counts$n) == nrow(events),
            sum(events$reason == "retained") == sum(events$in_characteristics_table))
  list(events = events, counts = counts, note = note)
}
