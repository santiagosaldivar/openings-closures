# Run from the repository root: Rscript tests/test_hospital_characteristics_notes.R
source("analysis/hospital_characteristics_notes.R")

events <- function(ccn = character(), year = integer()) {
  data.frame(ccn = ccn, year = year, stringsAsFactors = FALSE)
}
raw_rows <- function(ccn, year, control = rep(1, length(ccn)),
                     specialty_hosp = rep(0, length(ccn)), hospname = ccn) {
  data.frame(ccn = ccn, year = year, control = control,
             specialty_hosp = specialty_hosp, hospname = hospname,
             stringsAsFactors = FALSE)
}
expect_error <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}

# Raw presence must distinguish an unavailable event year from an absent CCN.
# IDs remain strings, so "000001" cannot accidentally match "1".
openings <- events(c("000001", "000002", "000003", "1", "000010"),
                   c(2011L, 2012L, 2013L, 2011L, 2009L))
closures <- events(c("000004", "000005", "000011"), c(2014L, 2015L, 2024L))
raw <- raw_rows(c("000001", "000002", "000004", "000010", "000011"),
                c(2011L, 2013L, 2014L, 2009L, 2024L))
panel <- events("000001", 2011L)
result <- hc_event_coverage(openings, closures, panel, raw)
stopifnot(identical(result$events$reason,
                    c("retained", "absent_event_year", "absent_ccn", "absent_ccn",
                      "other_eligibility", "absent_ccn")),
          identical(result$events$ccn[1], "000001"),
          sum(result$counts$n) == 6L,
          grepl("Of 4 openings and 2 closures", result$note, fixed = TRUE),
          grepl("identified during 2010--2023", result$note, fixed = TRUE),
          !grepl("closures in Figure 1", result$note, fixed = TRUE),
          grepl("includes 1 and 0", result$note, fixed = TRUE),
          grepl("1 opening without an event-year record", result$note, fixed = TRUE),
          grepl("2 openings and 1 closure with a CCN absent", result$note, fixed = TRUE),
          !grepl("Exclusions: 0|; 0", result$note),
          !grepl("[%&_#$]", result$note))

# Exclusion precedence follows cleaning. Code 3 ("other") is retained by
# cleaning, so it is not an exclusion reason; code 10 (tribal) is. A specialty
# override does not override the ownership filter or the manual name list.
ids <- sprintf("%06d", 1:9)
raw <- raw_rows(ids, rep(2015L, 9),
                control = c(3, 10, 3, 3, 1, 1, NA, 1, 1),
                specialty_hosp = c(0, 0, 1, 1, 1, 0, NA, 0, 1),
                hospname = c("Other", "Tribal", "Specialty", "Override ownership",
                             "Override manual", "Manual", "Unknown specialty",
                             "Eligible", "Included"))
result <- hc_event_coverage(events(ids, rep(2015L, 9)), events(), events("000009", 2015L), raw,
                            include_names = c("Override ownership", "Override manual"),
                            exclude_names = c("Specialty", "Override ownership",
                                              "Override manual", "Manual", "Included"))
stopifnot(identical(result$events$reason,
                    c("other_eligibility", "ownership_filter", "specialty_filter",
                      "manual_filter", "manual_filter", "manual_filter",
                      "specialty_filter", "other_eligibility", "retained")),
          !grepl("other ownership", result$note, fixed = TRUE),
          grepl("1 opening excluded by the ownership filter (POS control code 10)",
                result$note, fixed = TRUE),
          grepl("2 openings excluded by the specialty-hospital filter", result$note, fixed = TRUE))

# Empty groups, a completely empty study window, and full retention are valid.
raw <- raw_rows("000001", 2011L)
result <- hc_event_coverage(events(), events("000001", 2011L), events("000001", 2011L), raw)
stopifnot(grepl("Of 0 openings and 1 closure", result$note, fixed = TRUE),
          !grepl("Exclusions:", result$note, fixed = TRUE))
empty <- hc_event_coverage(events(), events(), events(), raw_rows(character(), integer()))
stopifnot(nrow(empty$events) == 0L, nrow(empty$counts) == 0L,
          grepl("Of 0 openings and 0 closures", empty$note, fixed = TRUE),
          !grepl("Exclusions:", empty$note, fixed = TRUE))
outside <- hc_event_coverage(events("000001", 2009L), events("000001", 2024L),
                             events(), raw_rows(c("000001", "000001"), c(2009L, 2024L)))
stopifnot(nrow(outside$events) == 0L)

# Multiple events for one CCN in different years remain distinct event records;
# duplicate CCN/year entries within a type must fail instead of inflating counts.
repeat_events <- events(c("000001", "000001"), c(2011L, 2018L))
repeat_raw <- raw_rows(repeat_events$ccn, repeat_events$year)
result <- hc_event_coverage(repeat_events, events(), repeat_events, repeat_raw)
stopifnot(sum(result$counts$n) == 2L,
          sum(result$events$in_characteristics_table) == 2L)
duplicate <- events(c("000001", "000001"), c(2011L, 2011L))
expect_error(hc_event_coverage(duplicate, events(), events(), raw), "unique CCN/year")
expect_error(hc_event_coverage(events(), duplicate, events(), raw), "unique CCN/year")
expect_error(hc_event_coverage(events(), events(), duplicate, raw), "unique CCN/year")
expect_error(hc_event_coverage(events(), events(), events(), rbind(raw, raw)), "unique CCN/year")
expect_error(hc_event_coverage(events(NA_character_, 2011L), events(), events(), raw), "nonmissing character")
expect_error(hc_event_coverage(events("000001", NA_integer_), events(), events(), raw), "nonmissing integer")
expect_error(hc_event_coverage(events(1, 2011L), events(), events(), raw), "preserve leading zeros")
expect_error(hc_event_coverage(events(), events(), events(), transform(raw, year = NA_integer_)), "nonmissing integer")
expect_error(hc_event_coverage(events(), events(), events(), transform(raw, ccn = NA_character_)), "nonmissing character")
message("Hospital event coverage and note checks passed.")
