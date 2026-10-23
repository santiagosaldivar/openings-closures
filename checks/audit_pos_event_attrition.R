# Audit event losses between the curated event lists and the characteristics
# table's exact CCN/year match. Run from the project root with:
# Rscript checks/audit_pos_event_attrition.R
# Writes diagnostics only; does not regenerate data or manuscript outputs.
# Aggregate stage, reason, and opening-gap counts are versioned. Hospital-level
# diagnostics are written beside them but remain ignored by Git.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(haven)
})

raw <- read_dta("data/raw/pos_panel_2009_2024.dta")
include_names <- read_csv("data/raw/pos_do_not_exclude.csv", show_col_types = FALSE)$Name
exclude_names <- read_csv("data/raw/POS_double_checking_exclude.csv", show_col_types = FALSE)$name
clean <- read_csv("data/processed/pos_panel_updated.csv", show_col_types = FALSE) %>%
  filter(between(year, 2010, 2023))
events <- bind_rows(
  read_csv("data/interim/openings_clean.csv", show_col_types = FALSE) %>%
    filter(opening == 1) %>%
    transmute(event_type = "Opening", ccn, hospital = hospname, event_year = part_year),
  read_csv("data/interim/closures_clean.csv", show_col_types = FALSE) %>%
    filter(closure == 1) %>%
    transmute(event_type = "Closure", ccn, hospital = hospname, event_year = term_year)
) %>% filter(between(event_year, 2010, 2023))

# Apply the same rules, in the same order, as cleaning/02_clean_pos.R.
after_specialty <- raw %>%
  mutate(specialty_hosp = if_else(hospname %in% include_names, 0, specialty_hosp),
         year = as.integer(year)) %>%
  filter(specialty_hosp != 1, between(year, 2009, 2023))
after_control <- after_specialty %>%
  filter(is.na(control) | control != 10)
rebuilt <- after_control %>%
  filter(!hospname %in% exclude_names, year >= 2010)

stopifnot(
  nrow(rebuilt) == nrow(clean),
  nrow(anti_join(rebuilt, clean, by = c("ccn", "year"))) == 0L,
  nrow(anti_join(clean, rebuilt, by = c("ccn", "year"))) == 0L,
  anyDuplicated(raw[c("ccn", "year")]) == 0L,
  anyDuplicated(events[c("event_type", "ccn")]) == 0L
)

history <- raw %>% group_by(ccn) %>% summarise(
  first_raw_year = min(year), last_raw_year = max(year),
  raw_years = paste(sort(unique(year)), collapse = ";"),
  raw_part_years = paste(sort(unique(part_year)), collapse = ";"),
  raw_term_years = paste(sort(unique(term_year)), collapse = ";"),
  .groups = "drop"
)
clean_history <- clean %>% group_by(ccn) %>% summarise(
  clean_years = paste(sort(unique(year)), collapse = ";"), .groups = "drop"
)
event_rows <- raw %>% transmute(
  ccn, event_year = year, raw_event_year_present = TRUE,
  raw_event_hospital = hospname,
  raw_event_control = as.numeric(control),
  raw_event_control_label = as.character(as_factor(control)),
  raw_event_specialty_hosp = specialty_hosp,
  on_include_name_list = hospname %in% include_names,
  on_exclude_name_list = hospname %in% exclude_names
)

audit <- events %>%
  left_join(history, by = "ccn") %>%
  left_join(clean_history, by = "ccn") %>%
  left_join(event_rows, by = c("ccn", "event_year")) %>%
  left_join(clean %>% distinct(ccn, event_year = year) %>%
              mutate(in_characteristics_table = TRUE), by = c("ccn", "event_year")) %>%
  mutate(
    raw_event_year_present = coalesce(raw_event_year_present, FALSE),
    in_characteristics_table = coalesce(in_characteristics_table, FALSE),
    first_raw_year_minus_event_year = first_raw_year - event_year,
    reason = case_when(
      in_characteristics_table ~ "Retained in characteristics table",
      is.na(first_raw_year) ~ "CCN absent from raw POS panel",
      !raw_event_year_present ~ "Event-year row absent from raw POS panel",
      !on_include_name_list & (is.na(raw_event_specialty_hosp) | raw_event_specialty_hosp == 1) ~
        "Removed by specialty hospital filter",
      raw_event_control %in% 10 ~ "Removed by ownership/control filter",
      on_exclude_name_list ~ "Removed by manual hospital-name exclusion list",
      TRUE ~ "Unexplained: review"
    )
  ) %>% arrange(event_type, event_year, ccn)
stopifnot(!any(audit$reason == "Unexplained: review"))

count_stage <- function(panel, label) {
  events %>% semi_join(panel, by = c("ccn", "event_year" = "year")) %>%
    count(event_type, name = "n") %>% mutate(stage = label, .before = 1)
}
stages <- bind_rows(
  events %>% count(event_type, name = "n") %>% mutate(stage = "Curated events", .before = 1),
  count_stage(raw, "Exact event-year match in raw POS"),
  count_stage(after_specialty, "After specialty and year filters"),
  count_stage(after_control, "After ownership/control filter"),
  count_stage(rebuilt, "After manual exclusions: characteristics table")
)
reasons <- audit %>% count(event_type, reason, name = "n")
opening_gaps <- audit %>%
  filter(event_type == "Opening", !raw_event_year_present, !is.na(first_raw_year)) %>%
  count(first_raw_year_minus_event_year, name = "n")

out_dir <- "checks/output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(audit, file.path(out_dir, "pos_event_attrition_all.csv"))
write_csv(filter(audit, !in_characteristics_table), file.path(out_dir, "pos_event_attrition_excluded.csv"))
write_csv(stages, file.path(out_dir, "pos_event_attrition_stages.csv"))
write_csv(reasons, file.path(out_dir, "pos_event_attrition_reasons.csv"))
write_csv(opening_gaps, file.path(out_dir, "pos_event_attrition_opening_gaps.csv"))
print(stages, n = Inf)
print(reasons, n = Inf)
print(opening_gaps, n = Inf)
message("Rebuilt cleaned panel matches existing panel row count and CCN/year keys.")
