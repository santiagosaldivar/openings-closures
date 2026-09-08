# Baseline hospital counts and opening percentages using hospital ZIP RUCA.
# Moved from archive/openings_closings_code/baseline_urbanicity_split.R.
# Run from the project root:
# source("analysis/baseline_urbanicity_split.R"); run_baseline_urbanicity_split()
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(ggplot2)
})

# The cleaned POS panel already applies the original script's inclusion rules.
# Preserve its active flag (use pos_panel_updated, before reconciliation).
# Urban = metropolitan + micropolitan, using each hospital's ZIP, not HSA RUCA.
# Opening percentages are cumulative events / baseline hospitals, not annual rates.
run_baseline_urbanicity_split <- function(
    pos_path = "data/processed/pos_panel_updated.csv",
    openings_path = "data/interim/openings_clean.csv",
    ruca_path = "data/raw/RUCA2010zipcode.xlsx",
    baseline_year = 2010L,
    event_years = 2010:2023,
    out_dir = "outputs/tables",
    out_fig = "outputs/figures/ruca_distribution_baseline.png"
) {
  zip5 <- function(x) str_pad(as.character(x), 5, pad = "0")
  ruca <- read_excel(ruca_path, sheet = "Data") %>%
    transmute(zip5 = zip5(ZIP_CODE), category = case_when(
      RUCA1 %in% 1:3 ~ "Metropolitan",
      RUCA1 %in% 4:6 ~ "Micropolitan",
      RUCA1 %in% 7:9 ~ "Small Town",
      RUCA1 == 10 ~ "Rural",
      TRUE ~ "Unmatched RUCA"
    )) %>% distinct()
  if (anyDuplicated(ruca$zip5)) stop("Conflicting RUCA assignments for a ZIP.")

  baseline <- read_csv(pos_path, show_col_types = FALSE) %>%
    filter(year == baseline_year, active == 1) %>%
    mutate(zip5 = zip5(zip5)) %>%
    left_join(ruca, by = "zip5") %>%
    distinct(ccn, .keep_all = TRUE) %>%
    mutate(category = coalesce(category, "Unmatched RUCA"))
  if (nrow(baseline) == 0) stop("No active hospitals in baseline year.")
  openings <- read_csv(openings_path, show_col_types = FALSE) %>%
    transmute(ccn, zip5 = zip5(zip5), year = as.integer(part_year)) %>%
    filter(year %in% event_years) %>% distinct() %>%
    left_join(ruca, by = "zip5") %>%
    mutate(category = coalesce(category, "Unmatched RUCA"))

  groups <- list(
    Metropolitan = "Metropolitan", Micropolitan = "Micropolitan",
    `Small Town` = "Small Town", Rural = "Rural",
    Urban = c("Metropolitan", "Micropolitan"),
    `Rural & Small Town` = c("Rural", "Small Town"),
    `Unmatched RUCA` = "Unmatched RUCA"
  )
  counts <- bind_rows(lapply(names(groups), function(label) {
    members <- groups[[label]]
    n_baseline <- sum(baseline$category %in% members)
    n_openings <- sum(openings$category %in% members)
    tibble(category = label, baseline_year = baseline_year,
           event_start_year = min(event_years), event_end_year = max(event_years),
           baseline_hospitals = n_baseline, openings = n_openings,
           openings_pct_baseline = if (n_baseline > 0) 100 * n_openings / n_baseline else NA_real_,
           classification = "Hospital ZIP RUCA 2010")
  }))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(counts, file.path(out_dir, "baseline_urbanicity_split.csv"))
  plot_data <- counts %>% filter(category != "Unmatched RUCA") %>%
    mutate(breakdown = if_else(category %in% c("Urban", "Rural & Small Town"),
                               "Combined categories", "Detailed categories"),
           category = factor(category, levels = names(groups)))
  p <- ggplot(plot_data, aes(category, baseline_hospitals)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = baseline_hospitals), vjust = -0.4) +
    facet_wrap(~ breakdown, scales = "free_x") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = paste("Active hospitals at baseline:", baseline_year),
         subtitle = "Unique CCNs classified by hospital ZIP RUCA; urban includes micropolitan",
         x = NULL, y = "Hospitals") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  dir.create(dirname(out_fig), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_fig, p, width = 11, height = 5, dpi = 300, bg = "white")
  print(counts, n = Inf, width = Inf)
  invisible(counts)
}
