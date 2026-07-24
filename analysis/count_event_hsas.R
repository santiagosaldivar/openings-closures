# count_events_hsas.R
# Prints number of openings, closures, and unique HSAs in which they occur.
# Mirrors the event-reading logic in build_hsa_panel_assignment(method = "event_zip").

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr)
})

# --- File paths (set to match your pipeline) ---------------------------------
openings_file  <- "data/raw/updated_openings_august2025.csv"
closures_file  <- "data/raw/updated_closures_august2025.csv"
crosswalk_file <- "data/raw/ZipHsaHrr.csv"  # <-- set this
source("cleaning/00_shared_urbanicity_helpers.R", local = FALSE)

# --- Read events exactly as the panel-assignment code does -------------------
openings <- read_csv(openings_file, show_col_types = FALSE) %>%
  transmute(
    zip5  = str_pad(as.character(zip5), 5, "left", "0"),
    year  = as.integer(part_year),
    group = "Opening"
  ) %>%
  filter(year >= 2010, !is.na(year))

closures <- read_csv(closures_file, show_col_types = FALSE) %>%
  transmute(
    zip5  = str_pad(as.character(zip5), 5, "left", "0"),
    year  = as.integer(term_year),
    group = "Closure"
  ) %>%
  filter(year >= 2010, !is.na(year))

events <- bind_rows(openings, closures) %>%
  left_join(ocgh_load_zip_hsa_lookup(crosswalk_file), by = "zip5")

# --- Counts ------------------------------------------------------------------
placed    <- events %>% filter(!is.na(hsanum))
n_open    <- sum(events$group == "Opening")
n_close   <- sum(events$group == "Closure")
n_nomatch <- sum(is.na(events$hsanum))

cat("Openings (rows, year >= 2010): ", n_open, "\n", sep = "")
cat("Closures (rows, year >= 2010): ", n_close, "\n", sep = "")
cat("Total events:                  ", n_open + n_close, "\n\n", sep = "")

cat("Unique HSAs (any event):       ", n_distinct(placed$hsanum), "\n", sep = "")
cat("Unique HSAs with an opening:   ",
    n_distinct(placed$hsanum[placed$group == "Opening"]), "\n", sep = "")
cat("Unique HSAs with a closure:    ",
    n_distinct(placed$hsanum[placed$group == "Closure"]), "\n\n", sep = "")

cat("Events with no HSA match (ZIP not in crosswalk): ", n_nomatch, "\n", sep = "")