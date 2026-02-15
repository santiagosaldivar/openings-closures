# Build combined hospital activity figure from opening/closure events and RUCA codes.
# Usage: source("cleaning/07_stage_urban_rural_activity.R"); stage_urban_rural_activity()

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

resolve_input_path <- function(primary_path, label) {
  if (file.exists(primary_path)) return(primary_path)
  stop(label, " not found at: ", primary_path)
}

#' Build combined hospital activity PNG into outputs
#'
#' @param openings_file Preferred openings CSV path
#' @param closures_file Preferred closures CSV path
#' @param ruca_file Preferred RUCA Excel path
#' @param dest_dir Destination directory (default "outputs/figures")
#' @return destination path invisibly
stage_urban_rural_activity <- function(
  openings_file = "data/raw/updated_openings_august2025.csv",
  closures_file = "data/raw/updated_closures_august2025.csv",
  ruca_file = "data/raw/RUCA2010zipcode.xlsx",
  dest_dir = "outputs/figures"
) {
  openings_path <- resolve_input_path(openings_file, "Openings file")
  closures_path <- resolve_input_path(closures_file, "Closures file")
  ruca_path <- resolve_input_path(ruca_file, "RUCA file")

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

  ruca <- read_excel(ruca_path, sheet = "Data") %>%
    transmute(
      zip5 = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      ruca_simple = case_when(
        RUCA1 %in% c(1, 2, 3) ~ "Metropolitan",
        RUCA1 %in% c(4, 5, 6) ~ "Micropolitan",
        RUCA1 %in% c(7, 8, 9) ~ "Small Town",
        RUCA1 == 10 ~ "Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    distinct()

  event_processed <- bind_rows(openings, closures) %>%
    filter(event_year >= 2010, !is.na(event_year)) %>%
    left_join(ruca, by = "zip5") %>%
    filter(!is.na(ruca_simple)) %>%
    mutate(
      ruca_grouped = case_when(
        ruca_simple %in% c("Rural", "Small Town") ~ "Rural & Small Town",
        TRUE ~ ruca_simple
      )
    ) %>%
    group_by(event_year, ruca_grouped, event_type) %>%
    summarise(count = n(), .groups = "drop")

  p_combined <- ggplot(
    event_processed,
    aes(x = event_year, y = count, color = ruca_grouped, linetype = event_type)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set1") +
    scale_linetype_manual(values = c("Opening" = "solid", "Closure" = "dashed")) +
    labs(
      x = "Year",
      y = "Number of Events",
      color = "Urbanicity",
      linetype = "Event Type"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest_path <- file.path(dest_dir, "combined_hospital_activity.png")
  ggsave(dest_path, plot = p_combined, width = 7, height = 5, dpi = 300)
  invisible(dest_path)
}
