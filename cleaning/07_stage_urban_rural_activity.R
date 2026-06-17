# Build combined hospital activity figure from opening/closure events and RUCA codes.
# Usage: source("cleaning/07_stage_urban_rural_activity.R"); stage_urban_rural_activity()

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tidyr)
})

shared_helper_path <- "cleaning/00_shared_urbanicity_helpers.R"
if (!exists("ocgh_build_hsa_year_ruca_assignment") && file.exists(shared_helper_path)) {
  source(shared_helper_path, local = FALSE)
}

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
  crosswalk_file = "data/raw/ZipHsaHrr.csv",
  zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
  census_root = "data/raw/census_raw_data",
  dest_dir = "outputs/figures"
) {
  openings_path <- resolve_input_path(openings_file, "Openings file")
  closures_path <- resolve_input_path(closures_file, "Closures file")
  ruca_path <- resolve_input_path(ruca_file, "RUCA file")
  crosswalk_path <- resolve_input_path(crosswalk_file, "ZIP-HSA crosswalk")

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

  zip_hsa <- ocgh_load_zip_hsa_lookup(crosswalk_path)
  years_to_assign <- bind_rows(openings, closures) %>%
    filter(event_year >= 2010, !is.na(event_year)) %>%
    distinct(year = event_year) %>%
    pull(year)

  hsa_assignment <- ocgh_build_hsa_year_ruca_assignment(
    years = years_to_assign,
    crosswalk_file = crosswalk_path,
    ruca_file = ruca_path,
    method = "hsa_population_weighted",
    zip_zcta_file = zip_zcta_file,
    census_root = census_root
  ) %>%
    select(hsanum, year, ruca_grouped)

  event_processed <- bind_rows(openings, closures) %>%
    filter(event_year >= 2010, !is.na(event_year)) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(hsa_assignment, by = c("hsanum", "event_year" = "year")) %>%
    filter(!is.na(ruca_grouped)) %>%
    group_by(event_year, ruca_grouped, event_type) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(event_year, ruca_grouped, event_type, fill = list(count = 0)) %>%
    mutate(
      ruca_grouped = factor(
        ruca_grouped,
        levels = c("Metropolitan", "Micropolitan", "Rural & Small Town")
      )
    )

  verify_table <- event_processed %>%
    pivot_wider(
      names_from = c(ruca_grouped, event_type),
      values_from = count,
      values_fill = 0
    ) %>%
    arrange(event_year)
  
  print(verify_table, n = Inf, width = Inf)
  
  p_combined <- ggplot(
    event_processed,
    aes(x = event_year, y = count, color = ruca_grouped, linetype = event_type)
  ) +
    geom_line(linewidth = 0.95) +
    geom_point(size = 2.6) +
    scale_color_brewer(palette = "Set1") +
    scale_linetype_manual(values = c("Opening" = "solid", "Closure" = "dashed")) +
    labs(
      x = "Year",
      y = "Number of Events",
      color = "Urbanicity",
      linetype = "Event Type"
    ) +
    scale_x_continuous(breaks = seq(min(event_processed$event_year), max(event_processed$event_year), by = 2)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    theme_minimal(base_family = "Times New Roman", base_size = 16) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(t = 0),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 13),
      legend.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(size = 17),
      axis.text = element_text(size = 13),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(8, 8, 16, 8)
    )

  p_combined_poster <- ggplot(
    event_processed,
    aes(x = event_year, y = count, color = ruca_grouped, linetype = event_type)
  ) +
    geom_line(linewidth = 0.65) +
    geom_point(size = 1.8) +
    scale_color_brewer(palette = "Set1") +
    scale_linetype_manual(values = c("Opening" = "solid", "Closure" = "dashed")) +
    labs(
      x = "Year",
      y = "Number of Events",
      color = "Urbanicity",
      linetype = "Event Type"
    ) +
    theme_minimal(base_family = "Arial", base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(t = 0),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest_path <- file.path(dest_dir, "combined_hospital_activity.png")
  poster_dest_path <- file.path(dest_dir, "combined_hospital_activity_poster.png")
  poster_hires_dest_path <- file.path(dest_dir, "combined_hospital_activity_poster_hires.png")
  ggsave(dest_path, plot = p_combined, width = 8.4, height = 5.6, dpi = 300)
  ggsave(poster_dest_path, plot = p_combined_poster, width = 8, height = 5, dpi = 300)
  ggsave(poster_hires_dest_path, plot = p_combined_poster, width = 12, height = 7.5, dpi = 600)
  invisible(c(standard = dest_path, poster = poster_dest_path, poster_hires = poster_hires_dest_path))
}
