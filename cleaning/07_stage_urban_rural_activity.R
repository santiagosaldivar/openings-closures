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
#' @details The two-way urbanicity figure is rendered as faceted bars
#'   (openings and closures in separate panels); the three-way figure
#'   remains a line graph.
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
    select(hsanum, year, ruca_grouped, geography_type)

  events_labelled <- bind_rows(openings, closures) %>%
    filter(event_year >= 2010, !is.na(event_year)) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(hsa_assignment, by = c("hsanum", "event_year" = "year")) %>%
    filter(!is.na(geography_type))

  # Three-way descriptive breakdown. `ruca_grouped` is nested inside
  # `geography_type`, so the Metropolitan and Micropolitan counts below sum
  # exactly to the Urban counts in the two-way figure.
  event_processed <- events_labelled %>%
    group_by(event_year, ruca_grouped, event_type) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(event_year, ruca_grouped, event_type, fill = list(count = 0)) %>%
    mutate(
      ruca_grouped = factor(
        ruca_grouped,
        levels = c("Metropolitan", "Micropolitan", "Rural & Small Town")
      )
    )

  # Two-way pooled breakdown on the analysis variable.
  event_processed_2way <- events_labelled %>%
    group_by(event_year, geography_type, event_type) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(event_year, geography_type, event_type, fill = list(count = 0)) %>%
    mutate(
      geography_type = factor(
        geography_type,
        levels = c("Urban", "Rural & Small Town")
      )
    )

  verify_table <- event_processed %>%
    pivot_wider(
      names_from = c(ruca_grouped, event_type),
      values_from = count,
      values_fill = 0
    ) %>%
    arrange(event_year)

  verify_table_2way <- event_processed_2way %>%
    pivot_wider(
      names_from = c(geography_type, event_type),
      values_from = count,
      values_fill = 0
    ) %>%
    arrange(event_year)

  print(verify_table, n = Inf, width = Inf)
  print(verify_table_2way, n = Inf, width = Inf)

  build_activity_plot <- function(df, group_var,
                                  style = c("standard", "poster"),
                                  geom  = c("line", "bar")) {
    style <- match.arg(style)
    geom  <- match.arg(geom)
    
    if (geom == "bar") {
      # One panel. Two bars per year, side by side (openings left, closures
      # right), each stacked by urbanicity. Bar height is the year's total for
      # that event type. Fill carries both dimensions: blues for openings, reds
      # for closures, dark = Urban and light = Rural & Small Town within each.
      bar_width  <- 0.40
      bar_offset <- 0.22
      
      df <- df %>%
        mutate(
          event_type = factor(event_type, levels = c("Opening", "Closure")),
          x_pos = event_year + ifelse(event_type == "Opening", -bar_offset, bar_offset),
          fill_key = factor(
            paste0(as.character(.data[[group_var]]), " | ", as.character(event_type)),
            levels = c(
              "Urban | Opening", "Rural & Small Town | Opening",
              "Urban | Closure", "Rural & Small Town | Closure"
            )
          ),
          # White numerals wash out on the two light fills.
          label_col = ifelse(grepl("^Urban", as.character(.data[[group_var]])),
                             "white", "grey20")
        )
      base <- ggplot(
        df,
        aes(x = x_pos, y = count, fill = fill_key)
      ) +
        geom_col(
          position = position_stack(reverse = TRUE),
          width = bar_width,
          color = "white",
          linewidth = 0.25
        ) +
        geom_text(
          aes(label = ifelse(count >= 4, count, ""), color = label_col,
              group = fill_key),
          position = position_stack(reverse = TRUE, vjust = 0.5),
          size = if (style == "standard") 3.0 else 2.5,
          show.legend = FALSE
        ) +
        scale_color_identity() +
        scale_fill_manual(
          values = c(
            "Urban | Opening"              = "#002a85",
            "Rural & Small Town | Opening" = "#0091ea",
            "Urban | Closure"              = "#D73027", # Alternatively: ee2400
            "Rural & Small Town | Closure" = "#ffb09c"
          ),
          labels = c(
            "Openings (Urban)", "Openings (Rural)",
            "Closures (Urban)", "Closures (Rural)"
          ),
          name = NULL,
          guide = guide_legend(nrow = 2)
        ) +
        scale_x_continuous(
          breaks = seq(min(df$event_year), max(df$event_year), by = 1),
          minor_breaks = NULL
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(
          x = "Year",
          y = "Number of Events"
        )
    } else {
      base <- ggplot(
        df,
        aes(x = event_year, y = count, color = .data[[group_var]], linetype = event_type)
      ) +
        geom_line(linewidth = if (style == "standard") 0.95 else 0.65) +
        geom_point(size = if (style == "standard") 2.6 else 1.8) +
        scale_color_brewer(palette = "Set1") +
        scale_linetype_manual(values = c("Opening" = "solid", "Closure" = "dashed")) +
        labs(
          x = "Year",
          y = "Number of Events",
          color = "Urbanicity",
          linetype = "Event Type"
        )
      
      if (style == "standard") {
        base <- base +
          scale_x_continuous(breaks = seq(min(df$event_year), max(df$event_year), by = 2)) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.08)))
      }
    }
    
    if (style == "standard") {
      p <- base +
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
    } else {
      p <- base +
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
    }
    
    if (geom == "bar") {
      p <- p + theme(
        panel.grid = element_blank(),
        axis.line.y = element_line(color = "grey30", linewidth = 0.4),
        axis.ticks.y = element_line(color = "grey30", linewidth = 0.3),
        axis.line.x = element_line(color = "grey30", linewidth = 0.4),
        axis.ticks.x = element_line(color = "grey30", linewidth = 0.3),
        axis.text.x = element_text(
          angle = 45, hjust = 1,
          size = if (style == "standard") 12 else 9
        )
      )
    }
    
    p
  }

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  save_activity_set <- function(df, group_var, stub, geom = c("line", "bar")) {
    geom <- match.arg(geom)
    paths <- c(
      standard = file.path(dest_dir, paste0(stub, ".png")),
      poster = file.path(dest_dir, paste0(stub, "_poster.png")),
      poster_hires = file.path(dest_dir, paste0(stub, "_poster_hires.png"))
    )
    p_standard <- build_activity_plot(df, group_var, "standard", geom)
    p_poster <- build_activity_plot(df, group_var, "poster", geom)
    
    # Faceted bars need more horizontal room than the single-panel line version.
    w_standard <- if (geom == "bar") 11.0 else 8.4
    w_poster   <- if (geom == "bar") 11.0 else 8.0
    w_hires    <- if (geom == "bar") 15.0 else 12.0
    
    ggsave(paths[["standard"]], plot = p_standard, width = w_standard, height = 5.6, dpi = 300)
    ggsave(paths[["poster"]], plot = p_poster, width = w_poster, height = 5, dpi = 300)
    ggsave(paths[["poster_hires"]], plot = p_poster, width = w_hires, height = 7.5, dpi = 600)
    paths
  }

  paths_3way <- save_activity_set(
    event_processed, "ruca_grouped", "combined_hospital_activity"
  )
  paths_2way <- save_activity_set(
    event_processed_2way, "geography_type", "combined_hospital_activity_2way",
    geom = "bar"
  )

  invisible(c(
    standard = unname(paths_3way[["standard"]]),
    poster = unname(paths_3way[["poster"]]),
    poster_hires = unname(paths_3way[["poster_hires"]]),
    standard_2way = unname(paths_2way[["standard"]]),
    poster_2way = unname(paths_2way[["poster"]]),
    poster_hires_2way = unname(paths_2way[["poster_hires"]])
  ))
}
