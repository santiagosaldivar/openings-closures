suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tidyr)
})

build_event_counts_by_census_region <- function(
  openings_path = "data/interim/openings_clean.csv",
  closures_path = "data/interim/closures_clean.csv",
  zip_hsa_path = "data/raw/ZipHsaHrr.csv",
  census_region_path = "data/raw/census_division_crosswalk.csv",
  out_csv = "outputs/tables/census_region/opening_closure_counts_by_census_region.csv",
  out_plot = "outputs/figures/census_region/opening_closure_counts_by_census_region.png",
  out_division_csv = "outputs/tables/census_region/opening_closure_counts_by_census_division.csv",
  out_division_plot = "outputs/figures/census_region/opening_closure_counts_by_census_division.png",
  out_plot_poster = "outputs/figures/census_region/opening_closure_counts_by_census_region_poster.png",
  out_division_plot_poster = "outputs/figures/census_region/opening_closure_counts_by_census_division_poster.png",
  out_plot_poster_hires = "outputs/figures/census_region/opening_closure_counts_by_census_region_poster_hires.png",
  out_division_plot_poster_hires = "outputs/figures/census_region/opening_closure_counts_by_census_division_poster_hires.png"
) {
  if (!file.exists(openings_path)) stop("Openings file not found: ", openings_path)
  if (!file.exists(closures_path)) stop("Closures file not found: ", closures_path)
  if (!file.exists(zip_hsa_path)) stop("ZIP-HSA crosswalk not found: ", zip_hsa_path)
  if (!file.exists(census_region_path)) stop("Census region crosswalk not found: ", census_region_path)

  openings <- read_csv(openings_path, show_col_types = FALSE) %>%
    transmute(
      ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"),
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(part_year),
      event_type = "Opening"
    ) %>%
    filter(year >= 2010, !is.na(year)) %>%
    distinct()

  closures <- read_csv(closures_path, show_col_types = FALSE) %>%
    transmute(
      ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"),
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(term_year),
      event_type = "Closure"
    ) %>%
    filter(year >= 2010, !is.na(year)) %>%
    distinct()

  zip_hsa <- read_csv(zip_hsa_path, show_col_types = FALSE) %>%
    transmute(
      zip5 = str_pad(as.character(zipcode19), width = 5, side = "left", pad = "0"),
      hsastate = str_to_upper(str_trim(as.character(hsastate)))
    ) %>%
    distinct(zip5, .keep_all = TRUE)

  census_regions <- read_csv(census_region_path, show_col_types = FALSE, name_repair = "unique_quiet") %>%
    select(State, Division, Region) %>%
    mutate(
      State = str_to_upper(str_trim(as.character(State))),
      Division = str_trim(as.character(Division)),
      Region = str_trim(as.character(Region))
    ) %>%
    filter(!is.na(State), State != "") %>%
    distinct(State, .keep_all = TRUE)

  events_with_census <- bind_rows(openings, closures) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(census_regions, by = c("hsastate" = "State")) %>%
    mutate(
      Region = coalesce(Region, "Unmatched"),
      Division = coalesce(Division, "Unmatched")
    )

  build_counts <- function(data, geography_col, levels) {
    data %>%
      count(.data[[geography_col]], event_type, name = "n") %>%
      complete(
        !!sym(geography_col) := levels,
        event_type = c("Opening", "Closure"),
        fill = list(n = 0L)
      ) %>%
      pivot_wider(names_from = event_type, values_from = n) %>%
      mutate(
        Total = Opening + Closure,
        !!sym(geography_col) := factor(.data[[geography_col]], levels = levels)
      ) %>%
      arrange(.data[[geography_col]]) %>%
      mutate(!!sym(geography_col) := as.character(.data[[geography_col]]))
  }

  write_counts_plot <- function(
    counts,
    geography_col,
    levels,
    title,
    x_label,
    out_path,
    x_text_angle = 0,
    font_family = "Arial",
    base_size = 12,
    title_size = 13,
    axis_title_size = 12,
    axis_text_size = 10,
    legend_text_size = 10,
    label_size = 3.2,
    width = 8,
    height = 4.8,
    dpi = 300
  ) {
    plot_counts <- counts %>%
      filter(.data[[geography_col]] != "Unmatched" | Total > 0) %>%
      select(all_of(geography_col), Opening, Closure) %>%
      pivot_longer(c(Opening, Closure), names_to = "event_type", values_to = "n") %>%
      mutate(
        !!sym(geography_col) := factor(.data[[geography_col]], levels = levels),
        event_type = factor(event_type, levels = c("Opening", "Closure"))
      )

    p <- ggplot(plot_counts, aes(x = .data[[geography_col]], y = n, fill = event_type)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64) +
      geom_text(
        aes(label = n),
        position = position_dodge(width = 0.72),
        vjust = -0.35,
        size = label_size
      ) +
      scale_fill_manual(values = c(Opening = "#5DA5DA", Closure = "#1F5A99"), name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
      labs(
        title = title,
        x = x_label,
        y = "Count"
      ) +
      theme_minimal(base_family = font_family, base_size = base_size) +
      theme(
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text.x = element_text(color = "grey20", angle = x_text_angle, hjust = ifelse(x_text_angle == 0, 0.5, 1), size = axis_text_size),
        axis.text.y = element_text(color = "grey20", size = axis_text_size),
        legend.text = element_text(size = legend_text_size),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)
      )

    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggsave(out_path, plot = p, width = width, height = height, dpi = dpi)
  }

  region_levels <- c("Northeast", "Midwest", "South", "West", "Unmatched")
  division_levels <- c(
    "New England",
    "Middle Atlantic",
    "East North Central",
    "West North Central",
    "South Atlantic",
    "East South Central",
    "West South Central",
    "Mountain",
    "Pacific",
    "Unmatched"
  )

  counts <- events_with_census %>%
    build_counts("Region", region_levels)

  division_counts <- events_with_census %>%
    build_counts("Division", division_levels)

  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  write_csv(counts, out_csv)

  dir.create(dirname(out_division_csv), recursive = TRUE, showWarnings = FALSE)
  write_csv(division_counts, out_division_csv)

  write_counts_plot(
    counts = counts,
    geography_col = "Region",
    levels = region_levels,
    title = "Opening and Closure Counts by Census Region",
    x_label = NULL,
    out_path = out_plot,
    font_family = "Times New Roman",
    base_size = 14,
    title_size = 15,
    axis_title_size = 14,
    axis_text_size = 12,
    legend_text_size = 12,
    label_size = 3.6,
    width = 8.4,
    height = 5.4
  )

  write_counts_plot(
    counts = counts,
    geography_col = "Region",
    levels = region_levels,
    title = NULL,
    x_label = NULL,
    out_path = out_plot_poster
  )

  write_counts_plot(
    counts = counts,
    geography_col = "Region",
    levels = region_levels,
    title = NULL,
    x_label = NULL,
    out_path = out_plot_poster_hires,
    width = 12,
    height = 7.2,
    dpi = 600
  )

  write_counts_plot(
    counts = division_counts,
    geography_col = "Division",
    levels = division_levels,
    title = "Opening and Closure Counts by Census Division",
    x_label = NULL,
    out_path = out_division_plot,
    x_text_angle = 35,
    font_family = "Times New Roman",
    base_size = 14,
    title_size = 15,
    axis_title_size = 14,
    axis_text_size = 11,
    legend_text_size = 12,
    label_size = 3.2,
    width = 9.2,
    height = 5.6
  )

  write_counts_plot(
    counts = division_counts,
    geography_col = "Division",
    levels = division_levels,
    title = NULL,
    x_label = NULL,
    out_path = out_division_plot_poster,
    x_text_angle = 35
  )

  write_counts_plot(
    counts = division_counts,
    geography_col = "Division",
    levels = division_levels,
    title = NULL,
    x_label = NULL,
    out_path = out_division_plot_poster_hires,
    x_text_angle = 35,
    width = 12,
    height = 7.2,
    dpi = 600
  )

  invisible(list(
    region_counts = counts,
    division_counts = division_counts,
    outputs = c(
      region_csv = out_csv,
      region_plot = out_plot,
      region_plot_poster = out_plot_poster,
      region_plot_poster_hires = out_plot_poster_hires,
      division_csv = out_division_csv,
      division_plot = out_division_plot,
      division_plot_poster = out_division_plot_poster,
      division_plot_poster_hires = out_division_plot_poster_hires
    )
  ))
}
