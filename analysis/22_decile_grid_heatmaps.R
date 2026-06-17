# Decile x decile grid heatmaps of hospital openings and closures.
#
# One axis is the HSA decile of the below-poverty-line variable; the other is
# the HSA decile of the population-change variable. Cells are shaded by the
# number of opening or closure EVENTS whose HSA falls in that decile pair.
#
# Counting grain (one row per actual event; no de-duplication):
#   - two closures in the same HSA in the same year  -> +2 to that cell (closures)
#   - two closures in the same HSA in different years -> each counted separately
#   - one opening + one closure in an HSA            -> +1 to each panel, independently
# Event counts therefore come from the RAW event files (one row per event), not
# the HSA-year-grain staged percentile file (which would collapse same-year
# same-HSA events). The staged file is used only as the lookup that attaches
# each event's HSA-year percentiles, and as the full HSA-year universe for the
# per-cell n denominator.
#
# Deciles are event-year within-year percentiles binned into 1-10; an HSA's
# decile is an event-year property and can differ across that HSA's events.
#
# Usage: source("R/analysis/22_decile_grid_heatmaps.R"); run_decile_grid_heatmaps()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(patchwork)
})

shared_helper_path <- "cleaning/00_shared_urbanicity_helpers.R"
if (!exists("ocgh_load_zip_hsa_lookup") && file.exists(shared_helper_path)) {
  source(shared_helper_path, local = FALSE)
}

#' Build decile-grid heatmaps of openings and closures.
#'
#' @param input_csv staged percentile dataset (HSA-year grain; supplies the
#'   percentile lookup and the full HSA-year universe for the denominator)
#' @param out_fig_dir directory for figure, CSV, and standalone .tex outputs
#' @param openings_file,closures_file raw event files (one row per event)
#' @param crosswalk_file ZIP-to-HSA crosswalk (ZipHsaHrr)
#' @param x_var,y_var percentile columns to bin into deciles
#' @param x_label,y_label axis labels
#' @param cell_n which denominator to print in each cell: distinct HSAs in the
#'   cell across the study, or pooled HSA-years in the cell
#' @param open_color,close_color event hues (match the rest of the paper)
#' @param font_family base font; dpi export resolution
#' @return list of grid data and plots (invisible)
run_decile_grid_heatmaps <- function(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir = "outputs/figures/decile_grids",
    openings_file = "data/raw/updated_openings_august2025.csv",
    closures_file = "data/raw/updated_closures_august2025.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    x_var = "poverty_percentile",
    y_var = "pop_change_pct_percentile",
    x_label = "Below-poverty-line decile (HSA, within-year)",
    y_label = "Population-change decile (HSA, within-year)",
    cell_n = c("hsa", "hsa_year"),
    open_color = "#2166AC",
    close_color = "#D73027",
    font_family = "Times",
    dpi = 300
) {
  cell_n <- match.arg(cell_n)
  if (!file.exists(input_csv)) stop("Staged percentile file not found: ", input_csv)
  if (!file.exists(openings_file)) stop("Openings file not found: ", openings_file)
  if (!file.exists(closures_file)) stop("Closures file not found: ", closures_file)
  if (!exists("ocgh_load_zip_hsa_lookup")) {
    stop("Shared helper ocgh_load_zip_hsa_lookup() not found; source ", shared_helper_path)
  }
  
  # --- Decile binning (event-year within-year percentile -> 1..10) ---
  to_decile <- function(p) {
    mx <- suppressWarnings(max(p, na.rm = TRUE))
    if (is.finite(mx) && mx <= 1) p <- p * 100  # guard if stored on 0-1 scale
    cut(p, breaks = seq(0, 100, by = 10), include.lowest = TRUE, labels = FALSE)
  }
  
  # --- Percentile lookup + full HSA-year universe (from staged file) ---
  staged <- read_csv(input_csv, show_col_types = FALSE)
  if (!all(c(x_var, y_var, "hsanum", "year") %in% names(staged))) {
    stop("Staged file missing one of: hsanum, year, ", x_var, ", ", y_var)
  }
  lookup <- staged %>%
    transmute(
      hsanum,
      year = as.integer(year),
      x_pct = .data[[x_var]],
      y_pct = .data[[y_var]]
    ) %>%
    distinct(hsanum, year, .keep_all = TRUE) %>%
    mutate(in_lookup = TRUE)
  
  universe <- lookup %>%
    mutate(x_dec = to_decile(x_pct), y_dec = to_decile(y_pct)) %>%
    filter(!is.na(x_dec), !is.na(y_dec))
  
  cell_universe <- universe %>%
    group_by(x_dec, y_dec) %>%
    summarise(
      n_hsa = n_distinct(hsanum),
      n_hsa_year = n(),
      .groups = "drop"
    )
  
  # --- Event counts from raw files (one row per event) ---
  zip_hsa <- ocgh_load_zip_hsa_lookup(crosswalk_file)
  
  read_events <- function(path, year_col, group_label) {
    read_csv(path, show_col_types = FALSE) %>%
      transmute(
        zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
        year = as.integer(.data[[year_col]]),
        group = group_label
      ) %>%
      filter(year >= 2010, !is.na(year))
  }
  
  events_raw <- bind_rows(
    read_events(openings_file, "part_year", "Opening"),
    read_events(closures_file, "term_year", "Closure")
  ) %>%
    left_join(zip_hsa, by = "zip5") %>%
    left_join(lookup, by = c("hsanum", "year")) %>%
    mutate(
      x_dec = to_decile(x_pct), y_dec = to_decile(y_pct),
      drop_cause = dplyr::case_when(
        is.na(hsanum)               ~ "zip_not_in_crosswalk",
        is.na(in_lookup)            ~ "hsa_year_not_in_staged",
        is.na(x_pct) | is.na(y_pct) ~ "percentile_missing",
        TRUE                        ~ "kept"
      )
    )
  
  n_total_events <- nrow(events_raw)
  events <- events_raw %>% filter(drop_cause == "kept")
  n_dropped <- n_total_events - nrow(events)
  
  dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
  if (n_dropped > 0) {
    drop_summary <- events_raw %>%
      filter(drop_cause != "kept") %>%
      count(group, drop_cause, name = "n") %>%
      arrange(group, drop_cause)
    message(sprintf(
      "Dropped %d of %d events without a usable decile. Breakdown by cause:",
      n_dropped, n_total_events
    ))
    print(as.data.frame(drop_summary), row.names = FALSE)
    write_csv(
      events_raw %>% filter(drop_cause != "kept"),
      file.path(out_fig_dir, "decile_grid_dropped_events.csv")
    )
  }
  
  cell_events <- events %>%
    count(x_dec, y_dec, group, name = "n_events") %>%
    pivot_wider(names_from = group, values_from = n_events, values_fill = 0)
  if (!"Opening" %in% names(cell_events)) cell_events$Opening <- 0L
  if (!"Closure" %in% names(cell_events)) cell_events$Closure <- 0L
  
  # --- Assemble full 10x10 grid ---
  grid <- expand_grid(x_dec = 1:10, y_dec = 1:10) %>%
    left_join(cell_universe, by = c("x_dec", "y_dec")) %>%
    left_join(cell_events, by = c("x_dec", "y_dec")) %>%
    mutate(
      n_hsa = coalesce(n_hsa, 0L),
      n_hsa_year = coalesce(n_hsa_year, 0L),
      Opening = coalesce(Opening, 0L),
      Closure = coalesce(Closure, 0L),
      is_empty = n_hsa == 0,                  # no HSA ever occupied this cell
      n_show = if (cell_n == "hsa") n_hsa else n_hsa_year,
      # NA fill for empty cells so they render distinctly from 0-event cells
      open_fill = if_else(is_empty, NA_real_, as.numeric(Opening)),
      close_fill = if_else(is_empty, NA_real_, as.numeric(Closure))
    )
  
  common_max <- max(c(grid$Opening, grid$Closure), na.rm = TRUE)
  common_max <- max(common_max, 1)
  
  # --- Shared theme ---
  grid_theme <- theme_minimal(base_family = font_family, base_size = 12) +
    theme(
      text = element_text(family = font_family),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title = element_text(size = 11),
      legend.position = "right"
    )
  
  # When count is NA (bivariate / dot-on-tile, where the count is encoded by
  # color or dot size), show n only; otherwise show "count\n(n)". Written to be
  # robust to a scalar NA count by recycling to the length of n_show.
  cell_label <- function(count, n_show, is_empty) {
    n <- length(n_show)
    count <- rep_len(count, n)
    is_empty <- rep_len(is_empty, n)
    out <- ifelse(
      is.na(count),
      as.character(n_show),
      paste0(count, "\n(", n_show, ")")
    )
    out[is_empty] <- ""
    out
  }
  
  # ============================================================
  # Figure 1: Two panels side by side (A = openings, B = closures)
  # Shared count limits across panels for honest visual comparison.
  # ============================================================
  build_count_panel <- function(fill_col, count_col, high_color, low_color,
                                title, legend_name) {
    df <- grid %>% mutate(.fill = .data[[fill_col]], .count = .data[[count_col]])
    ggplot(df, aes(x = x_dec, y = y_dec)) +
      geom_tile(aes(fill = .fill), color = "grey70", linewidth = 0.3) +
      geom_text(
        aes(label = cell_label(.count, n_show, is_empty)),
        size = 2.2, lineheight = 0.85, family = font_family, color = "grey15"
      ) +
      scale_fill_gradient(
        low = low_color, high = high_color,
        limits = c(0, common_max), na.value = "grey92", name = legend_name
      ) +
      scale_x_continuous(breaks = 1:10, expand = c(0, 0)) +
      scale_y_continuous(breaks = 1:10, expand = c(0, 0)) +
      coord_fixed() +
      labs(title = title, x = x_label, y = y_label) +
      grid_theme
  }
  
  p_open <- build_count_panel(
    "open_fill", "Opening", open_color, "#f2f7fc",
    "A  Openings", "Openings"
  )
  p_close <- build_count_panel(
    "close_fill", "Closure", close_color, "#fdf3f0",
    "B  Closures", "Closures"
  )
  p_two_panel <- p_open + p_close + plot_layout(ncol = 2)
  
  # ============================================================
  # Figure 2: Bivariate choropleth (terciles of each count -> 3x3 palette)
  # ============================================================
  bivar_pal <- c(
    "1-1" = "#e8e8e8", "2-1" = "#ace4e4", "3-1" = "#5ac8c8",
    "1-2" = "#dfb0d6", "2-2" = "#a5add3", "3-2" = "#5698b9",
    "1-3" = "#be64ac", "2-3" = "#8c62aa", "3-3" = "#3b4994"
  )  # key = "openTercile-closeTercile": openings -> teal/blue, closures -> magenta
  
  populated <- grid %>% filter(!is_empty)
  populated <- populated %>%
    mutate(
      open_t = dplyr::ntile(Opening, 3),
      close_t = dplyr::ntile(Closure, 3),
      bi_key = paste0(open_t, "-", close_t)
    )
  grid_bi <- grid %>%
    left_join(
      populated %>% select(x_dec, y_dec, bi_key),
      by = c("x_dec", "y_dec")
    )
  
  p_bivar_main <- ggplot(grid_bi, aes(x = x_dec, y = y_dec)) +
    geom_tile(aes(fill = bi_key), color = "grey70", linewidth = 0.3) +
    geom_text(
      aes(label = cell_label(NA, n_show, is_empty)),
      size = 2.2, family = font_family, color = "grey15"
    ) +
    scale_fill_manual(values = bivar_pal, na.value = "grey92", guide = "none") +
    scale_x_continuous(breaks = 1:10, expand = c(0, 0)) +
    scale_y_continuous(breaks = 1:10, expand = c(0, 0)) +
    coord_fixed() +
    labs(title = "Openings and closures (bivariate)", x = x_label, y = y_label) +
    grid_theme
  
  legend_df <- expand_grid(open_t = 1:3, close_t = 1:3) %>%
    mutate(bi_key = paste0(open_t, "-", close_t))
  p_bivar_legend <- ggplot(legend_df, aes(x = open_t, y = close_t, fill = bi_key)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_manual(values = bivar_pal, guide = "none") +
    scale_x_continuous(breaks = 1:3, labels = c("Low", "Med", "High")) +
    scale_y_continuous(breaks = 1:3, labels = c("Low", "Med", "High")) +
    coord_fixed() +
    labs(x = "Openings \u2192", y = "Closures \u2192") +
    theme_minimal(base_family = font_family, base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 9),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  p_bivar <- p_bivar_main + p_bivar_legend + plot_layout(widths = c(4, 1))
  
  # ============================================================
  # Figure 3: Dot-on-tile (tile = closures ramp, dot size = openings)
  # ============================================================
  dot_df <- grid %>% filter(!is_empty, Opening > 0)
  p_dot <- ggplot(grid, aes(x = x_dec, y = y_dec)) +
    geom_tile(aes(fill = close_fill), color = "grey70", linewidth = 0.3) +
    scale_fill_gradient(
      low = "#fdf3f0", high = close_color,
      limits = c(0, common_max), na.value = "grey92", name = "Closures"
    ) +
    geom_point(
      data = dot_df, aes(size = Opening),
      color = open_color, alpha = 0.85
    ) +
    scale_size_area(max_size = 6, name = "Openings") +
    geom_text(
      aes(label = cell_label(NA, n_show, is_empty)),
      size = 2.0, family = font_family, color = "grey25",
      nudge_y = 0.30
    ) +
    scale_x_continuous(breaks = 1:10, expand = c(0, 0)) +
    scale_y_continuous(breaks = 1:10, expand = c(0, 0)) +
    coord_fixed() +
    labs(
      title = "Closures (tile) and openings (dot)",
      x = x_label, y = y_label
    ) +
    grid_theme
  
  # --- Export ---
  dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
  f_two   <- file.path(out_fig_dir, "decile_grid_two_panel.png")
  f_bivar <- file.path(out_fig_dir, "decile_grid_bivariate.png")
  f_dot   <- file.path(out_fig_dir, "decile_grid_dot_on_tile.png")
  
  ggsave(f_two,   p_two_panel, width = 14, height = 7,  dpi = dpi)
  ggsave(f_bivar, p_bivar,     width = 10, height = 7.5, dpi = dpi)
  ggsave(f_dot,   p_dot,       width = 9,  height = 7.5, dpi = dpi)
  
  # --- Per-cell CSV (both denominators retained regardless of cell_n) ---
  grid_out <- grid %>%
    transmute(
      poverty_decile = x_dec,
      pop_change_decile = y_dec,
      openings = Opening,
      closures = Closure,
      n_hsa,
      n_hsa_year,
      is_empty
    ) %>%
    arrange(poverty_decile, pop_change_decile)
  write_csv(grid_out, file.path(out_fig_dir, "decile_grid_cell_counts.csv"))
  
  # --- Standalone .tex companions (image + caption + notes), matching pipeline ---
  n_open_total <- sum(grid$Opening)
  n_close_total <- sum(grid$Closure)
  n_hsa_universe <- n_distinct(universe$hsanum)
  n_cell_label <- if (cell_n == "hsa") "distinct HSAs" else "HSA-years"
  
  note_text <- paste(
    sprintf(
      "Each cell aggregates hospital event counts by HSA decile of the below-poverty-line and population-change variables (within-year percentiles binned into deciles, measured in the event year). Counts are individual events: %s openings and %s closures placed; an HSA may contribute to multiple cells across years, and multiple events in one HSA-year each count.",
      format(n_open_total, big.mark = ","), format(n_close_total, big.mark = ",")
    ),
    sprintf(
      "Parenthetical values are the number of %s in each cell (universe of %s HSAs). Empty cells (no HSA in that decile pair) are shown in grey and distinct from populated cells with zero events.",
      n_cell_label, format(n_hsa_universe, big.mark = ",")
    )
  )
  
  write_standalone <- function(png_name, title, varwidth = "16in") {
    tex_lines <- c(
      sprintf("\\documentclass[varwidth=%s, border=10pt]{standalone}", varwidth),
      "\\usepackage{graphicx}",
      "\\usepackage{caption}",
      "",
      "\\begin{document}",
      "\\begin{minipage}{0.98\\textwidth}",
      "  \\centering",
      sprintf("  \\includegraphics[width=\\textwidth]{%s}", png_name),
      sprintf("  \\captionof{figure}{%s}", title),
      sprintf("  \\caption*{\\footnotesize \\textit{Note:} %s}", note_text),
      "\\end{minipage}",
      "\\end{document}"
    )
    out <- file.path(out_fig_dir, str_replace(png_name, "\\.png$", "_standalone.tex"))
    writeLines(tex_lines, out)
  }
  
  write_standalone("decile_grid_two_panel.png",
                   "Hospital Openings (A) and Closures (B) by Poverty and Population-Change Deciles",
                   varwidth = "20in")
  write_standalone("decile_grid_bivariate.png",
                   "Joint Distribution of Hospital Openings and Closures by Poverty and Population-Change Deciles")
  write_standalone("decile_grid_dot_on_tile.png",
                   "Hospital Closures (Shading) and Openings (Point Size) by Poverty and Population-Change Deciles")
  
  message(sprintf(
    "Wrote 3 figures to %s | openings placed=%s, closures placed=%s, dropped=%s",
    out_fig_dir, n_open_total, n_close_total, n_dropped
  ))
  
  invisible(list(
    grid = grid_out,
    plots = list(two_panel = p_two_panel, bivariate = p_bivar, dot_on_tile = p_dot),
    totals = c(openings = n_open_total, closures = n_close_total,
               dropped = n_dropped, hsa_universe = n_hsa_universe)
  ))
}