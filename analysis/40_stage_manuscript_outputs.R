# Stage manuscript-ready figures and tables into outputs/manuscript.
# Usage: source("analysis/40_stage_manuscript_outputs.R"); stage_manuscript_outputs()
#
# Copies the current manuscript set into a single flat folder. Originals stay
# where their source scripts write them; this folder is a convenience mirror
# and is safe to regenerate at any time. The two decile-grid figures share a
# filename at their sources, so they are renamed on copy (suffix = axis).
#
# To change the manuscript set, edit the manifest below (destination name =
# source path, relative to the repo root).

stage_manuscript_outputs <- function(dest_dir = "outputs/manuscript") {
  manifest <- c(
    # --- Figures -------------------------------------------------------------
    "beds2010_baseline_histogram.png" =
      "outputs/figures/beds2010_baseline_histogram.png",
    "beds2010_decile_events.png" =
      "outputs/figures/beds2010_decile_events.png",
    "opening_closure_counts_by_census_region_poster_hires.png" =
      "outputs/figures/census_region/opening_closure_counts_by_census_region_poster_hires.png",
    "combined_hospital_activity_2way.png" =
      "outputs/figures/combined_hospital_activity_2way.png",
    "combined_hospital_activity.png" =
      "outputs/figures/combined_hospital_activity.png",
    # Renamed on copy: both sources are named decile_grid_two_panel.png.
    "decile_grid_two_panel_beds_per_capita.png" =
      "outputs/figures/decile_grids/beds_per_capita/decile_grid_two_panel.png",
    "decile_grid_two_panel_pop_change.png" =
      "outputs/figures/decile_grids/pop_change/decile_grid_two_panel.png",
    "forest_plot_3panel_hsa_population_weighted.png" =
      "outputs/figures/percentiles_hsa_population_weighted/forest_plot_3panel_hsa_population_weighted.png",
    "urban_openings_closures_six_panel_violin.png" =
      "outputs/figures/percentiles_hsa_population_weighted/urban_openings_closures_six_panel_violin.png",
    "rural_small_town_openings_closures_six_panel_violin.png" =
      "outputs/figures/percentiles_hsa_population_weighted/rural_small_town_openings_closures_six_panel_violin.png",
    # --- Tables --------------------------------------------------------------
    "appendix_medians_over_time_standalone.tex" =
      "outputs/tables/appendix_medians_over_time_standalone.tex",
    "appendix_quantiles_2016_standalone.tex" =
      "outputs/tables/appendix_quantiles_2016_standalone.tex",
    "brief_summary_table_bh_pvalues_hsa_population_weighted_standalone.tex" =
      "outputs/tables/brief_summary_table_bh_pvalues_hsa_population_weighted_standalone.tex",
    "brief_summary_table_hsa_population_weighted_standalone.tex" =
      "outputs/tables/brief_summary_table_hsa_population_weighted_standalone.tex",
    "hospital_characteristics.tex" =
      "outputs/tables/hospital_characteristics.tex"
  )

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  copied <- character(0)
  missing <- character(0)
  for (dest_name in names(manifest)) {
    src <- manifest[[dest_name]]
    if (file.exists(src)) {
      ok <- file.copy(src, file.path(dest_dir, dest_name), overwrite = TRUE)
      if (ok) copied <- c(copied, dest_name) else missing <- c(missing, src)
    } else {
      missing <- c(missing, src)
    }
  }

  message("Manuscript outputs: copied ", length(copied), " of ",
          length(manifest), " files to ", dest_dir, ".")
  if (length(missing) > 0) {
    warning("Manuscript outputs NOT copied (source missing or copy failed):\n  - ",
            paste(missing, collapse = "\n  - "))
  }

  invisible(list(copied = copied, missing = missing))
}
