# Master runner for the openings/closings project.
# It sources cleaning/analysis scripts and executes them in order.

message("Starting pipeline...")
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

# Helper: source all scripts in a directory (non-recursive)
source_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) return(invisible())
  scripts <- list.files(dir_path, pattern = "\\.R$", full.names = TRUE)
  scripts <- scripts[!grepl("/\\._", scripts)]
  lapply(scripts, source, chdir = FALSE)
}

# 1) Data cleaning
source_dir(file.path(project_root, "cleaning"))

if (exists("clean_open_close")) {
  message("Cleaning openings/closures data...")
  clean_open_close(
    raw_dir = "data/raw",
    interim_dir = "data/interim",
    openings_file = "updated_openings_august2025.csv",
    closures_file = "updated_closures_august2025.csv"
  )
} else {
  message("Skipping openings/closures cleaning (function clean_open_close() not found).")
}

if (exists("clean_pos")) {
  message("Cleaning POS panel...")
  clean_pos(
    raw_dir = "data/raw",
    processed_dir = "data/processed",
    pos_file = "pos_panel_2009_2024.dta",
    do_not_exclude_file = "pos_do_not_exclude.csv",
    exclude_file = "POS_double_checking_exclude.csv"
  )
} else {
  message("Skipping POS cleaning (clean_pos() not found).")
}

if (exists("run_reconcile_pos_panel")) {
  message("Reconciling POS panel (term_year imputation for curated closures)...")
  run_reconcile_pos_panel(
    pos_path      = "data/processed/pos_panel_updated.csv",
    closures_path = "data/interim/closures_clean.csv",
    openings_path = "data/interim/openings_clean.csv",
    out_path      = "data/processed/pos_panel_reconciled.csv",
    audit_dir     = "checks/output"
  )
} else {
  message("Skipping POS reconciliation (run_reconcile_pos_panel() not found).")
}

if (exists("calc_zip_areas")) {
  message("Calculating ZCTA areas...")
  calc_zip_areas(
    shapefile_path = "data/raw/tl_2020_us_zcta520/tl_2020_us_zcta520.shp",
    output_path = "data/processed/zctas_with_area.csv"
  )
} else {
  message("Skipping ZCTA area calculation (calc_zip_areas() not found).")
}

if (exists("clean_census2010_pop")) {
  message("Building HSA-level 2010 Decennial Census population...")
  clean_census2010_pop(
    census_file = "data/raw/census_raw_data/DECENNIALSF12010/DECENNIALSF12010.P1-Data.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
    output_path = "data/processed/hsa_census2010_pop.csv"
  )
} else {
  message("Skipping 2010 Census population staging (clean_census2010_pop() not found).")
}

if (exists("stage_national_percentiles")) {
  message("Staging national percentiles file...")
  stage_national_percentiles(
    source_path = "data/raw/ntl_hsa_percentiles.csv",
    interim_dir = "data/interim"
  )
} else {
  message("Skipping national percentiles staging (stage_national_percentiles() not found).")
}

if (exists("stage_telestroke_dataset")) {
  message("Staging telestroke dataset...")
  stage_telestroke_dataset(
    telestroke_file = "data/raw/telestroke_data.xlsx",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    national_percentiles_file = "data/interim/ntl_hsa_percentiles.csv",
    processed_dir = "data/processed"
  )
} else {
  message("Skipping telestroke staging (stage_telestroke_dataset() not found).")
}

if (exists("stage_openclose_percentiles")) {
  message("Staging opening/closure percentile file...")
  stage_openclose_percentiles(
    openings_file = "data/raw/updated_openings_august2025.csv",
    closures_file = "data/raw/updated_closures_august2025.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    national_percentiles_file = "data/interim/ntl_hsa_percentiles.csv",
    interim_dir = "data/interim"
  )
} else {
  message("Skipping opening/closure percentile staging (stage_openclose_percentiles() not found).")
}

if (exists("stage_urban_rural_activity")) {
  message("Building combined hospital activity figure...")
  stage_urban_rural_activity(
    openings_file = "data/raw/updated_openings_august2025.csv",
    closures_file = "data/raw/updated_closures_august2025.csv",
    ruca_file = "data/raw/RUCA2010zipcode.xlsx",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
    census_root = "data/raw/census_raw_data",
    dest_dir = "outputs/figures"
  )
} else {
  message("Skipping combined hospital activity figure staging (stage_urban_rural_activity() not found).")
}

# TODO: add additional cleaning steps here as they are refactored.

# 2) Analysis
source_dir(file.path(project_root, "analysis"))

if (exists("create_national_distribution_tables")) {
  message("Creating national distribution tables...")
  create_national_distribution_tables(
    input_csv = "data/interim/ntl_hsa_percentiles.csv",
    years = c(2012, 2015, 2018, 2022),
    out_dir = "outputs/tables",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    ruca_file = "data/raw/RUCA2010zipcode.xlsx",
    zip_zcta_file = "data/raw/ZIPCodetoZCTACrosswalk2022UDS.xlsx",
    census_root = "data/raw/census_raw_data",
    panel_assignment = "hsa_population_weighted"
  )
} else {
  message("Skipping national distribution tables (create_national_distribution_tables() not found).")
}

if (exists("build_premerge_event_counts_tables")) {
  message("Building pre-merge event count appendix tables...")
  build_premerge_event_counts_tables(
    openings_path = "data/interim/openings_clean.csv",
    closures_path = "data/interim/closures_clean.csv",
    out_counts_tex = "outputs/tables/premerge_event_counts.tex",
    out_ruca_tex = "outputs/tables/premerge_event_counts_by_ruca.tex"
  )
} else {
  message("Skipping pre-merge event count appendix tables (build_premerge_event_counts_tables() not found).")
}

if (exists("build_event_counts_by_census_region")) {
  message("Building opening/closure counts by Census region and division...")
  build_event_counts_by_census_region(
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
  )
} else {
  message("Skipping Census region opening/closure counts (build_event_counts_by_census_region() not found).")
}

if (exists("run_hospital_characteristics")) {
  message("Building hospital characteristics table...")
  run_hospital_characteristics(
    openings_path = "data/interim/openings_clean.csv",
    closures_path = "data/interim/closures_clean.csv",
    pos_path = "data/processed/pos_panel_updated.csv",
    out_tex = "outputs/tables/hospital_characteristics.tex",
    out_tex_sensitivity = "outputs/tables/hospital_characteristics_sensitivity.tex",
    out_counts_csv = "outputs/tables/hospital_group_counts.csv",
    out_tests_csv = "outputs/tables/hospital_characteristics_tests.csv"
  )
} else {
  message("Skipping hospital characteristics table (run_hospital_characteristics() not found).")
}

if (exists("run_percentile_plots")) {
  message("Creating percentile histograms/violins...")
  run_percentile_plots(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir = "outputs/figures/percentiles"
  )
  message("Creating HSA ZIP-count plurality percentile histograms/violins...")
  run_percentile_plots(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir = "outputs/figures/percentiles_hsa_zip_count",
    panel_assignment = "hsa_zip_count"
  )
  message("Creating HSA population-weighted plurality percentile histograms/violins...")
  run_percentile_plots(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir = "outputs/figures/percentiles_hsa_population_weighted",
    panel_assignment = "hsa_population_weighted"
  )
}

if (exists("run_percentile_forest_plot")) {
  message("Creating HSA ZIP-count plurality forest-style percentile figure...")
  run_percentile_forest_plot(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir = "outputs/figures/percentiles_hsa_zip_count",
    panel_assignment = "hsa_zip_count"
  )
  message("Creating HSA population-weighted plurality forest-style percentile figure...")
  run_percentile_forest_plot(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir = "outputs/figures/percentiles_hsa_population_weighted",
    panel_assignment = "hsa_population_weighted"
  )
}

if (exists("run_kw_final_table")) {
  message("Building Kruskal-Wallis summary table...")
  run_kw_final_table(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_table_dir = "outputs/tables"
  )
  message("Building HSA ZIP-count plurality Kruskal-Wallis summary table...")
  run_kw_final_table(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_table_dir = "outputs/tables",
    panel_assignment = "hsa_zip_count"
  )
  message("Building HSA population-weighted plurality Kruskal-Wallis summary table...")
  run_kw_final_table(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_table_dir = "outputs/tables",
    panel_assignment = "hsa_population_weighted"
  )
}

if (exists("run_appendix_tables")) {
  message("Building appendix tables...")
  run_appendix_tables(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_table_dir = "outputs/tables"
  )
}

if (exists("run_descriptive")) {
  message("Running descriptive analysis...")
  run_descriptive(
    openings_path  = "data/interim/openings_clean.csv",
    closures_path  = "data/interim/closures_clean.csv",
    pos_path       = "data/processed/pos_panel_updated.csv",
    out_dir        = "outputs/tables",
    snapshot_years = c(2010, 2023),
    year_range     = 2010:2023
  )
} else {
  message("Skipping descriptive analysis (run_descriptive() not found).")
}

if (exists("run_all_decile_grid_heatmaps")) {
  message("Building decile-grid heatmaps (all axes)...")
  run_all_decile_grid_heatmaps(
    input_csv      = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_fig_dir    = "outputs/figures/decile_grids",
    openings_file  = "data/raw/updated_openings_august2025.csv",
    closures_file  = "data/raw/updated_closures_august2025.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    x_var          = "poverty_percentile",
    cell_n         = "hsa"
  )
} else {
  message("Skipping decile-grid heatmaps (run_all_decile_grid_heatmaps() not found).")
}

if (exists("run_main_models")) {
  message("Running main models...")
  run_main_models(input_dir = "data/processed", out_dir = "outputs/models")
} else {
  message("Skipping main models (run_main_models() not found).")
}

if (exists("run_maps")) {
  message("Generating maps...")
  run_maps(input_dir = "data/processed", out_dir = "outputs/figures")
} else {
  message("Skipping maps (run_maps() not found).")
}

if (exists("run_event_rate_descriptives")) {
  run_event_rate_descriptives(
    path_panel     = file.path("data/interim/opening_closure_nonevent_percentiles.csv"),
    path_openings  = file.path("data/raw/updated_openings_august2025.csv"),
    path_closures  = file.path("data/raw/updated_closures_august2025.csv"),
    path_crosswalk = file.path("data/raw/ZipHsaHrr.csv"),
    dir_out        = "outputs/tables",
    year_min       = 2012,
    year_max       = 2023,
    baseline_year  = 2012
  )
}

#if (exists("run_event_regressions")) {
#  run_event_regressions(
#    path_panel     = file.path("data/interim/opening_closure_nonevent_percentiles.csv"),
#    path_openings  = file.path("data/raw/updated_openings_august2025.csv"),
#    path_closures  = file.path("data/raw/updated_closures_august2025.csv"),
#    path_crosswalk = file.path("data/raw/ZipHsaHrr.csv"),
#    path_geo       = NULL,
#    dir_out        = "outputs/tables",
#    dir_checks     = "checks/output",
#    year_min       = 2012,
#    year_max       = 2023
#    )
#}

#if (exists("run_event_regression_figures")) {
#  run_event_regression_figures(
#    path_main   = "outputs/tables/event_regressions_main.csv",
#    path_desc   = "outputs/tables/event_rates_descriptive.csv",
#    dir_fig     = "outputs/figures/event_regressions",
#    family_keep = "Poisson"
#    )
#}

if (exists("run_beds2010_decile_events")) {
  message("Building baseline-2010 beds decile event chart...")
  run_beds2010_decile_events(
    national_percentiles_file = "data/interim/ntl_hsa_percentiles.csv",
    openings_file  = "data/raw/updated_openings_august2025.csv",
    closures_file  = "data/raw/updated_closures_august2025.csv",
    crosswalk_file = "data/raw/ZipHsaHrr.csv",
    out_fig = "outputs/figures/beds2010_decile_events.png",
    out_hist = "outputs/figures/beds2010_baseline_histogram.png",
    out_csv = "outputs/tables/beds2010_decile_events.csv"
  )
} else {
  message("Skipping baseline-2010 beds decile event chart (run_beds2010_decile_events() not found).")
}

message("Pipeline finished.")
