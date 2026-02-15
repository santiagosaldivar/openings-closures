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

if (exists("calc_zip_areas")) {
  message("Calculating ZCTA areas...")
  calc_zip_areas(
    shapefile_path = "data/raw/tl_2020_us_zcta520/tl_2020_us_zcta520.shp",
    output_path = "data/processed/zctas_with_area.csv"
  )
} else {
  message("Skipping ZCTA area calculation (calc_zip_areas() not found).")
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
    out_dir = "outputs/tables"
  )
} else {
  message("Skipping national distribution tables (create_national_distribution_tables() not found).")
}

if (exists("run_hospital_characteristics")) {
  message("Building hospital characteristics table...")
  run_hospital_characteristics(
    openings_path = "data/interim/openings_clean.csv",
    closures_path = "data/interim/closures_clean.csv",
    pos_path = "data/processed/pos_panel_updated.csv",
    out_tex = "outputs/tables/hospital_characteristics.tex"
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
}

if (exists("run_kw_final_table")) {
  message("Building Kruskal-Wallis summary table...")
  run_kw_final_table(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_table_dir = "outputs/tables"
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
  run_descriptive(input_dir = "data/processed", out_dir = "outputs/tables")
} else {
  message("Skipping descriptive analysis (run_descriptive() not found).")
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

message("Pipeline finished.")
