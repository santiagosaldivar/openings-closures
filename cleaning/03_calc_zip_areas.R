# Compute ZCTA areas from a shapefile and export a flat CSV.
# Usage: source("R/cleaning/03_calc_zip_areas.R"); calc_zip_areas()

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

#' Calculate ZCTA areas (square miles) from a shapefile
#'
#' @param shapefile_path Path to ZCTA shapefile (.shp)
#' @param output_path Output CSV path (default "data/processed/zctas_with_area.csv")
#' @return tibble of ZCTAs with area columns
calc_zip_areas <- function(
  shapefile_path = "data/raw/tl_2020_us_zcta520/tl_2020_us_zcta520.shp",
  output_path = "data/processed/zctas_with_area.csv"
) {
  if (!file.exists(shapefile_path)) {
    stop("Shapefile not found: ", shapefile_path)
  }

  zctas <- st_read(shapefile_path, quiet = TRUE)

  sq_meters_to_sq_miles <- 0.000000386102

  zctas_with_area <- zctas %>%
    mutate(AREA_SQ_MI = ALAND20 * sq_meters_to_sq_miles) %>%
    sf::st_drop_geometry() %>%
    select(ZCTA5CE20, GEOID20, ALAND20, AWATER20, AREA_SQ_MI)

  out_dir <- dirname(output_path)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(zctas_with_area, output_path)

  invisible(zctas_with_area)
}
