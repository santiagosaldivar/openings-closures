# Optional full rebuild wrapper for ntl_hsa_percentiles.csv.
# This intentionally does NOT run inside run_project.R because it can take a long time.
# Usage:
#   source("cleaning/09_rebuild_ntl_hsa_percentiles_full.R")
#   rebuild_ntl_hsa_percentiles_full()

#' Full rebuild wrapper for ntl_hsa_percentiles using raw inputs
#'
#' @param raw_dir Preferred raw root (default "data/raw")
#' @param output_raw_path Destination path in data/raw
#' @param output_interim_path Optional mirror destination in data/interim
#' @return list of output paths invisibly
rebuild_ntl_hsa_percentiles_full <- function(
  raw_dir = "data/raw",
  output_raw_path = "data/raw/ntl_hsa_percentiles.csv",
  output_interim_path = "data/interim/ntl_hsa_percentiles.csv"
) {
  source("cleaning/10_rebuild_ntl_hsa_percentiles_from_raw.R", local = TRUE)
  rebuild_ntl_hsa_percentiles_from_raw(
    raw_dir = raw_dir,
    output_raw_path = output_raw_path,
    output_interim_path = output_interim_path
  )
}
