
# Open/Close Hospital Pipeline

This repository runs the openings/closures data pipeline and analysis from:
- `run_project.R`
- `cleaning/`
- `analysis/`

`archive/` is backup-only and is not part of the active pipeline.

## Repository Setup (Data hosted externally)

This repo is configured to keep large datasets out of Git.  
Place data from your Dropbox (or other storage) into:

- `data/raw/`

The scripts will generate:
- `data/interim/`
- `data/processed/`
- `outputs/`

## Required Raw Inputs

At minimum for the standard run (`run_project.R`), place these in `data/raw/`:

- `updated_openings_august2025.csv`
- `updated_closures_august2025.csv`
- `pos_panel_2009_2024.dta`
- `pos_do_not_exclude.csv`
- `POS_double_checking_exclude.csv`
- `tl_2020_us_zcta520/tl_2020_us_zcta520.shp` (and companion shapefile files)
- `ntl_hsa_percentiles.csv`
- `telestroke_data.xlsx`
- `ZipHsaHrr.csv`
- `RUCA2010zipcode.xlsx`

Additional files are needed only for the optional full rebuild of `ntl_hsa_percentiles.csv` (see below).

## R Dependencies

Install required packages before running:

```r
install.packages(c(
  "broom", "dplyr", "ggplot2", "gt", "haven", "kableExtra",
  "lubridate", "patchwork", "purrr", "readr", "readxl",
  "rlang", "sf", "stringr", "tidyr"
))
```

Optional:
- `webshot2` only if you want `gt` table PNG export (otherwise scripts fall back to `.tex`).

## Run Commands

Standard pipeline:

```bash
Rscript run_project.R
```

Optional full rebuild of national HSA percentiles from raw census/SDI sources:

```bash
Rscript -e "source('cleaning/09_rebuild_ntl_hsa_percentiles_full.R'); rebuild_ntl_hsa_percentiles_full()"
```

For this optional full rebuild, include these in `data/raw/`:
- `census_raw_data/` (ACS extracts by year)
- `sdi_data/`
- `ZIPCodetoZCTACrosswalk2022UDS.xlsx`
- `variable_mapping_DP02.csv`
- `variable_mapping_DP05.csv`
- `variable_mapping_S1701.csv`


## Notes

- To be updated as necessary. 

## Baseline hospital urbanicity

`analysis/baseline_urbanicity_split.R` (moved from the archive) runs through
`run_project.R` after cleaning. It counts unique active CCNs in 2010 from
`pos_panel_updated.csv`, using hospital ZIP RUCA 2010 classifications.
Urban combines metropolitan and micropolitan; rural/small town combines RUCA 7–10.
These are hospital ZIP classifications, distinct from the HSA-level urbanicity
used elsewhere in the analysis. Unmatched ZIPs are reported separately.
The script divides cleaned opening events in 2010–2023 by each baseline count.

Outputs: `outputs/tables/baseline_urbanicity_split.csv` and
`outputs/figures/ruca_distribution_baseline.png`.
With cleaned inputs available, run just this analysis from the project root:

```bash
Rscript -e "source('analysis/baseline_urbanicity_split.R'); run_baseline_urbanicity_split()"
```
