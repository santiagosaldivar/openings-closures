
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
  "rlang", "sf", "stringr", "tidyr", "sandwich", "survey"
))
```

Optional:
- `webshot2` only if you want `gt` table PNG export (otherwise scripts fall back to `.tex`).

## Run Commands

Standard pipeline:

```bash
Rscript run_project.R
```

The pipeline finishes by creating `outputs/manuscript/figures.tex` and
`figures.pdf` (Figures 1-4 and Table 1), plus `appendix.tex` and `appendix.pdf`
(the appendix introduction, Figures A1-A5, and Tables A1-A3). Table 1 uses the
closure-reconciled POS panel. The original two appendix introductory paragraphs
are maintained in `manuscript/appendix_intro.tex`; edit that source when needed.

PDF compilation requires `pdflatex` on PATH and the standard TeX packages
`geometry`, `lmodern`, `graphicx`, `adjustbox`, `booktabs`, `array`, `siunitx`,
`pdflscape`, and `hyperref` (available in MacTeX or installable with TinyTeX).
Set `MANUSCRIPT_LATEX_ENGINE` to an alternative installed engine if needed.
The build fails explicitly for missing current inputs or failed compilation,
retaining the previously successful PDFs. Logs and input hashes are saved
alongside the documents.

After rerunning individual figure/table scripts, rebuild both documents from
their current outputs without rerunning the full analysis:

```bash
Rscript -e "source('analysis/41_build_manuscript_documents.R'); run_manuscript_documents()"
```

Use `run_manuscript_documents(compile = FALSE)` only for a sources-only export.
Generated LaTeX embeds current table contents and refers to local staged PNGs;
copy the `.tex` files and accompanying PNGs together to compile elsewhere.
It does not require the historical August 5 wrappers or `_preamble` file.

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

- [Hospital event count audit](docs/pos_event_attrition.md): explains the activity figure versus characteristics table counts, with a reproducible audit and saved aggregate results.
- `analysis/manuscript_claims.R` generates `outputs/manuscript/manuscript_claims.md`
  and `.csv` with reproducible urban violin percentages (Figure 4 in the supplied
  August 5 LaTeX), manuscript sentences, and
  counting rules. Its `run_all_manuscript_claims()` entry point also runs
  `analysis/manuscript_sample_claims.R`, producing
  `outputs/manuscript/manuscript_sample_claims.md` and companion CSVs for the
  sample total, 2010/2023 counts, Figure 1 events, and baseline-cohort closure
  percentages. The manuscript default is the closure-reconciled POS panel
  with population-weighted HSA urbanicity. The report retains original-panel
  and hospital-ZIP results as historical comparisons, checks reconciliation
  against the curated closures, and records input hashes. Adopted values are
  in `manuscript_sample_claim_checks.csv`; ready-to-use methods and results
  paragraphs are in `manuscript_sample_replacement_text.txt`.
  `analysis/manuscript_community_claims.R` adds CSVs for community percentile
  means, raw/BH-adjusted tests, significance counts, availability, and the
  income example. `analysis/manuscript_hospital_claims.R` adds CSVs for
  hospital descriptions/denominators, bed-decile poverty shares/exclusions,
  and endpoint bed totals. The latter labels existing omnibus tests as
  previously generated evidence, rather than treating them as pairwise tests.
  All reports run automatically before the document build at the end of `run_project.R`.
  To regenerate all numerical evidence from existing staged data, run:

  ```sh
  Rscript -e "source('analysis/manuscript_claims.R'); run_all_manuscript_claims()"
  ```

  Rebuild the full pipeline after upstream data changes. Distribution-shape
  interpretations are labeled for visual review rather than assumed to remain true.
  To run only the sample/event audit, use
  `Rscript -e "source('analysis/manuscript_sample_claims.R'); run_manuscript_sample_claims()"`.
  The original `run_manuscript_claims()` entry point still runs the urban violin
  report only; its optional `figure_label` controls the manuscript figure number.

## Hospital characteristics table

`analysis/20_hospital_characteristics.R` produces two main versions from the
same hospital-year sample: `outputs/tables/hospital_characteristics.tex`
(without location) and `hospital_characteristics_with_rural.tex` (with hospital
ZIP rural/small-town location). Both have one overall p-value column, mutually
exclusive CMS teaching-affiliation categories, and one decimal for descriptive
values. Ownership and teaching each receive one block-level Rao-Scott test
clustered by hospital; mean beds receive a clustered Wald test. See the
[codebook](docs/codebook.md#hospital-characteristics-tables) for definitions.

The generator also writes both year-weighted sensitivity versions, full-precision
tests, category counts, missingness/denominator diagnostics, and insertion-ready
manuscript methods text. The staging step copies all four TeX tables and the
methods text to `outputs/manuscript/` for insertion into the main manuscript.
This analysis defaults to `pos_panel_reconciled.csv`,
matching the input already passed by `run_project.R`.

The standalone caption is **Table 1.** with a bold label, including when the
table is included by an external LaTeX wrapper. The shorter note explains why
its event counts differ from Figure 1. Counts and exclusion reasons are rebuilt
from the current event lists, selected POS panel, raw POS records, and cleaning
name lists on every run; no historical counts are hardcoded. The aggregate
`hospital_characteristics_event_coverage.csv` records this reconciliation and
input hashes and is staged with the table. Detailed teaching definitions and
test specifications remain in `hospital_characteristics_methods.md`.

With cleaned inputs available, regenerate and check these outputs with:

```sh
Rscript -e 'source("analysis/20_hospital_characteristics.R"); run_hospital_characteristics(); source("analysis/40_stage_manuscript_outputs.R"); stage_manuscript_outputs()'
Rscript tests/test_hospital_characteristics.R
Rscript tests/test_hospital_characteristics_notes.R
```

Required additional packages: `survey` and `sandwich`. The analysis also loads
packages from `.Rlibs/` when that optional project-local library exists.

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
