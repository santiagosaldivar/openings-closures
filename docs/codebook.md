# Codebook

Variable-level documentation for staged analysis files. Entries are added as
definitions change or need explicit conventions recorded. Methodological
figure notes live in `outputs/figures/README.md`.

## `data/interim/ntl_hsa_percentiles.csv`

### `certbeds_per_1000_residents_lag1`

HSA-level certified beds per 1,000 residents attributed to staged year `t`.
Built in `cleaning/04_stage_national_percentiles.R`.

| Year(s) | Numerator | Denominator | Denominator source |
|---|---|---|---|
| 2010 | 2009 certified beds | **2010 population (contemporaneous, NOT lagged)** | 2010 Decennial Census (SF1 P1) |
| 2011 | 2010 certified beds | 2010 population (lagged) | 2010 Decennial Census (SF1 P1) |
| 2012-2023 | year `t-1` certified beds | year `t-1` population (lagged) | ACS 5-year, HSA-aggregated (`sum_total_pop_event`) |

**2010 flag — contemporaneous denominator.** The 2010 observation is the only
year in the series whose denominator is not lagged: it pairs 2009 beds with
the 2010 Decennial Census population, because no 2009 population source exists
in the pipeline. Treat comparisons of the 2010 value against later years
accordingly.

Numerator: certified beds summed over active hospitals in the cleaned POS
panel (`data/processed/pos_panel_reconciled.csv`), ZIP-to-HSA via
`ZipHsaHrr.csv`. HSA-years where no active hospital reports beds are NA (not
zero). 2009 POS rows exist solely as the lag source for the 2010 numerator and
never appear as observation years.

Census denominator: built by `cleaning/03b_clean_census2010_pop.R`
(`data/processed/hsa_census2010_pop.csv`), ZCTA P001001 aggregated to HSA with
the same crosswalks and deduplication as the ACS population.

### `certbeds_per_1000_residents`

Contemporaneous (unlagged) companion column: `1000 * beds_t / pop_t`. For 2010
the denominator is the 2010 Decennial Census population; 2011-2023 use ACS
5-year denominators. This column is an intermediate; the lagged column above
is the variable of record for ranking and analysis.

## `data/interim/opening_closure_nonevent_percentiles.csv`

### `certbeds_per_1000_residents_percentile` (and `_geo`, `_geo_zip_count` variants)

Within-year percentile ranks of `certbeds_per_1000_residents_lag1`, built in
`cleaning/06_stage_openclose_percentiles.R`. Populated 2010-2023; the 2010
pool inherits the contemporaneous-denominator convention flagged above.
