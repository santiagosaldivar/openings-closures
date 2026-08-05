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

### `population_density`

HSA residents per square mile: HSA-summed population over HSA-summed ZCTA land
area (`data/processed/zctas_with_area.csv`), built in
`cleaning/10_rebuild_ntl_hsa_percentiles_from_raw.R`.

| Year(s) | Population source |
|---|---|
| 2010 | 2010 Decennial Census (SF1 P1), ZCTA-level |
| 2011-2023 | ACS 5-year `B01003`, ZCTA-level (`sum_total_pop_event`) |

Both sums are restricted to the same area-complete ZCTA set (rows with
non-missing area and non-missing population), so the 2010 value is
compositionally consistent with 2011+. Density is a level, so the Census
substitution follows the same logic as the beds-per-1,000 denominator above.

### `pop_change_pct`

Trailing population change for staged year `t`:
`100 * (pop_{t-1} - pop_{t-3}) / pop_{t-3}`, over ACS 5-year HSA populations
(`sum_total_pop_event`). **First valid year: 2014** (needs the 2011 ACS value
as the `t-3` lag).

**Deliberately not extended with the 2010 Decennial Census.** An ACS 5-year
estimate behaves like a population level at its window midpoint, so the
ACS-only formula compares midpoints roughly two years apart. The only value
the Census could add is 2013 = (ACS 2012 - Census 2010) / Census 2010; ACS
2012 (2008-2012) has midpoint ~2010 and the Census is April 2010, so that
"change" would span roughly zero years and be biased toward zero relative to
every other year, ranking 2013 events artificially near the middle of the
growth-percentile distribution. `sum_total_pop_event` is left NA for 2010 in
the rebuild so the lag-based formula cannot pick the Census value up. The
Census is used only for level variables (beds per 1,000, urbanicity weights,
population density).

## `data/interim/opening_closure_nonevent_percentiles.csv`

### `certbeds_per_1000_residents_percentile` (and `_geo`, `_geo_zip_count` variants)

Within-year percentile ranks of `certbeds_per_1000_residents_lag1`, built in
`cleaning/06_stage_openclose_percentiles.R`. Populated 2010-2023; the 2010
pool inherits the contemporaneous-denominator convention flagged above.
