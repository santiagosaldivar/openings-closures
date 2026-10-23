# Hospital event counts: figure versus characteristics table

The count discrepancy reflects two additional requirements imposed by the characteristics table, even though events were identified from POS: a hospital must survive POS cleaning and have a record in the exact event year. The figure includes 175 openings and 320 closures, whereas the table includes 137 and 301. Cleaning removes 3 openings and 18 closures because their event-year ownership code is 3 ("other"). Another 35 openings are recorded retrospectively in POS: their opening dates are present, but their first annual observations occur one year later (31 hospitals) or two years later (4 hospitals). One closure, Peach County Hospital, has an identifier absent from the current raw panel. A path forward is to review whether the "other" ownership exclusion is substantively justified, evaluate using the first available POS record within two years for opening characteristics with a sensitivity analysis, and resolve the Peach County identifier. Any resulting sample differences should be stated explicitly in the table and figure notes.

## Reconciliation

| Step | Openings | Closures |
| --- | ---: | ---: |
| Curated events counted in the activity figure | 175 | 320 |
| Exact CCN/event-year match in raw POS | 140 | 319 |
| After specialty and year filters | 140 | 319 |
| After ownership/control filter | 137 | 301 |
| After manual name exclusions: characteristics table | 137 | 301 |

Thus, the specialty, year, and manual name exclusions cause no additional loss of these matched events. The ownership filter excludes both codes 3 and 10, but all 21 affected event-year records have code 3. The table matches POS `year` to opening `part_year` or closure `term_year`; identifying an event from a later POS record does not guarantee an annual observation in the event year. For all 35 openings without an event-year observation, the opening year agrees with the `part_year` values in their later raw POS records.

Peach County Hospital's curated closure uses CCN `110009`. That identifier is absent from the current raw POS panel, which contains related Peach hospital names under `111310`. This is a candidate identifier issue, not a verified correction.

## Reproduce the audit

From the repository root, with the local study inputs installed:

```sh
Rscript checks/audit_pos_event_attrition.R
```

Required R packages: `dplyr`, `readr`, and `haven`. Inputs are the raw POS panel, the include/exclude name lists, the cleaned event lists, and `data/processed/pos_panel_updated.csv`. This historical audit reproduces the POS cleaning stages. The current characteristics table and `run_project.R` use `data/processed/pos_panel_reconciled.csv`; reconciliation changes active/termination flags, not CCN/year keys, so the event match counts above remain the same for the current inputs.

The table generator now also recomputes event coverage for its selected panel on every run using `analysis/hospital_characteristics_notes.R`. The concise table footnote reports the current totals and exclusion reasons; `hospital_characteristics_event_coverage.csv` saves the aggregate counts and input hashes beside the table and in the manuscript staging folder. This explains the discrepancy without changing sample eligibility, borrowing later observations, or reassigning Peach County's CCN.

The audit independently applies the cleaning rules and checks that the rebuilt panel matches the existing cleaned panel's row count and CCN/year keys. It also checks uniqueness of raw hospital-year records and of hospitals within each event type, and stops if any exclusion is unexplained. It does not modify analytical data or manuscript outputs.

Versioned aggregate results:

- [Counts after each inclusion step](../checks/output/pos_event_attrition_stages.csv)
- [Counts by inclusion/exclusion reason](../checks/output/pos_event_attrition_reasons.csv)
- [First POS observation lag for openings missing their event-year record](../checks/output/pos_event_attrition_opening_gaps.csv)

The script also writes `checks/output/pos_event_attrition_all.csv` (all events) and `checks/output/pos_event_attrition_excluded.csv` (excluded events with identifiers, names, years, and raw ownership codes). These hospital-level diagnostics remain ignored by Git. The saved aggregate counts describe the inputs audited for this note and are regenerated when the audit runs.
