# Recreate manuscript claims from the staged analysis data.
# Urban violin plot (Figure 4 in figures_08052026.tex) only:
# Run from the project root:
# Rscript -e "source('analysis/manuscript_claims.R'); run_manuscript_claims()"
# All manuscript numerical reports:
# Rscript -e "source('analysis/manuscript_claims.R'); run_all_manuscript_claims()"
# The full pipeline runs all reports after manuscript figure staging.
run_manuscript_claims <- function(
    input_csv = "data/interim/opening_closure_nonevent_percentiles.csv",
    out_dir = "outputs/manuscript",
    figure_label = "Figure 4") {
  dat <- read.csv(input_csv, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  measures <- c(
    bachelors = "Bachelor's degree attainment", income = "Household income",
    public_health_insurance = "Public insurance coverage", SDI = "Social deprivation index",
    pop_change_pct = "Population growth", certbeds_per_1000_residents = "Beds per 1,000 residents"
  )
  cols <- paste0(names(measures), "_percentile_geo")
  required <- c("hsanum", "year", "group", "geography_type", cols)
  missing <- setdiff(required, names(dat))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  if (anyNA(dat[c("hsanum", "year", "group")])) stop("Missing observation keys.")
  if (anyDuplicated(dat[c("hsanum", "year", "group")])) {
    stop("Duplicate HSA-year-event-type records; claims would double-count observations.")
  }
  for (col in cols) {
    if (!is.numeric(dat[[col]]) || any(!is.na(dat[[col]]) &
        (!is.finite(dat[[col]]) | dat[[col]] < 0 | dat[[col]] > 100))) {
      stop("Invalid percentile values in ", col)
    }
  }
  urban <- dat[!is.na(dat$geography_type) & dat$geography_type == "Urban" &
                 dat$group %in% c("Opening", "Closure"), ]
  if (!nrow(urban)) stop("No urban event observations.")
  stats <- do.call(rbind, lapply(names(measures), function(measure) {
    do.call(rbind, lapply(c("Opening", "Closure"), function(group) {
      sub <- urban[urban$group == group, ]
      x <- sub[[paste0(measure, "_percentile_geo")]]
      valid <- !is.na(x)
      x <- x[valid]
      if (!length(x)) stop("No available observations for ", measure, "/", group)
      do.call(rbind, lapply(c(25, 50, 75), function(threshold) {
        data.frame(measure = measure, label = unname(measures[measure]), group = group,
                   threshold = threshold, numerator = sum(x > threshold),
                   denominator = length(x), percentage = 100 * mean(x > threshold),
                   missing = sum(!valid), unique_hsas = length(unique(sub$hsanum[valid])),
                   first_year = min(sub$year[valid]), last_year = max(sub$year[valid]),
                   min_percentile = min(x), max_percentile = max(x))
      }))
    }))
  }))
  get <- function(measure, group, threshold) {
    stats[stats$measure == measure & stats$group == group & stats$threshold == threshold, ]
  }
  fraction <- function(row) sprintf("%d of %d (%.1f%%)", row$numerator, row$denominator, row$percentage)
  b <- get("bachelors", "Opening", 50)
  p <- get("pop_change_pct", "Opening", 75)
  s <- get("SDI", "Closure", 50)
  ic <- get("public_health_insurance", "Closure", 50)
  io <- get("public_health_insurance", "Opening", 50)
  sentences <- c(
    sprintf("Among urban HSA-years with a hospital opening and available educational attainment data, %s were above the 50th percentile for bachelor's degree attainment among urban HSAs in the corresponding year.", fraction(b)),
    sprintf("Among urban HSA-years with a hospital opening and available population change data, %s were above the 75th percentile for population growth among urban HSAs in the corresponding year.", fraction(p)),
    sprintf("Among urban HSA-years with a hospital closure and available SDI data, %s had SDI values above the 50th percentile among urban HSAs in the corresponding year.", fraction(s)),
    sprintf("Public insurance coverage exceeded the corresponding year's urban 50th percentile in %s closure HSA-years and %s opening HSA-years with available data.", fraction(ic), fraction(io))
  )
  paragraph <- paste(
    paste(figure_label, "shows the distributions of six community characteristics among urban HSA-years with hospital openings or closures."),
    sprintf("Among opening HSA-years, %s were above the 50th percentile for bachelor's degree attainment, and %s were above the 75th percentile for population growth.", fraction(b), fraction(p)),
    sprintf("Among closure HSA-years, %s were above the 50th percentile for social deprivation.", fraction(s)),
    sentences[4],
    "Percentiles were calculated among urban HSAs within each year; denominators include observations with available data for each measure."
  )
  table_rows <- apply(stats, 1, function(r) sprintf(
    "| %s | %s | >%s | %s | %s | %.1f%% | %s |",
    r[["label"]], r[["group"]], trimws(r[["threshold"]]), trimws(r[["numerator"]]),
    trimws(r[["denominator"]]), as.numeric(r[["percentage"]]), trimws(r[["missing"]])))
  lines <- c(
    "# Manuscript claims: urban six-panel violin plot", "",
    sprintf("Generated from `%s` (MD5: `%s`).", input_csv, unname(tools::md5sum(input_csv))), "",
    "Figure: `urban_openings_closures_six_panel_violin.png`.", "",
    "Population-weighted HSA urbanicity; Urban pools metropolitan and micropolitan areas. Uses the staged `geography_type` and `_percentile_geo` columns used for the population-weighted analysis. This is not the rural/small-town figure.", "",
    "## Reusable sentences", "", paste0(seq_along(sentences), ". ", sentences), "",
    "## Combined numerical paragraph", "", paragraph, "",
    "## Visual interpretation to review after regeneration", "",
    "The earlier draft described broad percentile ranges among opening communities, greater concentration at higher bed-supply percentiles among closing communities, and broader public-insurance coverage distributions among closing communities. These are visual interpretations, not automatically validated claims. Recheck the regenerated figure before retaining them; the numerical paragraph above updates without assuming those shapes persist.", "",
    "## Counts and thresholds", "",
    "Thresholds are strictly greater than 25, 50, or 75; values equal to the threshold are not counted in the numerator. These are descriptive shares, not hypothesis tests or probabilities of hospital events.", "",
    "| Measure | Event group | Percentile threshold | Above threshold | Available N | Percentage | Missing N |",
    "| --- | --- | --- | ---: | ---: | ---: | ---: |", table_rows, "",
    "## Counting rules", "",
    "- Multiple events of the same type in one HSA-year count once.",
    "- An HSA with events in different years contributes once per event year within each group.",
    "- An HSA-year with both an opening and a closure contributes once to each group.",
    "- Denominators exclude missing values separately for each measure. HSA-years are not unique HSAs or individual hospitals.",
    "- Percentiles are relative to all urban HSAs with available data in the corresponding year, not only event HSAs or the national HSA distribution.", "",
    "## Recreate", "", "From the project root:", "", "```sh",
    "Rscript -e \"source('analysis/manuscript_claims.R'); run_manuscript_claims()\"", "```", "",
    "This command uses the existing staged dataset. After raw data or upstream analysis changes, run `Rscript run_project.R` to rebuild the data, figures, and this report together. The full pipeline generates this report after manuscript staging.", "",
    "The accompanying `manuscript_claims.csv` also records unique HSA counts, available year ranges, and observed percentile minima and maxima."
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(stats, file.path(out_dir, "manuscript_claims.csv"), row.names = FALSE)
  writeLines(lines, file.path(out_dir, "manuscript_claims.md"))
  message("Wrote manuscript claims to ", out_dir)
  invisible(stats)
}

run_all_manuscript_claims <- function(out_dir = "outputs/manuscript") {
  source("analysis/manuscript_sample_claims.R", local = TRUE)
  source("analysis/manuscript_community_claims.R", local = TRUE)
  source("analysis/manuscript_hospital_claims.R", local = TRUE)
  sample <- run_manuscript_sample_claims(out_dir = out_dir)
  violin <- run_manuscript_claims(out_dir = out_dir)
  community <- run_manuscript_community_claims(out_dir = out_dir)
  hospital <- run_manuscript_hospital_claims(out_dir = out_dir)
  invisible(list(sample = sample, violin = violin, community = community, hospital = hospital))
}
