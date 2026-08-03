# ==============================================================================
# 32_event_regression_figures.R
#
# Figures for the community poverty / hospital entry-exit regressions.
# Reads only the CSVs written by 31_hsa_event_regressions.R, so this script can
# be re-run without refitting anything.
#
# Inputs:
#   output/tables/event_regressions_main.csv
#   output/tables/event_rates_descriptive.csv
#
# Outputs (PNG + standalone LaTeX wrapper for each):
#   fig_a_descriptive_rates      crude event rates by poverty quintile x urbanicity
#   fig_b_poverty_attenuation    the M1 -> M2 contrast, poverty term only
#   fig_c_full_forest            all substantive terms, adjusted models
#   fig_d_interaction            poverty x urbanicity, relative to reference stratum
#   fig_e_poisson_vs_logit       diagnostic: agreement between families
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

# Project colour conventions.
.col_opening <- "#2166AC"
.col_closure <- "#D73027"

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# Split "closure_M2_direct_poisson" into its parts.
parse_model_names <- function(df) {
  df %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        stringr::str_detect(.data$model, "^closure") ~ "Closures",
        stringr::str_detect(.data$model, "^opening") ~ "Openings",
        TRUE ~ NA_character_
      ),
      spec = dplyr::case_when(
        stringr::str_detect(.data$model, "M1_total")       ~ "M1: total",
        stringr::str_detect(.data$model, "M2_direct")      ~ "M2: direct",
        stringr::str_detect(.data$model, "M3_interaction") ~ "M3: interaction",
        TRUE ~ NA_character_
      ),
      family = dplyr::case_when(
        stringr::str_detect(.data$model, "poisson") ~ "Poisson",
        stringr::str_detect(.data$model, "logit")   ~ "Logistic",
        TRUE ~ NA_character_
      ),
      outcome = factor(.data$outcome, levels = c("Closures", "Openings"))
    )
}

# Human-readable term labels. Urbanicity levels are unknown at write time, so
# the factor prefix is stripped rather than enumerated.
label_terms <- function(x, poverty_pp = 5) {
  dplyr::case_when(
    x == "poverty_scaled" ~ sprintf("Poverty (per %d pp)", poverty_pp),
    x == "beds_lag"       ~ "Certified beds per 1,000 (t\u22121)",
    x == "log_atrisk"     ~ "log(hospitals at risk)",
    x == "log_pop"        ~ "log(population)",
    stringr::str_detect(x, "^poverty_scaled:urban_f") ~
      paste0("Poverty \u00d7 ", stringr::str_remove(x, "^poverty_scaled:urban_f")),
    stringr::str_detect(x, "^urban_f") ~
      paste0("Urbanicity: ", stringr::str_remove(x, "^urban_f")),
    TRUE ~ x
  )
}

# Terms that are adjustment machinery rather than reportable quantities.
is_nuisance <- function(x) {
  x == "(Intercept)" | stringr::str_detect(x, "^year_f") |
    stringr::str_detect(x, "^region_f")
}

save_figure <- function(plot, name, dir_fig, width, height, dpi = 300) {
  dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir_fig, paste0(name, ".png"))
  ggplot2::ggsave(path, plot, width = width, height = height,
                  dpi = dpi, bg = "white")
  path
}

# Standalone LaTeX wrapper, matching the project's existing figure convention.
write_standalone_tex <- function(png_name, dir_fig, title, subtitle, notes,
                                 graphic_width = "6.5in") {
  tex <- sprintf(
    "\\documentclass[varwidth=7in]{standalone}
\\usepackage{graphicx}
\\usepackage{mathptmx}
\\begin{document}
\\begin{minipage}{7in}
\\textbf{%s}\\\\[2pt]
%s\\\\[6pt]
\\includegraphics[width=%s]{%s.png}\\\\[6pt]
\\footnotesize \\textbf{Notes.} %s
\\end{minipage}
\\end{document}
", title, subtitle, graphic_width, png_name, notes)
  
  path <- file.path(dir_fig, paste0(png_name, ".tex"))
  writeLines(tex, path)
  path
}

# ------------------------------------------------------------------------------
# Figure A: crude event rates by poverty quintile and urbanicity
# ------------------------------------------------------------------------------

fig_descriptive_rates <- function(desc, dir_fig) {
  
  d <- desc %>%
    dplyr::filter(!is.na(.data$pov_q), !is.na(.data$urbanicity)) %>%
    dplyr::select(pov_q, urbanicity,
                  Closures = closures_per_1000_hosp_years,
                  Openings = openings_per_million_person_years) %>%
    tidyr::pivot_longer(c("Closures", "Openings"),
                        names_to = "outcome", values_to = "rate") %>%
    dplyr::mutate(
      outcome = factor(.data$outcome, levels = c("Closures", "Openings")),
      pov_q   = factor(.data$pov_q)
    )
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$pov_q, y = .data$rate,
                                       fill = .data$outcome)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_grid(outcome ~ urbanicity, scales = "free_y",
                        switch = "y") +
    ggplot2::scale_fill_manual(values = c(Closures = .col_closure,
                                          Openings = .col_opening),
                               guide = "none") +
    ggplot2::labs(
      x = "Community poverty quintile (1 = lowest, 5 = highest)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 90),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  png <- save_figure(p, "fig_a_descriptive_rates", dir_fig, 9, 6)
  write_standalone_tex(
    "fig_a_descriptive_rates", dir_fig,
    title = "Crude rates of hospital closure and opening by community poverty and urbanicity, 2012--2023",
    subtitle = "Hospital Service Area--years, baseline-fixed poverty quintiles",
    notes = paste(
      "Closure rates are expressed per 1,000 hospital-years at risk;",
      "opening rates per million person-years. Denominators differ because",
      "closures are events that can only occur to an existing hospital,",
      "whereas openings can occur in any community. Poverty quintiles are",
      "fixed at baseline so that changes in the gradient cannot reflect",
      "Hospital Service Areas moving between strata. Unadjusted."
    )
  )
  invisible(png)
}

# ------------------------------------------------------------------------------
# Figure B: the M1 -> M2 attenuation, poverty term only
# ------------------------------------------------------------------------------

fig_poverty_attenuation <- function(main, dir_fig, poverty_pp = 5,
                                    family_keep = "Poisson") {
  
  d <- main %>%
    dplyr::filter(.data$term == "poverty_scaled",
                  .data$spec %in% c("M1: total", "M2: direct"),
                  .data$family == family_keep)
  
  if (nrow(d) == 0) {
    warning("No rows for family '", family_keep, "'; skipping Figure B.")
    return(invisible(NULL))
  }
  
  d <- d %>%
    dplyr::mutate(spec = factor(.data$spec,
                                levels = c("M2: direct", "M1: total")))
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$spec,
                                       colour = .data$outcome)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf.low,
                                         xmax = .data$conf.high),
                            height = 0.12, linewidth = 0.6) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::scale_colour_manual(values = c(Closures = .col_closure,
                                            Openings = .col_opening),
                                 guide = "none") +
    ggplot2::labs(
      x = sprintf("Incidence rate ratio per %d percentage-point increase in poverty (log scale)",
                  poverty_pp),
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  
  png <- save_figure(p, "fig_b_poverty_attenuation", dir_fig, 7.5, 5)
  write_standalone_tex(
    "fig_b_poverty_attenuation", dir_fig,
    title = "Association between community poverty and hospital closure or opening, before and after adjustment for existing bed supply",
    subtitle = sprintf("Incidence rate ratios per %d percentage points of poverty, %s models",
                       poverty_pp, family_keep),
    notes = paste(
      "M1 estimates the total association between community poverty and event",
      "occurrence. M2 adds certified beds per 1,000 residents in the prior year.",
      "Because existing bed supply plausibly lies on the causal path from",
      "community poverty to event risk, M2 estimates a direct association: the",
      "portion of the poverty gradient not operating through existing capacity.",
      "Attenuation from M1 to M2 therefore indicates mediation through supply,",
      "not absence of association. Both models adjust for urbanicity, calendar",
      "year, and the at-risk denominator. Standard errors are clustered by",
      "Hospital Service Area. Estimates are associational."
    )
  )
  invisible(png)
}

# ------------------------------------------------------------------------------
# Figure C: all substantive terms from the adjusted models
# ------------------------------------------------------------------------------

fig_full_forest <- function(main, dir_fig, poverty_pp = 5,
                            family_keep = "Poisson") {
  
  d <- main %>%
    dplyr::filter(.data$spec == "M2: direct",
                  .data$family == family_keep,
                  !is_nuisance(.data$term)) %>%
    dplyr::mutate(term_label = label_terms(.data$term, poverty_pp))
  
  if (nrow(d) == 0) {
    warning("No rows for family '", family_keep, "'; skipping Figure C.")
    return(invisible(NULL))
  }
  
  # Order terms consistently across panels, poverty first.
  ord <- d %>%
    dplyr::distinct(term, term_label) %>%
    dplyr::arrange(.data$term != "poverty_scaled", .data$term_label)
  d$term_label <- factor(d$term_label, levels = rev(ord$term_label))
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$term_label,
                                       colour = .data$outcome)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf.low,
                                         xmax = .data$conf.high),
                            height = 0.12, linewidth = 0.6) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~ outcome, ncol = 2, scales = "free_x") +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::scale_colour_manual(values = c(Closures = .col_closure,
                                            Openings = .col_opening),
                                 guide = "none") +
    ggplot2::labs(x = "Incidence rate ratio (log scale)", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  
  png <- save_figure(p, "fig_c_full_forest", dir_fig, 10, 5.5)
  write_standalone_tex(
    "fig_c_full_forest", dir_fig,
    title = "Adjusted associations between community characteristics and hospital closure or opening, 2012--2023",
    subtitle = sprintf("Fully adjusted (M2) %s models; incidence rate ratios",
                       family_keep),
    notes = paste(
      "All terms from the fully adjusted models except calendar-year and",
      "census-region fixed effects, which are retained in estimation but",
      "omitted here. Urbanicity coefficients are relative to the Urban",
      "reference category, which pools metropolitan and micropolitan RUCA",
      "codes. Panels use separate horizontal scales; magnitudes should not",
      "be compared across panels by eye. Standard errors are clustered by",
      "Hospital Service Area. Estimates are associational."
    )
  )
  invisible(png)
}

# ------------------------------------------------------------------------------
# Figure D: poverty x urbanicity interaction terms
#
# These are RATIOS OF RATIOS relative to the reference urbanicity category, not
# stratum-specific poverty slopes. A stratum-specific slope is a linear
# combination of the main effect and an interaction term, and its confidence
# interval requires the covariance between them, which the results CSV does not
# carry. See the note at the foot of this script.
# ------------------------------------------------------------------------------

fig_interaction <- function(main, dir_fig, family_keep = "Poisson") {
  
  d <- main %>%
    dplyr::filter(.data$spec == "M3: interaction",
                  .data$family == family_keep,
                  stringr::str_detect(.data$term, "^poverty_scaled:urban_f")) %>%
    dplyr::mutate(term_label = label_terms(.data$term))
  
  if (nrow(d) == 0) {
    warning("No interaction terms found; skipping Figure D.")
    return(invisible(NULL))
  }
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$term_label,
                                       colour = .data$outcome)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf.low,
                                         xmax = .data$conf.high),
                            height = 0.12, linewidth = 0.6) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~ outcome, ncol = 2, scales = "free_x") +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::scale_colour_manual(values = c(Closures = .col_closure,
                                            Openings = .col_opening),
                                 guide = "none") +
    ggplot2::labs(
      x = "Ratio of incidence rate ratios, relative to Urban (log scale)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  
  png <- save_figure(p, "fig_d_interaction", dir_fig, 10, 4.5)
  write_standalone_tex(
    "fig_d_interaction", dir_fig,
    title = "Difference in the poverty association across urbanicity strata",
    subtitle = "Interaction terms from the fully adjusted models",
    notes = paste(
      "Each estimate is a ratio of incidence rate ratios: the factor by which",
      "the poverty association in that urbanicity stratum differs from the",
      "association among Urban HSAs, the reference stratum. A value of 1",
      "indicates no difference. These are not stratum-specific poverty",
      "associations; those are linear combinations of the main effect and the",
      "relevant interaction term. Standard errors are clustered by Hospital",
      "Service Area."
    )
  )
  invisible(png)
}

# ------------------------------------------------------------------------------
# Figure E: diagnostic agreement between Poisson and logistic families
# ------------------------------------------------------------------------------

fig_family_agreement <- function(main, dir_fig) {
  
  d <- main %>%
    dplyr::filter(.data$spec %in% c("M1: total", "M2: direct"),
                  !is_nuisance(.data$term)) %>%
    dplyr::select(outcome, spec, term, family, estimate) %>%
    tidyr::pivot_wider(names_from = "family", values_from = "estimate") %>%
    dplyr::filter(!is.na(.data$Poisson), !is.na(.data$Logistic))
  
  if (nrow(d) == 0) {
    warning("Both families not present; skipping Figure E.")
    return(invisible(NULL))
  }
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Poisson, y = .data$Logistic,
                                       colour = .data$outcome,
                                       shape = .data$spec)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "grey40") +
    ggplot2::geom_point(size = 2.5, alpha = 0.85) +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::scale_colour_manual(values = c(Closures = .col_closure,
                                            Openings = .col_opening),
                                 name = NULL) +
    ggplot2::labs(
      x = "Poisson incidence rate ratio (log scale)",
      y = "Logistic odds ratio (log scale)",
      shape = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
  
  png <- save_figure(p, "fig_e_poisson_vs_logit", dir_fig, 7, 5.5)
  write_standalone_tex(
    "fig_e_poisson_vs_logit", dir_fig,
    title = "Agreement between Poisson and logistic specifications",
    subtitle = "Diagnostic; not intended for publication",
    notes = paste(
      "Each point is one coefficient estimated under both specifications.",
      "The dashed line is equality. Because events are rare, odds ratios and",
      "rate ratios should be close; points far from the line indicate terms",
      "where multiplicity within Hospital Service Area-years is doing",
      "substantive work, and where the Poisson specification should be",
      "preferred."
    )
  )
  invisible(png)
}

# ------------------------------------------------------------------------------
# Driver
# ------------------------------------------------------------------------------

run_event_regression_figures <- function(
    path_main = "output/tables/event_regressions_main.csv",
    path_desc = "output/tables/event_rates_descriptive.csv",
    dir_fig   = "output/figures/event_regressions",
    family_keep = c("Poisson", "Logistic"),
    verbose   = TRUE
) {
  
  family_keep <- match.arg(family_keep)
  
  main <- readr::read_csv(path_main, show_col_types = FALSE, progress = FALSE) %>%
    parse_model_names()
  desc <- readr::read_csv(path_desc, show_col_types = FALSE, progress = FALSE)
  
  poverty_pp <- if ("poverty_units_pp" %in% names(main)) {
    unique(main$poverty_units_pp)[1]
  } else 5
  
  if (verbose) {
    message("Models found: ", paste(unique(main$model), collapse = ", "))
    message("Plotting family: ", family_keep)
  }
  
  out <- list(
    a = fig_descriptive_rates(desc, dir_fig),
    b = fig_poverty_attenuation(main, dir_fig, poverty_pp, family_keep),
    c = fig_full_forest(main, dir_fig, poverty_pp, family_keep),
    d = fig_interaction(main, dir_fig, family_keep),
    e = fig_family_agreement(main, dir_fig)
  )
  
  if (verbose) message("Figures written to ", dir_fig)
  invisible(out)
}

# ------------------------------------------------------------------------------
# LIMITATION, Figure D
#
# Stratum-specific poverty associations (the poverty IRR *within* each
# urbanicity category, rather than relative to a reference) require the
# clustered covariance between the main effect and the interaction term, which
# is not carried in event_regressions_main.csv. Adding them means either
# refitting here or having script 31 export the linear combinations. The latter
# is cleaner: a small addition to run_event_regressions() using the existing
# vcovCL object would emit a stratum-specific contrast table that this script
# could plot directly.
# ------------------------------------------------------------------------------

# Call from run_project.R, e.g.:
#
# if (exists("run_event_regression_figures")) {
#   run_event_regression_figures(
#     path_main   = "outputs/tables/event_regressions_main.csv",
#     path_desc   = "outputs/tables/event_rates_descriptive.csv",
#     dir_fig     = "outputs/figures/event_regressions",
#     family_keep = "Poisson"
#   )
# }