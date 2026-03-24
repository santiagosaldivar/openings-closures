suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
})

resolve_existing_path <- function(paths, label) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop(label, " not found. Checked: ", paste(paths, collapse = ", "))
  }
  existing[[1]]
}

escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

write_standalone_table <- function(lines, out_path) {
  doc <- c(
    "\\documentclass[varwidth=10in, border=10pt]{standalone}",
    "\\usepackage{booktabs}",
    "\\usepackage{caption}",
    "",
    "\\begin{document}",
    lines,
    "\\end{document}"
  )

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(doc, out_path)
}

format_count <- function(x) {
  format(as.integer(x), big.mark = ",", trim = TRUE)
}

build_premerge_event_counts_tables <- function(
  openings_path = "data/interim/openings_clean.csv",
  closures_path = "data/interim/closures_clean.csv",
  ruca_candidates = c("data/raw/RUCA2010zipcode.xlsx", "data/01_raw/RUCA2010zipcode.xlsx"),
  out_counts_tex = "outputs/tables/premerge_event_counts.tex",
  out_ruca_tex = "outputs/tables/premerge_event_counts_by_ruca.tex"
) {
  if (!file.exists(openings_path)) stop("Openings file not found: ", openings_path)
  if (!file.exists(closures_path)) stop("Closures file not found: ", closures_path)

  ruca_path <- resolve_existing_path(ruca_candidates, "RUCA file")

  openings <- read_csv(openings_path, show_col_types = FALSE) %>%
    transmute(
      ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"),
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(part_year),
      event_type = "Opening"
    ) %>%
    filter(year >= 2010, !is.na(year)) %>%
    distinct()

  closures <- read_csv(closures_path, show_col_types = FALSE) %>%
    transmute(
      ccn = str_pad(as.character(ccn), width = 6, side = "left", pad = "0"),
      zip5 = str_pad(as.character(zip5), width = 5, side = "left", pad = "0"),
      year = as.integer(term_year),
      event_type = "Closure"
    ) %>%
    filter(year >= 2010, !is.na(year)) %>%
    distinct()

  events <- bind_rows(openings, closures)

  overall_counts <- events %>%
    count(event_type, name = "n") %>%
    arrange(match(event_type, c("Opening", "Closure")))

  ruca <- read_excel(ruca_path, sheet = "Data") %>%
    transmute(
      zip5 = str_pad(as.character(ZIP_CODE), width = 5, side = "left", pad = "0"),
      ruca_category = case_when(
        RUCA1 %in% c(1, 2, 3) ~ "Metropolitan",
        RUCA1 %in% c(4, 5, 6) ~ "Micropolitan",
        RUCA1 %in% c(7, 8, 9) ~ "Small Town",
        RUCA1 == 10 ~ "Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    distinct(zip5, ruca_category)

  ruca_levels <- c("Metropolitan", "Micropolitan", "Small Town", "Rural", "Unmatched RUCA")

  ruca_counts <- events %>%
    left_join(ruca, by = "zip5") %>%
    mutate(ruca_category = coalesce(ruca_category, "Unmatched RUCA")) %>%
    count(ruca_category, event_type, name = "n") %>%
    tidyr::complete(ruca_category = ruca_levels, event_type = c("Opening", "Closure"), fill = list(n = 0L)) %>%
    tidyr::pivot_wider(names_from = event_type, values_from = n) %>%
    mutate(Total = Opening + Closure) %>%
    mutate(ruca_category = factor(ruca_category, levels = ruca_levels)) %>%
    arrange(ruca_category) %>%
    mutate(ruca_category = as.character(ruca_category))

  overall_lines <- c(
    "\\centering",
    "\\captionof{table}{Pre-merge Opening and Closure Counts}",
    "\\begin{tabular}{lr}",
    "\\toprule",
    "Event Type & \\multicolumn{1}{c}{Count} \\\\",
    "\\midrule",
    vapply(
      seq_len(nrow(overall_counts)),
      function(i) {
        sprintf(
          "%s & %s \\\\",
          overall_counts$event_type[[i]],
          format_count(overall_counts$n[[i]])
        )
      },
      character(1)
    ),
    "\\bottomrule",
    "\\end{tabular}"
  )

  ruca_lines <- c(
    "\\centering",
    "\\captionof{table}{Pre-merge Opening and Closure Counts by RUCA Category}",
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    "RUCA Category & \\multicolumn{1}{c}{Openings} & \\multicolumn{1}{c}{Closures} & \\multicolumn{1}{c}{Total} \\\\",
    "\\midrule",
    vapply(
      seq_len(nrow(ruca_counts)),
      function(i) {
        sprintf(
          "%s & %s & %s & %s \\\\",
          escape_latex(ruca_counts$ruca_category[[i]]),
          format_count(ruca_counts$Opening[[i]]),
          format_count(ruca_counts$Closure[[i]]),
          format_count(ruca_counts$Total[[i]])
        )
      },
      character(1)
    ),
    "\\bottomrule",
    "\\end{tabular}"
  )

  write_standalone_table(overall_lines, out_counts_tex)
  write_standalone_table(ruca_lines, out_ruca_tex)

  invisible(list(
    overall_counts = overall_counts,
    ruca_counts = ruca_counts,
    outputs = c(overall = out_counts_tex, ruca = out_ruca_tex)
  ))
}
