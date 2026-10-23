# Run from the repository root after generating the current figures and tables:
# Rscript tests/test_manuscript_documents.R
source("analysis/41_build_manuscript_documents.R")

expect_error <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
  invisible(error)
}

run_document_checks <- function() {
  scratch <- tempfile("manuscript_documents_check_")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  bundle <- file.path(scratch, "portable bundle with spaces")
  dir.create(bundle)

  # A stale staged file must not substitute for a missing source input.
  missing_builder <- run_manuscript_documents
  local_environment <- new.env(parent = environment(run_manuscript_documents))
  local_environment$manuscript_document_assets <- function() {
    setNames(file.path(scratch, "absent_source.png"), "stale.png")
  }
  environment(missing_builder) <- local_environment
  writeLines("old staged asset", file.path(bundle, "stale.png"))
  intro <- file.path(scratch, "intro.tex")
  writeLines("An introductory paragraph.", intro)
  expect_error(missing_builder(bundle, intro_path = intro, compile = FALSE),
               "Missing current manuscript inputs")
  stopifnot(!file.exists(file.path(bundle, "figures.tex")))

  assets <- manuscript_document_assets()
  required <- c(unname(assets), "manuscript/appendix_intro.tex")
  if (!all(file.exists(required))) {
    message("Current-output integration checks skipped: generate all manuscript source assets first.")
    return(invisible(NULL))
  }

  # Source-only builds must work without any installed LaTeX engine and must
  # replace stale staged assets rather than silently continuing to use them.
  stale_name <- names(assets)[[1]]
  writeLines("outdated staged content", file.path(bundle, stale_name))
  built <- run_manuscript_documents(bundle, compile = FALSE,
                                    latex_engine = "definitely_missing_manuscript_engine")
  stopifnot(all(file.exists(built$tex)), is.null(built$pdf),
            all(unname(tools::md5sum(unname(assets))) ==
                  unname(tools::md5sum(file.path(bundle, names(assets))))))
  main <- readLines(file.path(bundle, "figures.tex"))
  appendix <- readLines(file.path(bundle, "appendix.tex"))
  expected_main_figures <- paste0("Figure ", 1:4, ". ")
  expected_appendix_figures <- paste0("Figure A", 1:5, ". ")
  for (label in expected_main_figures) stopifnot(sum(grepl(label, main, fixed = TRUE)) == 1L)
  for (label in expected_appendix_figures) stopifnot(sum(grepl(label, appendix, fixed = TRUE)) == 1L)
  stopifnot(sum(grepl("\\bfseries Figure ", main, fixed = TRUE)) == 4L,
            sum(grepl("\\bfseries Figure A", appendix, fixed = TRUE)) == 5L,
            sum(grepl("Table 1. ", main, fixed = TRUE)) == 1L)
  for (i in 1:3) stopifnot(sum(grepl(paste0("Table A", i, ". "), appendix, fixed = TRUE)) == 1L)
  stopifnot(any(grepl("{beds2010_decile_events_by_poverty.png}", main, fixed = TRUE)),
            !any(grepl("{beds2010_decile_events.png}", main, fixed = TRUE)),
            !any(grepl("Exhibit", c(main, appendix), fixed = TRUE)),
            !any(grepl("08052026", c(main, appendix), fixed = TRUE)))

  # Preserve the complete authored intro, including blank lines, immediately
  # after the Appendix heading and before its first figure.
  expected_intro <- readLines("manuscript/appendix_intro.tex")
  intro_start <- which(appendix == "\\section*{Appendix}") + 1L
  stopifnot(length(intro_start) == 1L,
            identical(appendix[seq.int(intro_start, length.out = length(expected_intro))], expected_intro),
            intro_start + length(expected_intro) < which(grepl("Figure A1. ", appendix, fixed = TRUE)))

  # Table cells and notes must come from the current generated table, not a
  # historical copy or a table screenshot embedded in an older manuscript.
  table_parts <- manuscript_table_parts(assets[["hospital_characteristics.tex"]])
  table_heading <- paste0("{\\large\\bfseries Table 1. ", table_parts$title, "\\par}")
  stopifnot(sum(main == table_heading) == 1L,
            sum(grepl("\\bfseries Table ", main, fixed = TRUE)) == 1L,
            all(table_parts$tabular %in% main),
            all(vapply(table_parts$notes, function(note) any(grepl(note, main, fixed = TRUE)), logical(1))))
  manifest <- read.csv(file.path(bundle, "manuscript_document_inputs.csv"), stringsAsFactors = FALSE)
  stopifnot(identical(manifest$source[seq_along(assets)], unname(assets)),
            all(manifest$md5 == unname(tools::md5sum(manifest$source))))

  # Every plot reference is relative to the bundle, so copying the bundle to
  # another folder does not depend on the original absolute project path.
  image_lines <- grep("\\includegraphics", c(main, appendix), fixed = TRUE, value = TRUE)
  image_names <- sub("^.*\\\\includegraphics\\[[^]]*\\]\\{([^}]+)\\}.*$", "\\1", image_lines)
  stopifnot(length(image_names) == 9L, all(image_names %in% names(assets)),
            all(file.exists(file.path(bundle, image_names))))

  # Missing engines must fail before rewriting existing document sources.
  tex_hash <- tools::md5sum(built$tex)
  expect_error(run_manuscript_documents(bundle, latex_engine = "definitely_missing_manuscript_engine"),
               "LaTeX engine not found")
  stopifnot(identical(tex_hash, tools::md5sum(built$tex)))

  if (.Platform$OS.type == "unix") {
    # A failed second document must retain both previously published PDFs,
    # even after the first document has successfully produced a replacement.
    bad_engine <- file.path(scratch, "failing-latex")
    writeLines(c(
      "#!/bin/sh", "output_dir=''", "document=''", "for argument in \"$@\"; do",
      "  case \"$argument\" in",
      "    -output-directory=*) output_dir=${argument#-output-directory=} ;;",
      "    figures.tex) document=figures ;;", "    appendix.tex) document=appendix ;;",
      "  esac", "done", "if [ \"$document\" = figures ]; then",
      "  echo 'new PDF sentinel' > \"$output_dir/figures.pdf\"", "  exit 0", "fi",
      "echo intentional test compilation failure", "exit 1"
    ), bad_engine)
    Sys.chmod(bad_engine, "0755")
    pdf_paths <- file.path(bundle, c("figures.pdf", "appendix.pdf"))
    for (path in pdf_paths) writeLines("previously published PDF sentinel", path)
    pdf_hash <- tools::md5sum(pdf_paths)
    working_directory <- getwd()
    expect_error(suppressWarnings(run_manuscript_documents(bundle, latex_engine = bad_engine)),
                 "LaTeX failed for appendix")
    stopifnot(identical(pdf_hash, tools::md5sum(pdf_paths)),
              identical(working_directory, getwd()))
  }
  message("Manuscript document integration checks passed.")
}

run_document_checks()
