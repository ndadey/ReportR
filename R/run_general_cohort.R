#' Run the General Cohort Analysis (Line 1.3)
#'
#' @title Run General Cohort Analysis
#' @description Standalone orchestrator for Line 1.3 cohort composition and
#'   performance analysis. Parallel to [run_general_trend_analysis()] (Line 1)
#'   and [run_anomaly_analysis()] (Line 4). Runs [run_cohort_analysis()], saves
#'   results to RDS using the Dropbox-safe tempdir pattern, and optionally
#'   renders the Quarto report.
#'
#' @param student_results_long A standardized long-format student results data
#'   frame (see data contract in `CLAUDE.md §6`). Must have already been
#'   processed by [standardize_student_results()].
#' @param assessment_spec Named list describing the assessment program. Set
#'   `attr(assessment_spec, "spec_file")` to the absolute path of the spec
#'   file so it can be sourced inside the QMD.
#' @param output_dir Character. Directory where the RDS and rendered report
#'   will be copied after rendering.
#' @param report_years Integer vector. Years to include in the analysis. If
#'   `NULL` (default), all years present in `student_results_long` are used.
#'   Always filter *after* [standardize_student_results()].
#' @param min_n Integer. Minimum cell size for small-n suppression. Default
#'   `10`.
#' @param render_report Logical. If `TRUE` (default), renders the Quarto
#'   report to both docx and html.
#' @param open_report Logical. If `TRUE`, opens the rendered html in a
#'   browser after rendering. Default `FALSE`.
#'
#' @return The cohort results list, returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' attr(assessment_spec, "spec_file") <-
#'   system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR")
#' cohort_results <- run_general_cohort(
#'   student_results_long = demo_data,
#'   assessment_spec      = assessment_spec,
#'   output_dir           = tempdir(),
#'   render_report        = FALSE
#' )
#' }
run_general_cohort <- function(student_results_long,
                                assessment_spec,
                                output_dir,
                                report_years  = NULL,
                                min_n         = 10,
                                render_report = TRUE,
                                open_report   = FALSE) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    is.character(output_dir), length(output_dir) == 1L,
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT", "SCALE_SCORE",
          "ACHIEVEMENT_LEVEL") %in% names(student_results_long))
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # --- 1. Filter years (always after standardization) ----------------------
  if (!is.null(report_years)) {
    message("Filtering to report years: ",
            paste(report_years, collapse = ", "), "...")
    student_results_long <- student_results_long |>
      dplyr::filter(.data[["YEAR"]] %in% report_years)
  }

  # --- 2. Run analysis -----------------------------------------------------
  message("Running cohort analysis...")
  cohort_results <- run_cohort_analysis(student_results_long, assessment_spec,
                                         min_n = min_n)

  # --- 3. Save RDS via tempdir (Dropbox-safe) ------------------------------
  RENDER_DIR <- file.path(tempdir(), "ReportR_render")
  dir.create(RENDER_DIR, recursive = TRUE, showWarnings = FALSE)

  rds_tmp <- file.path(RENDER_DIR, "cohort_results.rds")
  message("Saving RDS to tempdir...")
  saveRDS(cohort_results, rds_tmp)
  file.copy(rds_tmp, file.path(output_dir, "cohort_results.rds"),
            overwrite = TRUE)
  message("RDS saved to: ", file.path(output_dir, "cohort_results.rds"))

  # --- 4. Render report (optional) -----------------------------------------
  if (render_report) {
    qmd_src <- system.file("templates", "01_General_Cohort_Report.qmd",
                            package = "ReportR")

    # Fall back to source-tree path during development
    if (!nzchar(qmd_src)) {
      pkg_root <- tryCatch(find.package("ReportR"), error = function(e) NULL)
      if (!is.null(pkg_root)) {
        qmd_src <- file.path(pkg_root, "inst", "templates",
                             "01_General_Cohort_Report.qmd")
      }
    }

    if (!nzchar(qmd_src) || !file.exists(qmd_src)) {
      warning("01_General_Cohort_Report.qmd not found; skipping render.")
      return(invisible(cohort_results))
    }

    qmd_local <- file.path(RENDER_DIR, "01_General_Cohort_Report.qmd")
    copied    <- file.copy(qmd_src, qmd_local, overwrite = TRUE)
    if (!copied) {
      warning("Could not copy QMD template; skipping render.")
      return(invisible(cohort_results))
    }

    spec_file <- attr(assessment_spec, "spec_file")
    if (is.null(spec_file) || !nzchar(spec_file)) {
      warning("attr(assessment_spec, 'spec_file') is not set. ",
              "Set it to the absolute path of your spec file so the QMD can ",
              "source it. Skipping render.")
      return(invisible(cohort_results))
    }

    pkg_path <- tryCatch(find.package("ReportR"), error = function(e) NULL)

    message("Rendering report...")
    output_file <- "01_General_Cohort_Report.html"
    quarto::quarto_render(
      input          = qmd_local,
      output_format  = "html",
      output_file    = output_file,
      execute_params = list(
        pkg_path     = pkg_path,
        spec_file    = spec_file,
        results_file = rds_tmp,
        min_n        = min_n
      )
    )

    final_html <- file.path(output_dir, output_file)
    file.copy(file.path(RENDER_DIR, output_file), final_html, overwrite = TRUE)
    message("Report written to: ", final_html)

    if (open_report && file.exists(final_html)) {
      utils::browseURL(final_html)
    }
  }

  invisible(cohort_results)
}
