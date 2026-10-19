#' Run the General Cohort Analysis (Line 1.3)
#'
#' @title Run General Cohort Analysis
#' @description Standalone orchestrator for Line 1.3 cohort composition and
#'   performance analysis. Parallel to [run_general_trend_analysis()] (Line 1)
#'   and [run_anomaly_analysis()] (Line 4). Runs [run_cohort_analysis()], saves
#'   results to RDS, and optionally renders the Quarto report.
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

  # --- 1. Run analysis on all years ----------------------------------------
  # report_years controls display in the QMD (main body vs appendix), not
  # which years are computed. All available years are always analysed so that
  # historical data appears in the appendix and is exported to Excel.
  message("Running cohort analysis...")
  cohort_results <- run_cohort_analysis(student_results_long, assessment_spec,
                                         min_n = min_n)

  # --- 3. Save RDS ----------------------------------------------------------
  rds_path <- file.path(output_dir, "cohort_results.rds")
  message("Saving RDS...")
  saveRDS(cohort_results, rds_path)
  message("RDS saved to: ", rds_path)

  # --- 4. Render report (optional) -----------------------------------------
  if (render_report && !requireNamespace("quarto", quietly = TRUE)) {
    warning("The 'quarto' package is required to render the report; ",
            "skipping render. Install it with install.packages('quarto').")
    render_report <- FALSE
  }
  if (render_report) {
    # Prefer source-tree template so edits are picked up without reinstalling
    pkg_root <- tryCatch(find.package("ReportR"), error = function(e) NULL)
    qmd_src  <- if (!is.null(pkg_root))
      file.path(pkg_root, "inst", "templates", "01_General_Cohort_Report.qmd")
    else
      ""
    if (!nzchar(qmd_src) || !file.exists(qmd_src)) {
      qmd_src <- system.file("templates", "01_General_Cohort_Report.qmd",
                              package = "ReportR")
    }

    if (!nzchar(qmd_src) || !file.exists(qmd_src)) {
      warning("01_General_Cohort_Report.qmd not found; skipping render.")
      return(invisible(cohort_results))
    }

    qmd_local <- file.path(output_dir, "01_General_Cohort_Report.qmd")
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
        results_file = rds_path,
        min_n        = min_n,
        report_years = report_years
      )
    )

    final_html <- file.path(output_dir, output_file)
    file.remove(qmd_local)
    message("Report written to: ", final_html)

    if (open_report && file.exists(final_html)) {
      utils::browseURL(final_html)
    }
  }

  invisible(cohort_results)
}
