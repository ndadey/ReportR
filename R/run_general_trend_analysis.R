#' Run all Line 1 general trend analyses
#'
#' Orchestrates the three Line 1 sub-analyses (score trends, proficiency
#' trends, and score distributions) and returns a single named results list.
#' All subsequent report chunks should read from the returned object rather
#' than re-running analysis.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Should already be processed by
#'   [standardize_student_results()].
#' @param assessment_spec Named list describing the assessment program
#'   (see assessment_spec).
#' @param report_years Integer vector. Years to include. If `NULL` (default)
#'   all years present in the data are used.
#' @param min_n Integer. Minimum group size for small-n suppression. Default
#'   `10`.
#'
#' @return A named list `general_trend_results` with sub-lists:
#'   * `$score_trends` — output of [run_score_trends()]
#'   * `$proficiency` — output of [run_proficiency()]
#'   * `$distributions` — output of [run_score_density()]
#'   * `$params` — list of run parameters
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' results <- run_general_trend_analysis(
#'   student_results_long = demo_data,
#'   assessment_spec      = assessment_spec
#' )
#' results$score_trends$plot$mean_trend
#' results$proficiency$plot$trend
#' results$distributions$plot$density[["ELA"]]
#' }
run_general_trend_analysis <- function(student_results_long,
                                       assessment_spec,
                                       report_years = NULL,
                                       min_n        = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  # Optionally subset to report years
  if (!is.null(report_years)) {
    student_results_long <- student_results_long |>
      dplyr::filter(.data[["YEAR"]] %in% report_years)
  }

  general_trend_results <- list()

  message("Running score trends...")
  general_trend_results$score_trends <- run_score_trends(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  message("Running proficiency trends...")
  general_trend_results$proficiency <- run_proficiency(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  message("Running score distributions...")
  general_trend_results$distributions <- run_score_density(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  # Store run parameters so the report can reference them without calling
  # formals() on the function (which fails when the package is installed).
  general_trend_results$params <- list(min_n = min_n)

  general_trend_results
}
