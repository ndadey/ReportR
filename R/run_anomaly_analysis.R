#' Run all Line 4 score quality analyses
#'
#' Orchestrates the five score quality sub-analyses (HOSS/LOSS pileups, score
#' integrity, year-to-year outliers, year-to-year correlations, and
#' distribution diagnostics) and returns a single named results list. All
#' subsequent report chunks should read from the returned object rather than
#' re-running analysis.
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
#' @param point_adjust Integer. Scale-score points from LOSS/HOSS boundary
#'   that defines "near". Default `5`.
#' @param mild_thresh Numeric. Percent threshold for Mild HOSS/LOSS flag.
#'   Default `5`.
#' @param moderate_thresh Numeric. Percent threshold for Moderate flag.
#'   Default `10`.
#' @param severe_thresh Numeric. Percent threshold for Severe flag.
#'   Default `15`.
#'
#' @return A named list `anomaly_results` with sub-lists:
#'   * `$loss_hoss` — output of [run_loss_hoss()]
#'   * `$score_integrity` — output of [run_score_integrity()]
#'   * `$outliers` — output of [run_outliers()] (stub)
#'   * `$correlations` — output of [run_correlations()] (stub)
#'   * `$distributions` — output of [run_distributions()] (stub)
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' results <- run_anomaly_analysis(
#'   student_results_long = demo_data,
#'   assessment_spec      = assessment_spec,
#'   report_years         = 2016:2019,
#'   min_n                = 10
#' )
#' }
run_anomaly_analysis <- function(student_results_long,
                                 assessment_spec,
                                 report_years    = NULL,
                                 min_n           = 10,
                                 point_adjust    = 5,
                                 mild_thresh     = 5,
                                 moderate_thresh = 10,
                                 severe_thresh   = 15) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  # Optionally subset to report years
  if (!is.null(report_years)) {
    student_results_long <- student_results_long |>
      dplyr::filter(.data[["YEAR"]] %in% report_years)
  }

  anomaly_results <- list()

  anomaly_results$loss_hoss <- run_loss_hoss(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    point_adjust         = point_adjust,
    min_n                = min_n,
    mild_thresh          = mild_thresh,
    moderate_thresh      = moderate_thresh,
    severe_thresh        = severe_thresh
  )

  anomaly_results$score_integrity <- run_score_integrity(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  anomaly_results$outliers <- run_outliers(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  anomaly_results$correlations <- run_correlations(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  anomaly_results$distributions <- run_distributions(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  # Regenerate distribution plots with shape statistics from both analyses
  anomaly_results$loss_hoss$plot_distributions <- plot_loss_hoss_distributions(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    loss_hoss_table      = anomaly_results$loss_hoss$table,
    distribution_table   = anomaly_results$distributions$table,
    point_adjust         = point_adjust
  )

  anomaly_results
}
