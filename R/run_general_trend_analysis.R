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
#' @param score_flat_threshold Numeric. Scale score point changes smaller than
#'   this are labelled "relatively flat" in the score trends narrative. Default
#'   `1.0`.
#' @param prof_flat_threshold Numeric. Percentage point changes smaller than
#'   this are labelled "relatively flat" in the proficiency narrative. Default
#'   `1.0`.
#'
#' @param flag_sd Numeric. SD threshold for normative transition flagging.
#'   Default `1.5`.
#'
#' @return A named list `general_trend_results` with sub-lists:
#'   * `$score_trends`            — output of [run_score_trends()]
#'   * `$proficiency`             — output of [run_proficiency()]
#'   * `$distributions`           — output of [run_score_density()]
#'   * `$achievement_transitions` — output of [run_achievement_transitions()]
#'   * `$params`                  — list of run parameters
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
                                       report_years         = NULL,
                                       min_n                = 10,
                                       score_flat_threshold = 1.0,
                                       prof_flat_threshold  = 1.0,
                                       flag_sd              = 1.5) {
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
    min_n                = min_n,
    flat_threshold       = score_flat_threshold
  )

  message("Running proficiency trends...")
  general_trend_results$proficiency <- run_proficiency(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n,
    flat_threshold       = prof_flat_threshold
  )

  message("Running score distributions...")
  general_trend_results$distributions <- run_score_density(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )

  message("Running achievement transitions...")
  general_trend_results$achievement_transitions <- run_achievement_transitions(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    score_trends         = general_trend_results$score_trends,
    proficiency          = general_trend_results$proficiency,
    min_n                = min_n,
    flag_sd              = flag_sd
  )

  general_trend_results$params <- list(min_n = min_n)

  general_trend_results
}
