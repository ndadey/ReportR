#' Run all Line 1.2 general enrollment analyses
#'
#' Orchestrates the enrollment sub-analyses (counts, proportions, subgroup
#' transitions, entry/exit performance, and school/district aggregations)
#' and returns a single named results list. Mirrors [run_general_trend_analysis()]
#' in structure so both results objects can be saved and passed to their
#' respective report templates independently.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Should already be processed by
#'   [standardize_student_results()].
#' @param assessment_spec Named list describing the assessment program
#'   (see assessment_spec).
#' @param report_years Integer vector of years to include. `NULL` (default)
#'   uses all years present in the data.
#' @param subgroup_vars Character vector of demographic column names to use as
#'   subgroups. `NULL` (default) uses all variables in
#'   `assessment_spec$demographics`. Pass an explicit vector (e.g.
#'   `c("IEP", "LEP", "FRL", "GENDER")`) to restrict the analysis.
#' @param min_n Integer. Minimum group size for small-n suppression across
#'   all sub-analyses. Default `10`.
#'
#' @return A named list `general_enrollment_results` with sub-lists:
#'   * `$counts` — enrollment counts at various grains (overall, by year,
#'     by grade, by subject, by subgroup, by school, by district)
#'   * `$proportions` — subgroup proportions (by year, by grade, by school)
#'   * `$transitions` — named by subgroup variable; year-over-year composition
#'     shifts and summaries from [compute_subgroup_transitions()]
#'   * `$entry_exit` — performance of entering and exiting students
#'   * `$aggregate` — school- and district-level enrollment summaries
#'   * `$text` — narrative summary from [summarize_enrollment()]
#'   * `$params` — list of run parameters (`min_n`, `report_years`)
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' general_enrollment_results <- run_general_enrollment_analysis(
#'   student_results_long = demo_data,
#'   assessment_spec      = assessment_spec
#' )
#' general_enrollment_results$counts$by_year
#' general_enrollment_results$proportions$by_subgroup
#' }
run_general_enrollment_analysis <- function(student_results_long,
                                             assessment_spec,
                                             report_years  = NULL,
                                             subgroup_vars = NULL,
                                             min_n         = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    is.null(subgroup_vars) || is.character(subgroup_vars)
  )

  if (!is.null(report_years)) {
    student_results_long <- student_results_long |>
      dplyr::filter(.data[["YEAR"]] %in% report_years)
  }

  message("Running general enrollment analysis...")

  general_enrollment_results <- run_enrollment_analysis(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    report_years         = NULL,           # already filtered above
    subgroup_vars        = subgroup_vars,
    min_n                = min_n
  )

  # Store run parameters for reference in report templates
  general_enrollment_results$params <- list(
    min_n         = min_n,
    report_years  = report_years,
    subgroup_vars = subgroup_vars
  )

  general_enrollment_results
}
