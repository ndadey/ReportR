#' Run all Section 5 aggregate (school and district) analyses
#'
#' Orchestrates the five sub-analyses that can run from `student_results_long`
#' alone. Sub-analyses that require `school_results_long` (accountability grade
#' distributions, intervention analysis, outlier characterization by co-op or
#' locale) are set to `NULL` with explanatory comments.
#'
#' @param student_results_long A long-format student results data frame (see
#'   data contract). Must contain school and district identifier columns.
#' @param assessment_spec Named list describing the assessment program.
#' @param school_results_long Optional school-level data frame containing
#'   accountability grades, indicator scores, HQIM_FLAG, LITERACY_COACH_FLAG,
#'   CO_OP, LOCALE, etc. Default `NULL`. Currently unused — pass it when the
#'   data becomes available to enable sub-analyses 5.2 and 5.7.
#' @param report_years Integer vector of years to include. `NULL` uses all
#'   years in the data. Default `NULL`.
#' @param min_n_school Integer. Minimum students per school × year × subject ×
#'   grade cell. Default `10`.
#' @param min_n_subgroup Integer. Minimum subgroup size for disaggregated
#'   analyses. Default `10`.
#' @param improvement_threshold Numeric. Percentage-point change to classify
#'   a school as improving or declining. Default `3`.
#' @param outlier_sd_threshold Numeric. SD threshold for residual-based outlier
#'   flagging. Default `2`.
#' @param outlier_n_funnel Integer. Minimum n for funnel plot inclusion.
#'   Default `20`.
#' @param correlation_flag_threshold Numeric. Spearman r threshold for flagging
#'   unstable year-to-year correlations. Default `0.85`.
#'
#' @return A named list `aggregate_results` with sub-lists:
#'   * `$performance_trends` — [compute_performance_trends()]
#'   * `$accountability`     — `NULL` (requires school_results_long)
#'   * `$population_shifts`  — [compute_population_shifts()]
#'   * `$outliers`           — [compute_outlier_schools()]
#'   * `$disaggregated`      — [compute_school_disaggregated()]
#'   * `$stability`          — [compute_stability_analysis()]
#'   * `$intervention`       — `NULL` (requires school_results_long)
#'   * `$params`             — run parameters
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' aggregate_results <- run_aggregate_analysis(
#'   student_results_long = demo_data,
#'   assessment_spec      = assessment_spec
#' )
#' aggregate_results$performance_trends$improvement_classification
#' aggregate_results$outliers$positive_outlier_schools
#' }
run_aggregate_analysis <- function(student_results_long,
                                    assessment_spec,
                                    school_results_long        = NULL,
                                    report_years               = NULL,
                                    min_n_school               = 10,
                                    min_n_subgroup             = 10,
                                    improvement_threshold      = 3,
                                    outlier_sd_threshold       = 2,
                                    outlier_n_funnel           = 20,
                                    correlation_flag_threshold = 0.85) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  if (!is.null(report_years)) {
    student_results_long <- student_results_long |>
      dplyr::filter(.data[["YEAR"]] %in% report_years)
  }

  school_results_available <- !is.null(school_results_long)
  if (!school_results_available) {
    message(
      "school_results_long not provided.\n",
      "  5.2 Accountability Grades       — SKIPPED\n",
      "  5.3 co-op / locale breakdowns   — SKIPPED\n",
      "  5.4 Outlier characterization    — SKIPPED (identification will run)\n",
      "  5.7 Intervention Analysis       — SKIPPED\n"
    )
  }

  aggregate_results <- list()

  # ---- 5.1 Performance Trends ----
  message("Running performance trends (5.1)...")
  aggregate_results$performance_trends <- compute_performance_trends(
    data                       = student_results_long,
    spec                       = assessment_spec,
    min_n_school               = min_n_school,
    improvement_threshold      = improvement_threshold,
    correlation_flag_threshold = correlation_flag_threshold
  )

  # ---- 5.2 Accountability Grade Distributions — [REQUIRES school_results_long] ----
  # Needs: ACCOUNTABILITY_GRADE, ACCOUNTABILITY_INDEX, INDICATOR_* fields.
  # When school_results_long is available, implement compute_accountability_distributions()
  # to produce grade_distributions, grade_change_table, indicator_attribution,
  # index_sensitivity, and single_indicator_driven sub-lists.
  aggregate_results$accountability <- NULL

  # ---- 5.3 Population Shifts ----
  message("Running population shifts (5.3)...")
  aggregate_results$population_shifts <- compute_population_shifts(
    data             = student_results_long,
    performance_data = aggregate_results$performance_trends,
    spec             = assessment_spec,
    school_data      = school_results_long,
    min_n_school     = min_n_school
  )

  # ---- 5.4 Outlier Identification ----
  message("Running outlier identification (5.4)...")
  aggregate_results$outliers <- compute_outlier_schools(
    performance_data      = aggregate_results$performance_trends,
    spec                  = assessment_spec,
    school_data           = school_results_long,
    outlier_sd_threshold  = outlier_sd_threshold,
    outlier_n_funnel      = outlier_n_funnel
  )

  # ---- 5.5 Disaggregated Gaps ----
  message("Running disaggregated gap analysis (5.5)...")
  aggregate_results$disaggregated <- compute_school_disaggregated(
    data           = student_results_long,
    spec           = assessment_spec,
    min_n_subgroup = min_n_subgroup
  )

  # ---- 5.6 Stability Analysis ----
  message("Running stability analysis (5.6)...")
  aggregate_results$stability <- compute_stability_analysis(
    performance_data           = aggregate_results$performance_trends,
    spec                       = assessment_spec,
    correlation_flag_threshold = correlation_flag_threshold
  )

  # ---- 5.7 Intervention Analysis — [REQUIRES school_results_long] ----
  # Needs: HQIM_FLAG, LITERACY_COACH_FLAG from school_results_long.
  # When available, implement compute_intervention_analysis() using
  # propensity score matching (MatchIt, nearest method, caliper = 0.1 SD)
  # on demographic composition and prior proficiency.
  aggregate_results$intervention <- NULL

  aggregate_results$params <- list(
    min_n_school               = min_n_school,
    min_n_subgroup             = min_n_subgroup,
    improvement_threshold      = improvement_threshold,
    outlier_sd_threshold       = outlier_sd_threshold,
    outlier_n_funnel           = outlier_n_funnel,
    correlation_flag_threshold = correlation_flag_threshold,
    report_years               = report_years,
    school_results_available   = school_results_available
  )

  aggregate_results
}
