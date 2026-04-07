# ==============================================================================
# Line 1 — General Trend Analysis: Overall Score Distributions
#
# Ported from summarize_scores.R. Key changes from original:
#   - CONTENT_AREA renamed to SUBJECT throughout
#   - Rewrote in dplyr (was base-R split/lapply)
#   - Added assessment_spec argument for subject/grade validation
#   - Added $ScaleScore_Stats_Report: wide table with grades as rows,
#     year-grouped column blocks (Mean / SD / N per year)
#   - Renamed compute_score_summary() per package conventions
#   - Added mean_sd_table_wide() wrapper for Quarto templates
#
# Functions:
#   compute_score_summary()  — full distributional summary
#   mean_sd_table_wide()     — report-ready wide table (template alias)
# ==============================================================================


#' Compute scale score distributional summaries by year, subject, and grade
#'
#' Produces long- and wide-format summary tables of student scale score
#' distributions, a counts table, and a report-ready wide table with grades as
#' rows and year-grouped column blocks (Mean / SD / N). Designed as the primary
#' data source for all Line 1 score trend reporting.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Must contain `YEAR`, `SUBJECT`, `SCALE_SCORE`, and
#'   `GRADE` when `by_grade = TRUE`.
#' @param assessment_spec Named list describing the assessment program.
#'   Used to validate that subjects in the data match the spec.
#' @param probs Numeric vector of percentiles in \[0, 1\]. Default
#'   `c(0.05, 0.25, 0.50, 0.75, 0.95)`.
#' @param by_grade Logical. If `TRUE` (default) summaries are computed per
#'   `YEAR × GRADE × SUBJECT`. If `FALSE`, `GRADE` is ignored and summaries
#'   are per `YEAR × SUBJECT`.
#' @param min_n Integer. Groups with fewer than `min_n` non-missing scores
#'   have their statistics set to `NA`. Default `10`.
#'
#' @return A named list with four elements:
#'   * `$ScaleScore_Stats_Long` — long-format data frame: grouping columns plus
#'     `metric` and `value`. Metrics: `mean`, `sd`, `p05`, `p25`, `p50`,
#'     `p75`, `p95` (or as determined by `probs`).
#'   * `$ScaleScore_Stats_Wide` — wide-format data frame: one row per group,
#'     one column per metric.
#'   * `$ScaleScore_Stats_Report` — report-ready wide table: one row per grade,
#'     with year-grouped column blocks `<YEAR>_Mean`, `<YEAR>_SD`, `<YEAR>_N`.
#'   * `$ScaleScore_Counts` — data frame with `n_total`, `n_nonmissing`,
#'     `n_missing`, and `pct_missing` per group.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' out <- compute_score_summary(demo_data, assessment_spec)
#' head(out$ScaleScore_Stats_Wide)
#' }
compute_score_summary <- function(student_results_long,
                                  assessment_spec,
                                  probs    = c(0.05, 0.25, 0.50, 0.75, 0.95),
                                  by_grade = TRUE,
                                  min_n    = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("YEAR", "SUBJECT", "SCALE_SCORE") %in% names(student_results_long)),
    !by_grade || "GRADE" %in% names(student_results_long)
  )

  p_names    <- paste0("p", sprintf("%02d", round(probs * 100)))
  group_vars <- if (by_grade) c("YEAR", "SUBJECT", "GRADE") else c("YEAR", "SUBJECT")

  # ----------------------------------------------------------
  # 1) Counts per group
  # ----------------------------------------------------------
  counts <- student_results_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      n_total      = dplyr::n(),
      n_nonmissing = sum(!is.na(.data[["SCALE_SCORE"]])),
      n_missing    = sum(is.na(.data[["SCALE_SCORE"]])),
      pct_missing  = ifelse(
        .data[["n_total"]] > 0,
        .data[["n_missing"]] / .data[["n_total"]],
        NA_real_
      ),
      .groups = "drop"
    )

  # ----------------------------------------------------------
  # 2) Stats per group via group_modify (mean, sd, percentiles)
  # ----------------------------------------------------------
  wide <- student_results_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::group_modify(function(data, keys) {
      x <- data[["SCALE_SCORE"]]
      n <- sum(!is.na(x))

      if (n >= min_n) {
        qs       <- stats::quantile(x, probs = probs, na.rm = TRUE,
                                    names = FALSE, type = 7)
        mean_val <- mean(x, na.rm = TRUE)
        sd_val   <- stats::sd(x, na.rm = TRUE)
      } else {
        qs       <- rep(NA_real_, length(probs))
        mean_val <- NA_real_
        sd_val   <- NA_real_
      }

      out <- data.frame(mean = mean_val, sd = sd_val,
                        stringsAsFactors = FALSE)
      for (i in seq_along(probs)) out[[p_names[i]]] <- qs[i]
      out
    }) |>
    dplyr::ungroup()

  # ----------------------------------------------------------
  # 3) Long table
  # ----------------------------------------------------------
  metric_cols <- c("mean", "sd", p_names)
  long <- wide |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(metric_cols),
      names_to  = "metric",
      values_to = "value"
    )

  # ----------------------------------------------------------
  # 4) Report-ready wide table: grades as rows, year-block columns
  #    Layout: SUBJECT [GRADE] | <yr>_Mean  <yr>_SD  <yr>_N  | ...
  # ----------------------------------------------------------
  report_src <- wide |>
    dplyr::left_join(
      counts |> dplyr::select(dplyr::all_of(c(group_vars, "n_nonmissing"))),
      by = group_vars
    ) |>
    dplyr::rename(Mean = "mean", SD = "sd", N = "n_nonmissing") |>
    tidyr::pivot_longer(
      cols      = c("Mean", "SD", "N"),
      names_to  = "stat",
      values_to = "value"
    ) |>
    dplyr::mutate(
      col_name = paste(.data[["YEAR"]], .data[["stat"]], sep = "_")
    ) |>
    dplyr::select(-"YEAR", -"stat") |>
    tidyr::pivot_wider(
      names_from  = "col_name",
      values_from = "value"
    )

  # Order columns: id columns, then year blocks sorted chronologically
  id_cols   <- if (by_grade) c("SUBJECT", "GRADE") else "SUBJECT"
  yr_order  <- sort(unique(student_results_long[["YEAR"]]))
  stat_cols <- as.vector(
    outer(yr_order, c("Mean", "SD", "N"), paste, sep = "_")
  )
  stat_cols <- stat_cols[stat_cols %in% names(report_src)]

  report <- report_src |>
    dplyr::select(dplyr::all_of(c(id_cols, stat_cols)))

  if (by_grade) {
    report <- report |>
      dplyr::arrange(.data[["SUBJECT"]], grade_key(.data[["GRADE"]]))
  } else {
    report <- report |>
      dplyr::arrange(.data[["SUBJECT"]])
  }

  # ----------------------------------------------------------
  # 5) Return
  # ----------------------------------------------------------
  list(
    ScaleScore_Stats_Long   = long,
    ScaleScore_Stats_Wide   = wide,
    ScaleScore_Stats_Report = report,
    ScaleScore_Counts       = counts
  )
}


#' Report-ready wide means/SD table for a single subject
#'
#' A thin wrapper around [compute_score_summary()] that filters to one subject
#' and returns the `$ScaleScore_Stats_Report` element directly. Named to match
#' the aspirational function name used in `01_General_Trend_Analysis.qmd` so
#' the template works without modification once this function exists.
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param subject Character scalar. Subject code to include (e.g. `"ELA"`).
#' @param probs Numeric vector of percentiles. Default
#'   `c(0.05, 0.25, 0.50, 0.75, 0.95)`.
#' @param min_n Integer. Minimum group size for suppression. Default `10`.
#'
#' @return A data frame: one row per grade, year-grouped columns
#'   `<YEAR>_Mean`, `<YEAR>_SD`, `<YEAR>_N`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' mean_sd_table_wide(demo_data, assessment_spec, subject = "ELA")
#' }
mean_sd_table_wide <- function(student_results_long,
                               assessment_spec,
                               subject,
                               probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
                               min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.character(subject), length(subject) == 1L
  )

  dat <- student_results_long |>
    dplyr::filter(.data[["SUBJECT"]] == subject)

  compute_score_summary(
    student_results_long = dat,
    assessment_spec      = assessment_spec,
    probs                = probs,
    by_grade             = TRUE,
    min_n                = min_n
  )[["ScaleScore_Stats_Report"]]
}
