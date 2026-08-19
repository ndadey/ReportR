# ==============================================================================
# utils_enrollment.R — Enrollment Analysis Utilities
#
# STEP 1 FINDINGS — extracted from the existing 1.1 codebase
# (R/01_trend_overall.R, R/01_trend_proficiency.R, R/01_trend_distributions.R,
# R/run_general_trend_analysis.R) before any 1.2 enrollment code was written.
# All 1.2 files must be consistent with these findings.
#
#   Pipe style:              |>  (native R pipe)
#   Data manipulation:       dplyr + tidyr (both in Imports)
#   Error handling:          stopifnot() at function entry; no tryCatch / cli
#   Input validation:        stopifnot() with explicit column and type checks,
#                            matching the pattern in compute_score_summary(),
#                            compute_proficiency(), etc.
#   Table rendering:         flextable (in Imports — verified in DESCRIPTION)
#   Sankey/alluvial package: none found in DESCRIPTION; ggalluvial added to
#                            Suggests for plot_enrollment_sankey(); the function
#                            uses requireNamespace() and returns an informative
#                            error if ggalluvial is not installed
#   Color palette:           no shared palette object — inline hex codes only:
#                              "#f4cccc" (severe/red), "#fce8b2" (mild/yellow),
#                              "#f6b26b" (orange), "#9fc5e8" (blue)
#   Shared suppression util: none in 1.1 — each function uses
#                            dplyr::if_else(.data[["n"]] >= min_n, ..., NA_real_)
#                            inline after a separate summarize step.
#                            suppress_small_n_enroll() defined here uses base-R
#                            row indexing (per CLAUDE.md §7) for multi-column
#                            suppression applied after count computation.
#   Package naming:          snake_case for file names; ReportR is the package
#   assessment_spec fields:  $program, $subjects, $achievement_levels,
#                            $scale_scores, $years, $demographics, [$notes]
#   Demographic/subgroup vars: names(assessment_spec$demographics)
#   min_n:                   always a function parameter (default 10);
#                            NOT a field in assessment_spec
# ==============================================================================


#' Suppress statistics for small enrollment groups
#'
#' Sets specified numeric columns to `NA_real_` for rows where the count
#' column falls below `min_n`. Adds a logical `suppressed` column to the
#' returned data frame.
#'
#' Uses base-R row indexing rather than `dplyr::across()` + `dplyr::if_else()`
#' to avoid the NA-assignment-into-list-column ambiguity documented in CLAUDE.md
#' (same pattern as `compute_distributions()` small-n suppression).
#'
#' @param df A data frame.
#' @param n_col Character scalar. Name of the column holding the group count.
#' @param min_n Integer. Groups below this size are suppressed.
#' @param suppress_cols Character vector of column names to set to `NA`.
#'
#' @return `df` with `suppress_cols` set to `NA_real_` where suppressed,
#'   plus a logical `suppressed` column.
#' @export
suppress_small_n_enroll <- function(df, n_col, min_n, suppress_cols) {
  stopifnot(
    is.data.frame(df),
    is.character(n_col), length(n_col) == 1L, n_col %in% names(df),
    is.numeric(min_n) || is.integer(min_n), length(min_n) == 1L,
    is.character(suppress_cols),
    all(suppress_cols %in% names(df))
  )
  small <- !is.na(df[[n_col]]) & df[[n_col]] < min_n
  if (any(small)) {
    df[small, suppress_cols] <- NA_real_
  }
  df[["suppressed"]] <- small
  df
}
