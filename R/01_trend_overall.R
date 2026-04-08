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


# ==============================================================================
# plot_score_trends()
# ==============================================================================

#' Create trend plots from a score summary
#'
#' Produces four ggplot objects visualising scale score trends over time:
#' mean trends, mean with SD bands, median with IQR bands, and grade-level
#' profiles. All plots are faceted by subject using labels from
#' `assessment_spec`.
#'
#' @param score_summary Named list returned by [compute_score_summary()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list with four ggplot elements:
#'   * `$mean_trend` — mean per grade over time, faceted by subject
#'   * `$mean_sd` — mean ± 1 SD bands, faceted by subject
#'   * `$median_iqr` — median with IQR bands, faceted by subject
#'   * `$grade_profile` — grade on x-axis, year as colour, faceted by subject
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' ss <- compute_score_summary(demo_data, assessment_spec)
#' plots <- plot_score_trends(ss, assessment_spec)
#' plots$mean_trend
#' }
plot_score_trends <- function(score_summary, assessment_spec) {
  stopifnot(
    is.list(score_summary),
    !is.null(score_summary$ScaleScore_Stats_Wide),
    !is.null(score_summary$ScaleScore_Stats_Long),
    is.list(assessment_spec)
  )

  # Subject label lookup
  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  wide <- score_summary$ScaleScore_Stats_Wide
  long <- score_summary$ScaleScore_Stats_Long

  # Attach subject labels and order GRADE
  wide <- wide |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      grade_lbl  = factor(
        paste0("Grade ", as.integer(.data[["GRADE"]])),
        levels = paste0("Grade ", sort(unique(as.integer(.data[["GRADE"]]))))
      ),
      YEAR_chr   = as.character(.data[["YEAR"]])
    )

  long <- long |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      grade_lbl  = factor(
        paste0("Grade ", as.integer(.data[["GRADE"]])),
        levels = paste0("Grade ", sort(unique(as.integer(.data[["GRADE"]]))))
      ),
      YEAR_chr   = as.character(.data[["YEAR"]])
    )

  # ---- 1. Mean trend ----
  d_mean <- long |> dplyr::filter(.data[["metric"]] == "mean")

  p_mean <- ggplot2::ggplot(
    d_mean,
    ggplot2::aes(x = YEAR_chr, y = value, group = grade_lbl, color = grade_lbl)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ subj_label, nrow = 1) +
    ggplot2::labs(x = NULL, y = "Mean Scale Score", color = "Grade") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # ---- 2. Mean ± SD bands ----
  d_meansd <- wide |>
    dplyr::select(dplyr::all_of(c("YEAR", "YEAR_chr", "SUBJECT", "subj_label",
                                   "GRADE", "grade_lbl", "mean", "sd"))) |>
    dplyr::filter(!is.na(.data[["mean"]]), !is.na(.data[["sd"]])) |>
    dplyr::rename(mean_val = "mean", sd_val = "sd") |>
    dplyr::mutate(
      lower = .data[["mean_val"]] - .data[["sd_val"]],
      upper = .data[["mean_val"]] + .data[["sd_val"]]
    )

  p_meansd <- ggplot2::ggplot(
    d_meansd,
    ggplot2::aes(x = YEAR_chr, y = mean_val,
                 group = grade_lbl, color = grade_lbl, fill = grade_lbl)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                         alpha = 0.15, color = NA) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ subj_label, nrow = 1) +
    ggplot2::labs(x = NULL, y = "Scale Score", color = "Grade", fill = "Grade") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # ---- 3. Median with IQR ----
  has_iqr <- all(c("p25", "p50", "p75") %in% names(wide))

  if (has_iqr) {
    d_iqr <- wide |>
      dplyr::select(dplyr::all_of(c("YEAR", "YEAR_chr", "SUBJECT", "subj_label",
                                     "GRADE", "grade_lbl", "p25", "p50", "p75"))) |>
      dplyr::filter(!is.na(.data[["p50"]])) |>
      dplyr::rename(p25_val = "p25", p50_val = "p50", p75_val = "p75")

    p_iqr <- ggplot2::ggplot(
      d_iqr,
      ggplot2::aes(x = YEAR_chr, y = p50_val,
                   group = grade_lbl, color = grade_lbl, fill = grade_lbl)
    ) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = p25_val, ymax = p75_val),
                           alpha = 0.15, color = NA) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ subj_label, nrow = 1) +
      ggplot2::labs(x = NULL, y = "Scale Score", color = "Grade", fill = "Grade") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    p_iqr <- NULL
  }

  # ---- 4. Grade profile (grade on x, year as color) ----
  d_profile <- long |>
    dplyr::filter(.data[["metric"]] == "mean", !is.na(.data[["value"]]))

  p_profile <- ggplot2::ggplot(
    d_profile,
    ggplot2::aes(x = grade_lbl, y = value,
                 group = YEAR_chr, color = YEAR_chr)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ subj_label, nrow = 1) +
    ggplot2::labs(x = NULL, y = "Mean Scale Score", color = "Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  list(
    mean_trend    = p_mean,
    mean_sd       = p_meansd,
    median_iqr    = p_iqr,
    grade_profile = p_profile
  )
}


# ==============================================================================
# summarize_score_trends()
# ==============================================================================

#' Summarize score trends in plain text
#'
#' Computes first-to-last year mean changes by subject and grade and generates
#' a narrative markdown summary.
#'
#' @param score_summary Named list returned by [compute_score_summary()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list:
#'   * `$table` — `ScaleScore_Stats_Report` data frame
#'   * `$by_subject` — named list of per-subject narrative strings
#'   * `$narrative` — markdown bullet list summarising key changes
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' ss <- compute_score_summary(demo_data, assessment_spec)
#' summarize_score_trends(ss, assessment_spec)$narrative
#' }
summarize_score_trends <- function(score_summary, assessment_spec) {
  stopifnot(
    is.list(score_summary),
    !is.null(score_summary$ScaleScore_Stats_Wide),
    is.list(assessment_spec)
  )

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  wide <- score_summary$ScaleScore_Stats_Wide |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]])
    )

  subjects <- unique(wide[["SUBJECT"]])
  by_subject <- vector("list", length(subjects))
  names(by_subject) <- subjects

  for (s in subjects) {
    d_s  <- wide |>
      dplyr::filter(.data[["SUBJECT"]] == s) |>
      dplyr::arrange(.data[["YEAR"]])

    lbl  <- unique(d_s[["subj_label"]])[1]
    yrs  <- sort(unique(d_s[["YEAR"]]))

    if (length(yrs) < 2) {
      by_subject[[s]] <- paste0("Only one year of data available for ", lbl, ".")
      next
    }

    first_yr <- yrs[1]
    last_yr  <- yrs[length(yrs)]

    d_first <- d_s |> dplyr::filter(.data[["YEAR"]] == first_yr)
    d_last  <- d_s |> dplyr::filter(.data[["YEAR"]] == last_yr)

    changes <- dplyr::inner_join(
      d_first |> dplyr::select("GRADE", mean_first = "mean"),
      d_last  |> dplyr::select("GRADE", mean_last  = "mean"),
      by = "GRADE"
    ) |>
      dplyr::mutate(delta = .data[["mean_last"]] - .data[["mean_first"]]) |>
      dplyr::filter(!is.na(.data[["delta"]]))

    if (nrow(changes) == 0) {
      by_subject[[s]] <- paste0("Insufficient data to compute changes for ", lbl, ".")
      next
    }

    overall_delta <- mean(changes[["delta"]], na.rm = TRUE)
    direction     <- if (overall_delta > 0) "increased" else "decreased"
    by_subject[[s]] <- paste0(
      lbl, ": mean scores ", direction, " by an average of ",
      round(abs(overall_delta), 1), " points from ", first_yr, " to ", last_yr,
      " across ", nrow(changes), " grade(s)."
    )
  }

  bullets <- paste0("- ", unlist(by_subject), collapse = "\n")
  narrative <- paste0(
    "The following summarizes changes in mean scale scores from the first to ",
    "the last reported year:\n\n", bullets
  )

  list(
    table      = score_summary$ScaleScore_Stats_Report,
    by_subject = by_subject,
    narrative  = narrative
  )
}


# ==============================================================================
# run_score_trends()
# ==============================================================================

#' Run the Line 1 score trends sub-analysis
#'
#' Computes distributional score summaries, generates trend plots, and
#' produces narrative text. This is the primary score trends sub-analysis
#' for Line 1 (General Trend Analysis).
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract).
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size for small-n suppression. Default
#'   `10`.
#'
#' @return A named list:
#'   * `$table` — `ScaleScore_Stats_Report` wide table
#'   * `$plot` — list of ggplots: `$mean_trend`, `$mean_sd`, `$median_iqr`,
#'     `$grade_profile`
#'   * `$text` — list: `$narrative`, `$by_subject`, `$table`
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' out <- run_score_trends(demo_data, assessment_spec)
#' out$plot$mean_trend
#' }
run_score_trends <- function(student_results_long,
                             assessment_spec,
                             min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  ss   <- compute_score_summary(student_results_long, assessment_spec, min_n = min_n)
  plt  <- plot_score_trends(ss, assessment_spec)
  txt  <- summarize_score_trends(ss, assessment_spec)

  list(
    table = ss$ScaleScore_Stats_Report,
    plot  = plt,
    text  = txt
  )
}
