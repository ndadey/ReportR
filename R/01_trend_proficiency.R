# ==============================================================================
# Line 1 — General Trend Analysis: Proficiency
#
# Functions:
#   compute_proficiency()          — pct proficient by YEAR × SUBJECT × GRADE
#   plot_proficiency()             — trend line + heatmap plots
#   summarize_proficiency_findings() — narrative summary
#   run_proficiency()              — orchestrator
# ==============================================================================


#' Compute percent proficient by year, subject, and grade
#'
#' Determines the proficiency threshold from `assessment_spec$achievement_levels`
#' using [benchmark_index()] and counts students at or above that level.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Must contain `YEAR`, `SUBJECT`, `GRADE`, and
#'   `ACHIEVEMENT_LEVEL`.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Groups with fewer than `min_n` students have their
#'   statistics set to `NA`. Default `10`.
#'
#' @return A data frame with one row per `YEAR × SUBJECT × GRADE`:
#'   `n`, `n_proficient`, `pct_proficient` (NA when `n < min_n`).
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' compute_proficiency(demo_data, assessment_spec)
#' }
compute_proficiency <- function(student_results_long,
                                assessment_spec,
                                min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("YEAR", "SUBJECT", "GRADE", "ACHIEVEMENT_LEVEL") %in%
          names(student_results_long))
  )

  bench_idx <- benchmark_index(assessment_spec)
  labels    <- assessment_spec$achievement_levels$labels
  prof_labels <- labels[seq(bench_idx, length(labels))]

  out <- student_results_long |>
    dplyr::filter(!is.na(.data[["ACHIEVEMENT_LEVEL"]])) |>
    dplyr::group_by(.data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::summarise(
      n           = dplyr::n(),
      n_proficient = sum(.data[["ACHIEVEMENT_LEVEL"]] %in% prof_labels, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    dplyr::mutate(
      pct_proficient = dplyr::if_else(
        .data[["n"]] >= min_n,
        100 * .data[["n_proficient"]] / .data[["n"]],
        NA_real_
      )
    )

  out
}


#' Plot proficiency trends and heatmap
#'
#' Creates two ggplot objects from the output of [compute_proficiency()]:
#' a trend line plot and a heatmap, both faceted by subject.
#'
#' @param proficiency_table Data frame returned by [compute_proficiency()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list:
#'   * `$trend` — line plot: YEAR on x, pct_proficient on y, grade as colour,
#'     faceted by subject
#'   * `$heatmap` — heatmap: YEAR on x, grade on y, fill = pct_proficient,
#'     faceted by subject
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' prof <- compute_proficiency(demo_data, assessment_spec)
#' plots <- plot_proficiency(prof, assessment_spec)
#' plots$trend
#' }
plot_proficiency <- function(proficiency_table, assessment_spec) {
  stopifnot(
    is.data.frame(proficiency_table),
    all(c("YEAR", "SUBJECT", "GRADE", "pct_proficient") %in%
          names(proficiency_table)),
    is.list(assessment_spec)
  )

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  bench_label <- assessment_spec$achievement_levels$policy_benchmark

  dat <- proficiency_table |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      grade_lbl  = factor(
        paste0("Grade ", as.integer(.data[["GRADE"]])),
        levels = paste0("Grade ", sort(unique(as.integer(.data[["GRADE"]]))))
      ),
      YEAR_chr   = as.character(.data[["YEAR"]])
    )

  # ---- Trend line ----
  p_trend <- ggplot2::ggplot(
    dat |> dplyr::filter(!is.na(.data[["pct_proficient"]])),
    ggplot2::aes(x = YEAR_chr, y = pct_proficient,
                 group = grade_lbl, color = grade_lbl)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ subj_label, nrow = 1) +
    ggplot2::labs(
      x     = NULL,
      y     = paste0("% ", bench_label, " or Above"),
      color = "Grade"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # ---- Heatmap ----
  p_heat <- ggplot2::ggplot(
    dat,
    ggplot2::aes(x = YEAR_chr, y = grade_lbl, fill = pct_proficient)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::facet_wrap(~ subj_label, nrow = 1) +
    ggplot2::scale_fill_gradient(
      low = "#f7fbff", high = "#08519c",
      limits = c(0, 100), na.value = "grey85",
      name  = "% Proficient"
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  list(trend = p_trend, heatmap = p_heat)
}


#' Summarize proficiency trends in plain text
#'
#' Computes first-to-last year changes in percent proficient by subject and
#' produces a markdown narrative.
#'
#' @param proficiency_table Data frame returned by [compute_proficiency()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list:
#'   * `$table` — the input `proficiency_table`
#'   * `$by_subject` — named list of per-subject narrative strings
#'   * `$narrative` — markdown bullet list
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' prof <- compute_proficiency(demo_data, assessment_spec)
#' summarize_proficiency_findings(prof, assessment_spec)$narrative
#' }
summarize_proficiency_findings <- function(proficiency_table, assessment_spec) {
  stopifnot(
    is.data.frame(proficiency_table),
    is.list(assessment_spec)
  )

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  bench_label <- assessment_spec$achievement_levels$policy_benchmark

  dat <- proficiency_table |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]])
    )

  subjects    <- unique(dat[["SUBJECT"]])
  by_subject  <- vector("list", length(subjects))
  names(by_subject) <- subjects

  for (s in subjects) {
    d_s  <- dat |>
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
      d_first |> dplyr::select("GRADE", pct_first = "pct_proficient"),
      d_last  |> dplyr::select("GRADE", pct_last  = "pct_proficient"),
      by = "GRADE"
    ) |>
      dplyr::mutate(delta = .data[["pct_last"]] - .data[["pct_first"]]) |>
      dplyr::filter(!is.na(.data[["delta"]]))

    if (nrow(changes) == 0) {
      by_subject[[s]] <- paste0("Insufficient data to compute changes for ", lbl, ".")
      next
    }

    overall_delta <- mean(changes[["delta"]], na.rm = TRUE)
    direction     <- if (overall_delta > 0) "increased" else "decreased"
    by_subject[[s]] <- paste0(
      lbl, ": percent ", bench_label, " or above ", direction,
      " by an average of ", round(abs(overall_delta), 1),
      " percentage points from ", first_yr, " to ", last_yr,
      " across ", nrow(changes), " grade(s)."
    )
  }

  bullets   <- paste0("- ", unlist(by_subject), collapse = "\n")
  narrative <- paste0(
    "The following summarizes changes in percent ", bench_label,
    " or above from the first to the last reported year:\n\n", bullets
  )

  list(
    table      = proficiency_table,
    by_subject = by_subject,
    narrative  = narrative
  )
}


#' Run the Line 1 proficiency sub-analysis
#'
#' Computes percent proficient by year/subject/grade, generates trend and
#' heatmap plots, and produces narrative text.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract).
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size for small-n suppression. Default
#'   `10`.
#'
#' @return A named list:
#'   * `$table` — proficiency summary data frame
#'   * `$plot` — list of ggplots: `$trend`, `$heatmap`
#'   * `$text` — list: `$narrative`, `$by_subject`, `$table`
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' out <- run_proficiency(demo_data, assessment_spec)
#' out$plot$trend
#' }
run_proficiency <- function(student_results_long,
                            assessment_spec,
                            min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  tbl  <- compute_proficiency(student_results_long, assessment_spec, min_n = min_n)
  plt  <- plot_proficiency(tbl, assessment_spec)
  txt  <- summarize_proficiency_findings(tbl, assessment_spec)

  list(
    table = tbl,
    plot  = plt,
    text  = txt
  )
}
