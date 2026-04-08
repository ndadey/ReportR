# ==============================================================================
# Line 1 — General Trend Analysis: Score Distributions
#
# Functions:
#   plot_score_density()      — kernel density plots per subject
#   summarize_score_density() — narrative text
#   run_score_density()       — orchestrator
# ==============================================================================


#' Plot scale score distributions by subject
#'
#' Creates one kernel density plot per subject. Within each plot, grades are
#' shown as facets and years are overlaid as separate coloured curves. The
#' x-axis is constrained to the LOSS/HOSS range for that subject (with a small
#' margin) using `assessment_spec`.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Must contain `YEAR`, `SUBJECT`, `GRADE`, and
#'   `SCALE_SCORE`.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Grades with fewer than `min_n` students in a given
#'   year are excluded from the density. Default `10`.
#'
#' @return A named list of ggplot objects, one per subject (keyed by subject
#'   code, e.g. `"ELA"`).
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' plots <- plot_score_density(demo_data, assessment_spec)
#' plots[["ELA"]]
#' }
plot_score_density <- function(student_results_long,
                               assessment_spec,
                               min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("YEAR", "SUBJECT", "GRADE", "SCALE_SCORE") %in%
          names(student_results_long))
  )

  subjects <- names(assessment_spec$subjects)
  out      <- vector("list", length(subjects))
  names(out) <- subjects

  for (s in subjects) {
    subj_label <- assessment_spec$subjects[[s]]$label
    grades_in_spec <- assessment_spec$subjects[[s]]$grades

    d_s <- student_results_long |>
      dplyr::filter(
        .data[["SUBJECT"]] == s,
        !is.na(.data[["SCALE_SCORE"]]),
        !is.na(.data[["GRADE"]])
      )

    if (nrow(d_s) == 0) {
      out[[s]] <- NULL
      next
    }

    # Small-n filter per YEAR × GRADE
    d_s <- d_s |>
      dplyr::group_by(.data[["YEAR"]], .data[["GRADE"]]) |>
      dplyr::filter(dplyr::n() >= min_n) |>
      dplyr::ungroup()

    if (nrow(d_s) == 0) {
      out[[s]] <- NULL
      next
    }

    # Grade factor ordering
    grade_vals <- sort(unique(as.integer(d_s[["GRADE"]])))
    grade_levels <- paste0("Grade ", grade_vals)

    d_s <- d_s |>
      dplyr::mutate(
        grade_lbl = factor(
          paste0("Grade ", as.integer(.data[["GRADE"]])),
          levels = grade_levels
        ),
        YEAR_chr  = as.character(.data[["YEAR"]])
      )

    # X-axis limits from LOSS/HOSS across all grades in this subject
    x_margin <- 10
    if (!is.null(grades_in_spec)) {
      loss_hoss_vals <- lapply(grades_in_spec, function(g) {
        ss <- get_score_spec(assessment_spec, s, g)
        if (is.null(ss)) return(NULL)
        c(ss$loss, ss$hoss)
      })
    } else {
      ss <- get_score_spec(assessment_spec, s)
      loss_hoss_vals <- if (!is.null(ss)) list(c(ss$loss, ss$hoss)) else NULL
    }

    loss_hoss_vals <- loss_hoss_vals[!vapply(loss_hoss_vals, is.null, logical(1))]

    if (length(loss_hoss_vals) > 0) {
      all_vals <- unlist(loss_hoss_vals)
      x_lo <- min(all_vals) - x_margin
      x_hi <- max(all_vals) + x_margin
    } else {
      x_lo <- min(d_s[["SCALE_SCORE"]], na.rm = TRUE) - x_margin
      x_hi <- max(d_s[["SCALE_SCORE"]], na.rm = TRUE) + x_margin
    }

    p <- ggplot2::ggplot(
      d_s,
      ggplot2::aes(x = SCALE_SCORE, color = YEAR_chr, group = YEAR_chr)
    ) +
      ggplot2::geom_density(adjust = 1, na.rm = TRUE) +
      ggplot2::facet_wrap(~ grade_lbl, nrow = 1) +
      ggplot2::coord_cartesian(xlim = c(x_lo, x_hi)) +
      ggplot2::labs(
        title  = subj_label,
        x      = "Scale Score",
        y      = "Density",
        color  = "Year"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")

    out[[s]] <- p
  }

  out
}


#' Summarize score density in plain text
#'
#' Returns a brief narrative describing what the score density plots show.
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list with one element: `$narrative` (character string).
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' summarize_score_density(demo_data, assessment_spec)$narrative
#' }
summarize_score_density <- function(student_results_long, assessment_spec) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  prog <- assessment_spec$program$short_name %||%
    assessment_spec$program$name %||% "the assessment"

  narrative <- paste0(
    "The figures below show how scale score distributions have shifted over ",
    "time for each subject in ", prog, ". Within each panel, curves represent ",
    "individual testing years overlaid on the same axes. Grades are shown as ",
    "separate facets. Distributional changes — such as a rightward shift ",
    "(improving scores), increased spread, or the emergence of bimodal ",
    "patterns — can indicate meaningful trends in student performance."
  )

  list(narrative = narrative)
}

# Internal null-coalescing helper (not exported)
`%||%` <- function(a, b) if (!is.null(a)) a else b


#' Run the Line 1 score distributions sub-analysis
#'
#' Produces kernel density plots showing scale score distributional shifts
#' over time by subject and grade.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract).
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size; grades below this in a given year
#'   are excluded. Default `10`.
#'
#' @return A named list:
#'   * `$plot` — list: `$density` (named list of ggplots, one per subject)
#'   * `$text` — list: `$narrative`
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' out <- run_score_density(demo_data, assessment_spec)
#' out$plot$density[["ELA"]]
#' }
run_score_density <- function(student_results_long,
                              assessment_spec,
                              min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  plt <- plot_score_density(student_results_long, assessment_spec, min_n = min_n)
  txt <- summarize_score_density(student_results_long, assessment_spec)

  list(
    plot = list(density = plt),
    text = txt
  )
}
