# ==============================================================================
# Line 1 — General Trend Analysis: Score Distributions
#
# Functions:
#   .build_grade_density_plot() — package-level helper (not exported)
#   plot_score_density()        — kernel density plots per subject
#   summarize_score_density()   — narrative text
#   run_score_density()         — orchestrator
# ==============================================================================

# Package-level helper — builds one per-grade density plot from pre-computed
# kernel density curves. Defined OUTSIDE plot_score_density() so its
# environment never holds student_results_long; ggplot quosures therefore
# capture only small objects.
.build_grade_density_plot <- function(dens_dat, ann_text_g, g_lbl,
                                       x_lo, x_hi, loss_val, hoss_val,
                                       has_lh) {
  pg <- ggplot2::ggplot(
    dens_dat,
    ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                 color = .data[["YEAR_chr"]], group = .data[["YEAR_chr"]])
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::annotate("text", x = -Inf, y = Inf, label = ann_text_g,
                      hjust = -0.03, vjust = 1.1,
                      size = 2.2, color = "grey25") +
    ggplot2::labs(title = g_lbl, x = "Scale Score",
                  y = "Density", color = "Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.na(x_lo) && !is.na(x_hi)) {
    pg <- pg + ggplot2::coord_cartesian(xlim = c(x_lo, x_hi))
  }
  if (has_lh && !is.na(loss_val) && !is.na(hoss_val)) {
    pg <- pg +
      ggplot2::geom_vline(xintercept = hoss_val, linetype = "dashed",
                          color = "#cc0000", linewidth = 0.7) +
      ggplot2::geom_vline(xintercept = loss_val, linetype = "dashed",
                          color = "#1565c0", linewidth = 0.7)
  }
  pg
}


#' Plot scale score distributions by subject
#'
#' Creates one kernel density plot per subject. Within each plot, grades are
#' shown as facets and years are overlaid as separate colored curves. The
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
    grade_vals   <- sort(unique(as.integer(d_s[["GRADE"]])))
    grade_levels <- paste0("Grade ", grade_vals)

    d_s <- d_s |>
      dplyr::mutate(
        grade_lbl = factor(
          paste0("Grade ", as.integer(.data[["GRADE"]])),
          levels = grade_levels
        ),
        YEAR_chr  = as.character(.data[["YEAR"]])
      )

    # Per-grade LOSS/HOSS — drives boundary lines and free_x scale anchoring
    lh_data <- do.call(rbind, lapply(grade_vals, function(g) {
      ss <- get_score_spec(assessment_spec, s, g)
      if (is.null(ss)) return(NULL)
      data.frame(
        grade_lbl = factor(paste0("Grade ", g), levels = grade_levels),
        loss      = ss$loss,
        hoss      = ss$hoss,
        stringsAsFactors = FALSE
      )
    }))

    has_lh <- !is.null(lh_data) && nrow(lh_data) > 0

    # Per-year stats (mean, SD, N) per YEAR x GRADE for annotation
    stats_df <- d_s |>
      dplyr::group_by(.data[["grade_lbl"]], .data[["YEAR_chr"]]) |>
      dplyr::summarize(
        n_obs   = dplyr::n(),
        mean_ss = round(mean(.data[["SCALE_SCORE"]], na.rm = TRUE), 1),
        sd_ss   = round(stats::sd(.data[["SCALE_SCORE"]], na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data[["grade_lbl"]], .data[["YEAR_chr"]]) |>
      dplyr::mutate(
        ann_line = sprintf("%s  Mean: %.1f  SD: %.1f  N: %d",
                           .data[["YEAR_chr"]], .data[["mean_ss"]],
                           .data[["sd_ss"]], .data[["n_obs"]])
      )

    ann_df <- stats_df |>
      dplyr::group_by(.data[["grade_lbl"]]) |>
      dplyr::summarize(
        ann_text = paste(.data[["ann_line"]], collapse = "\n"),
        .groups  = "drop"
      )

    # Build one ggplot per grade — pre-compute density curves (512 pts/year)
    # and delegate to package-level helper so quosures never capture
    # student_results_long.
    grade_plots <- lapply(grade_vals, function(g) {
      g_lbl <- paste0("Grade ", g)
      d_g   <- d_s[as.character(d_s[["grade_lbl"]]) == g_lbl, ]
      if (nrow(d_g) == 0L) return(NULL)

      ann_text_g <- ""
      ann_row <- ann_df[as.character(ann_df[["grade_lbl"]]) == g_lbl, ]
      if (nrow(ann_row) > 0L) ann_text_g <- ann_row[["ann_text"]][1L]

      # Per-year kernel density (512 pts) — avoids storing raw scores in ggplot
      yrs_g <- sort(unique(d_g[["YEAR_chr"]]))
      dens_rows <- lapply(yrs_g, function(yr) {
        scores <- d_g[["SCALE_SCORE"]][d_g[["YEAR_chr"]] == yr &
                                       !is.na(d_g[["SCALE_SCORE"]])]
        if (length(scores) < 2L) return(NULL)
        lh_row <- if (has_lh)
          lh_data[as.character(lh_data[["grade_lbl"]]) == g_lbl, ]
        else
          data.frame(loss = NA_real_, hoss = NA_real_)
        x_from <- if (nrow(lh_row) > 0L && !is.na(lh_row[["loss"]][1L]))
          lh_row[["loss"]][1L] else min(scores)
        x_to   <- if (nrow(lh_row) > 0L && !is.na(lh_row[["hoss"]][1L]))
          lh_row[["hoss"]][1L] else max(scores)
        d    <- stats::density(scores, n = 512L, from = x_from, to = x_to)
        data.frame(x = d$x, y = d$y, YEAR_chr = yr)
      })
      dens_dat <- do.call(rbind, dens_rows)
      if (is.null(dens_dat) || nrow(dens_dat) == 0L) return(NULL)

      lh_row   <- if (has_lh)
        lh_data[as.character(lh_data[["grade_lbl"]]) == g_lbl, ]
      else
        data.frame(loss = NA_real_, hoss = NA_real_)
      loss_val <- if (nrow(lh_row) > 0L) lh_row[["loss"]][1L] else NA_real_
      hoss_val <- if (nrow(lh_row) > 0L) lh_row[["hoss"]][1L] else NA_real_

      .build_grade_density_plot(dens_dat, ann_text_g, g_lbl,
                                 x_lo = loss_val, x_hi = hoss_val,
                                 loss_val = loss_val, hoss_val = hoss_val,
                                 has_lh = has_lh)
    })
    grade_plots <- Filter(Negate(is.null), grade_plots)

    if (length(grade_plots) == 0) {
      out[[s]] <- NULL
      next
    }

    if (requireNamespace("patchwork", quietly = TRUE)) {
      p <- patchwork::wrap_plots(grade_plots, ncol = 2) +
        patchwork::plot_annotation(title = subj_label)
    } else {
      # fallback: single plot with facets and subject-wide x range
      x_lim <- if (has_lh) c(min(lh_data[["loss"]]), max(lh_data[["hoss"]])) else NULL
      p <- ggplot2::ggplot(
        d_s,
        ggplot2::aes(x = SCALE_SCORE, color = YEAR_chr, group = YEAR_chr)
      ) +
        ggplot2::geom_density(adjust = 1, na.rm = TRUE) +
        ggplot2::facet_wrap(~ grade_lbl, ncol = 2) +
        ggplot2::labs(title = subj_label, x = "Scale Score",
                      y = "Density", color = "Year") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")
      if (!is.null(x_lim))
        p <- p + ggplot2::coord_cartesian(xlim = x_lim)
    }

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
