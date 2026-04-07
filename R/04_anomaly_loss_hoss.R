# ==============================================================================
# Line 4 — Score Quality Analysis: HOSS/LOSS Pileup Detection
#
# Functions:
#   compute_loss_hoss_table()      — aggregate near-LOSS/HOSS rates with flags
#   plot_loss_hoss_cleveland()     — Cleveland dot plot by grade/year
#   plot_loss_hoss_heatmap()       — heatmap of pct near LOSS or HOSS
#   plot_loss_hoss_over_time()     — line plot over years
#   summarize_loss_hoss_findings() — text narrative + summary tables
#   run_loss_hoss()                — entry point; returns the sub-list
# ==============================================================================


# ==============================================================================
# 1. Compute table
# ==============================================================================

#' Compute HOSS/LOSS pileup rates by year, subject, and grade
#'
#' Joins student-level data to the LOSS/HOSS boundaries from `assessment_spec`,
#' flags students within `point_adjust` points of either boundary, aggregates
#' to year × subject × grade, applies severity flags, and suppresses cells
#' with fewer than `min_n` students.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Must contain `YEAR`, `SUBJECT`, `GRADE`, and
#'   `SCALE_SCORE`.
#' @param assessment_spec Named list describing the assessment program
#'   (see assessment_spec). Must contain `$subjects` and `$scale_scores`.
#' @param point_adjust Integer. Number of scale-score points within the
#'   boundary that counts as "near" LOSS or HOSS. Default `5`.
#' @param min_n Integer. Minimum group size; cells with fewer students have
#'   `pct_near_loss` and `pct_near_hoss` set to `NA`. Default `10`.
#' @param mild_thresh Numeric. Percentage threshold for a Mild flag. Default `5`.
#' @param moderate_thresh Numeric. Percentage threshold for a Moderate flag.
#'   Default `10`.
#' @param severe_thresh Numeric. Percentage threshold for a Severe flag.
#'   Default `15`.
#'
#' @return A data frame with one row per year × subject × grade combination,
#'   columns: `YEAR`, `SUBJECT`, `GRADE`, `LOSS`, `HOSS`, `n`,
#'   `n_near_loss`, `n_near_hoss`, `pct_near_loss`, `pct_near_hoss`,
#'   `small_n`, `hoss_flag`, `loss_flag`, `pct_near_hoss_change`,
#'   `pct_near_loss_change`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
#' }
compute_loss_hoss_table <- function(student_results_long,
                                    assessment_spec,
                                    point_adjust     = 5,
                                    min_n            = 10,
                                    mild_thresh      = 5,
                                    moderate_thresh  = 10,
                                    severe_thresh    = 15) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    !is.null(assessment_spec$subjects),
    !is.null(assessment_spec$scale_scores),
    all(c("YEAR", "SUBJECT", "GRADE", "SCALE_SCORE") %in%
          names(student_results_long))
  )

  # ----------------------------------------------------------
  # 1) Build LOSS/HOSS lookup from assessment_spec
  #    Uses the flat key format: "SUBJECT_GRADE" or "SUBJECT"
  # ----------------------------------------------------------
  loss_hoss_lookup <- build_loss_hoss_lookup(assessment_spec)

  # ----------------------------------------------------------
  # 2) Flag students near LOSS / HOSS
  # ----------------------------------------------------------
  student_flagged <- student_results_long |>
    dplyr::mutate(GRADE = as.integer(.data[["GRADE"]])) |>
    dplyr::left_join(loss_hoss_lookup, by = c("SUBJECT", "GRADE")) |>
    dplyr::mutate(
      near_loss = .data[["SCALE_SCORE"]] <= (.data[["LOSS"]] + point_adjust),
      near_hoss = .data[["SCALE_SCORE"]] >= (.data[["HOSS"]] - point_adjust)
    )

  # ----------------------------------------------------------
  # 3) Aggregate to year × subject × grade
  # ----------------------------------------------------------
  loss_hoss_table <- student_flagged |>
    dplyr::group_by(.data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::summarize(
      LOSS         = dplyr::first(.data[["LOSS"]]),
      HOSS         = dplyr::first(.data[["HOSS"]]),
      n            = dplyr::n(),
      n_near_loss  = sum(.data[["near_loss"]], na.rm = TRUE),
      n_near_hoss  = sum(.data[["near_hoss"]], na.rm = TRUE),
      pct_near_loss = 100 * .data[["n_near_loss"]] / .data[["n"]],
      pct_near_hoss = 100 * .data[["n_near_hoss"]] / .data[["n"]],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      small_n       = .data[["n"]] < min_n,
      pct_near_loss = ifelse(.data[["small_n"]], NA_real_, .data[["pct_near_loss"]]),
      pct_near_hoss = ifelse(.data[["small_n"]], NA_real_, .data[["pct_near_hoss"]])
    )

  # ----------------------------------------------------------
  # 4) Apply severity flags
  # ----------------------------------------------------------
  loss_hoss_table <- loss_hoss_table |>
    dplyr::mutate(
      hoss_flag = dplyr::case_when(
        .data[["pct_near_hoss"]] >= severe_thresh   ~ "Severe",
        .data[["pct_near_hoss"]] >= moderate_thresh ~ "Moderate",
        .data[["pct_near_hoss"]] >= mild_thresh     ~ "Mild",
        !is.na(.data[["pct_near_hoss"]])            ~ "None",
        TRUE                                        ~ NA_character_
      ),
      loss_flag = dplyr::case_when(
        .data[["pct_near_loss"]] >= severe_thresh   ~ "Severe",
        .data[["pct_near_loss"]] >= moderate_thresh ~ "Moderate",
        .data[["pct_near_loss"]] >= mild_thresh     ~ "Mild",
        !is.na(.data[["pct_near_loss"]])            ~ "None",
        TRUE                                        ~ NA_character_
      )
    ) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]])

  # ----------------------------------------------------------
  # 5) Year-over-year change columns
  #    NA when: first year in group, or either year is suppressed (small_n)
  # ----------------------------------------------------------
  loss_hoss_table |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::mutate(
      pct_near_hoss_change = dplyr::if_else(
        !.data[["small_n"]] & !dplyr::lag(.data[["small_n"]], default = TRUE),
        .data[["pct_near_hoss"]] - dplyr::lag(.data[["pct_near_hoss"]]),
        NA_real_
      ),
      pct_near_loss_change = dplyr::if_else(
        !.data[["small_n"]] & !dplyr::lag(.data[["small_n"]], default = TRUE),
        .data[["pct_near_loss"]] - dplyr::lag(.data[["pct_near_loss"]]),
        NA_real_
      )
    ) |>
    dplyr::ungroup()
}


# ==============================================================================
# 2. Cleveland dot plot
# ==============================================================================

#' Cleveland dot plot of near-LOSS or near-HOSS percentages
#'
#' Displays the percentage of students near the LOSS or HOSS boundary for each
#' grade (y-axis), with years differentiated by colour. A horizontal line spans
#' the min–max range across years for each grade to show year-to-year movement.
#' Faceted by subject with subjects arranged as columns so the plot stays
#' page-width and fits comfortably on a portrait page.
#'
#' @param loss_hoss_table Data frame returned by [compute_loss_hoss_table()].
#' @param metric Character. `"hoss"` (default) or `"loss"`.
#' @param point_size Numeric. Size of plotted points. Default `3`.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
#' plot_loss_hoss_cleveland(tbl, metric = "hoss")
#' }
plot_loss_hoss_cleveland <- function(loss_hoss_table,
                                     metric     = c("hoss", "loss"),
                                     point_size = 3) {
  stopifnot(is.data.frame(loss_hoss_table))
  metric    <- match.arg(metric)
  pct_var   <- if (metric == "hoss") "pct_near_hoss" else "pct_near_loss"
  title_lbl <- if (metric == "hoss") "HOSS" else "LOSS"

  dat <- loss_hoss_table |>
    dplyr::filter(!is.na(.data[[pct_var]])) |>
    dplyr::mutate(
      grade    = factor(paste0("Grade ", as.integer(.data[["GRADE"]])),
                        levels = paste0("Grade ", sort(unique(as.integer(.data[["GRADE"]])),
                                                       decreasing = TRUE))),
      subject  = .data[["SUBJECT"]],
      year_lbl = factor(.data[["YEAR"]])
    )

  if (nrow(dat) == 0) {
    return(
      ggplot2::ggplot(data.frame(x = 0.5, y = 0.5,
                                  label = paste("No", title_lbl, "data available"))) +
        ggplot2::geom_text(ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                                        label = .data[["label"]]),
                           size = 5, color = "grey40") +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::theme_void()
    )
  }

  # Range ribbon: min/max across years per grade × subject
  # Filter to groups with at least one non-NA value to avoid Inf warnings
  range_dat <- dat |>
    dplyr::filter(!is.na(.data[[pct_var]])) |>
    dplyr::group_by(.data[["grade"]], .data[["subject"]]) |>
    dplyr::summarise(
      x_min = min(.data[[pct_var]], na.rm = TRUE),
      x_max = max(.data[[pct_var]], na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x     = .data[[pct_var]],
      y     = .data[["grade"]],
      color = .data[["year_lbl"]]
    )
  ) +
    ggplot2::geom_segment(
      data = range_dat,
      ggplot2::aes(x = .data[["x_min"]], xend = .data[["x_max"]],
                   y = .data[["grade"]], yend = .data[["grade"]]),
      color = "grey70", linewidth = 0.6, inherit.aes = FALSE
    ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::facet_wrap(~ subject, nrow = 1, scales = "free_x") +
    ggplot2::scale_x_continuous(limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = paste0("Percent of Students Near ", title_lbl,
                     " by Subject and Grade"),
      x     = paste0("% Near ", title_lbl),
      y     = NULL,
      color = "Year"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text         = ggplot2::element_text(face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "bottom"
    )
}


# ==============================================================================
# 3. Heatmap
# ==============================================================================

#' Heatmap of near-LOSS or near-HOSS percentages
#'
#' Displays a tile heatmap with year on the x-axis, grade on the y-axis, and
#' fill intensity proportional to the percentage of students near the boundary.
#' Faceted by subject.
#'
#' @param loss_hoss_table Data frame returned by [compute_loss_hoss_table()].
#' @param metric Character. `"hoss"` (default) or `"loss"`.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
#' plot_loss_hoss_heatmap(tbl, metric = "hoss")
#' }
plot_loss_hoss_heatmap <- function(loss_hoss_table,
                                   metric = c("hoss", "loss")) {
  stopifnot(is.data.frame(loss_hoss_table))
  metric    <- match.arg(metric)
  pct_var   <- if (metric == "hoss") "pct_near_hoss" else "pct_near_loss"
  title_lbl <- if (metric == "hoss") "HOSS" else "LOSS"

  ggplot2::ggplot(
    loss_hoss_table |>
      dplyr::filter(!is.na(.data[[pct_var]])),
    ggplot2::aes(
      x    = factor(.data[["YEAR"]]),
      y    = factor(.data[["GRADE"]]),
      fill = .data[[pct_var]]
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::facet_wrap(~ SUBJECT) +
    ggplot2::scale_fill_viridis_c(option = "magma", na.value = "grey90") +
    ggplot2::labs(
      title = paste0("Percent of Students Near ", title_lbl),
      x     = "Year",
      y     = "Grade",
      fill  = paste0("% Near ", title_lbl)
    ) +
    ggplot2::theme_minimal()
}


# ==============================================================================
# 4. Line plot over time
# ==============================================================================

#' Line plot of near-LOSS or near-HOSS percentages over time
#'
#' Plots the percentage of students near each boundary across years, with one
#' line per grade, faceted by subject.
#'
#' @param loss_hoss_table Data frame returned by [compute_loss_hoss_table()].
#' @param metric Character. `"hoss"` (default) or `"loss"`.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
#' plot_loss_hoss_over_time(tbl, metric = "hoss")
#' }
plot_loss_hoss_over_time <- function(loss_hoss_table,
                                     metric = c("hoss", "loss")) {
  stopifnot(is.data.frame(loss_hoss_table))
  metric    <- match.arg(metric)
  pct_var   <- if (metric == "hoss") "pct_near_hoss" else "pct_near_loss"
  title_lbl <- if (metric == "hoss") "HOSS" else "LOSS"

  ggplot2::ggplot(
    loss_hoss_table |>
      dplyr::filter(!is.na(.data[[pct_var]])),
    ggplot2::aes(
      x     = .data[["YEAR"]],
      y     = .data[[pct_var]],
      color = factor(.data[["GRADE"]]),
      group = .data[["GRADE"]]
    )
  ) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(~ SUBJECT, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Percent of Students Near ", title_lbl, " Over Time"),
      x     = "Year",
      y     = paste0("Percent Near ", title_lbl),
      color = "Grade"
    ) +
    ggplot2::theme_minimal()
}


# ==============================================================================
# 5. Text summary
# ==============================================================================

#' Summarize HOSS/LOSS findings into narrative text and tables
#'
#' Produces a subject × grade summary of the worst-observed flag level across
#' all available years, a subject-level narrative string, and an overall
#' narrative paragraph ready for inline Quarto rendering.
#'
#' @param loss_hoss_table Data frame returned by [compute_loss_hoss_table()].
#'
#' @return A named list with:
#'   * `$table` — data frame of max flag per subject/grade
#'   * `$by_subject` — data frame of subject-level narrative summaries
#'   * `$narrative` — character vector of paragraph-length narrative strings
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
#' txt <- summarize_loss_hoss_findings(tbl)
#' cat(txt$narrative, sep = "\n\n")
#' }
summarize_loss_hoss_findings <- function(loss_hoss_table) {
  stopifnot(
    is.data.frame(loss_hoss_table),
    all(c("SUBJECT", "GRADE", "YEAR", "loss_flag", "hoss_flag") %in%
          names(loss_hoss_table))
  )

  flag_levels <- c("None", "Mild", "Moderate", "Severe")

  # ----------------------------------------------------------
  # 1) Subject × grade: worst flag across all years
  # ----------------------------------------------------------
  summary_tbl <- loss_hoss_table |>
    dplyr::arrange(.data[["YEAR"]]) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::summarize(
      max_hoss_flag  = dplyr::last(
        factor(.data[["hoss_flag"]], levels = flag_levels, ordered = TRUE)
      ),
      max_loss_flag  = dplyr::last(
        factor(.data[["loss_flag"]], levels = flag_levels, ordered = TRUE)
      ),
      years_observed = dplyr::n(),
      .groups = "drop"
    )

  # ----------------------------------------------------------
  # 2) Subject-level narrative
  # ----------------------------------------------------------
  by_subject <- summary_tbl |>
    tidyr::pivot_longer(
      cols      = c("max_hoss_flag", "max_loss_flag"),
      names_to  = "type",
      values_to = "flag"
    ) |>
    dplyr::mutate(
      type = ifelse(.data[["type"]] == "max_hoss_flag", "Ceiling", "Floor"),
      flag = as.character(.data[["flag"]])
    ) |>
    dplyr::filter(.data[["flag"]] != "None") |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["type"]], .data[["flag"]]) |>
    dplyr::summarize(
      grades = paste(sort(unique(.data[["GRADE"]])), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data[["SUBJECT"]]) |>
    dplyr::summarize(
      summary_text = paste(
        paste0(.data[["type"]], " effects (", .data[["flag"]],
               ") in grades: ", .data[["grades"]]),
        collapse = "; "
      ),
      .groups = "drop"
    )

  # ----------------------------------------------------------
  # 3) Build bulleted lists of flagged year-subject-grade combinations
  # ----------------------------------------------------------
  make_flag_lines <- function(tbl, flag_col, label) {
    flagged <- tbl |>
      dplyr::filter(.data[[flag_col]] != "None" & !is.na(.data[[flag_col]])) |>
      dplyr::arrange(
        factor(.data[[flag_col]], levels = rev(flag_levels)),  # Severe first
        .data[["SUBJECT"]],
        .data[["GRADE"]]
      ) |>
      dplyr::mutate(
        line = paste0(
          "  - ", .data[["SUBJECT"]],
          ifelse(is.na(.data[["GRADE"]]), "", paste0(" Grade ", .data[["GRADE"]])),
          ", ", .data[["YEAR"]],
          " (", .data[[flag_col]], ")"
        )
      )
    if (nrow(flagged) == 0) return(paste0("No ", label, " effects flagged."))
    paste0(
      paste0(label, " effects flagged in the following year\u2013subject\u2013grade combinations:\n\n"),
      paste(flagged[["line"]], collapse = "\n")
    )
  }

  narrative <- c(
    make_flag_lines(loss_hoss_table, "hoss_flag", "Ceiling"),
    make_flag_lines(loss_hoss_table, "loss_flag",  "Floor")
  )

  list(
    table      = summary_tbl,
    by_subject = by_subject,
    narrative  = narrative
  )
}


# ==============================================================================
# 6. Score distribution histograms for flagged grades (one per subject/grade)
# ==============================================================================

#' Individual histograms for each grade flagged for HOSS/LOSS pileups
#'
#' Produces one histogram per flagged subject x grade combination. Each
#' histogram shows all years overlaid as separate density curves and includes
#' vertical lines for the HOSS/LOSS boundary, the `point_adjust` threshold,
#' and every cut score defined in `assessment_spec$scale_scores` for that
#' subject/grade. Each plot uses its own x-axis range. Subjects and grades with
#' no flags are omitted.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract).
#' @param assessment_spec Named list describing the assessment program.
#' @param loss_hoss_table Data frame returned by [compute_loss_hoss_table()].
#' @param distribution_table Optional data frame returned by
#'   [compute_distributions()]. When provided, per-year skewness and Sarle's
#'   bimodality coefficient are added to the in-plot annotation. Default `NULL`.
#' @param point_adjust Integer. Points from boundary used as the threshold
#'   line. Should match the value used in [compute_loss_hoss_table()].
#'   Default `5`.
#' @param x_margin Numeric. Extra scale-score points added outside the LOSS and
#'   HOSS boundaries when setting the x-axis limits. Default `10`.
#'
#' @return A flat named list of ggplot objects, one per subject/grade
#'   combination (e.g. `"ELA_Grade3"`). Each plot shows overlaid density
#'   curves for all years with LOSS and HOSS boundary lines, cut score lines,
#'   and a per-year statistics annotation.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' tbl   <- compute_loss_hoss_table(demo_data, assessment_spec)
#' plots <- plot_loss_hoss_distributions(demo_data, assessment_spec, tbl)
#' plots[["ELA_Grade3"]]
#' }
plot_loss_hoss_distributions <- function(student_results_long,
                                         assessment_spec,
                                         loss_hoss_table,
                                         distribution_table = NULL,
                                         point_adjust = 5,
                                         x_margin = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    is.data.frame(loss_hoss_table),
    all(c("YEAR", "SUBJECT", "GRADE", "SCALE_SCORE") %in%
          names(student_results_long)),
    all(c("YEAR", "SUBJECT", "GRADE", "pct_near_hoss", "pct_near_loss",
          "HOSS", "LOSS") %in% names(loss_hoss_table))
  )

  all_cells <- loss_hoss_table |>
    dplyr::distinct(.data[["SUBJECT"]], .data[["GRADE"]])

  if (nrow(all_cells) == 0) return(list())

  has_dist <- !is.null(distribution_table) &&
              is.data.frame(distribution_table) &&
              nrow(distribution_table) > 0 &&
              all(c("skewness", "bimodality_coefficient") %in%
                    names(distribution_table))

  plots <- lapply(seq_len(nrow(all_cells)), function(i) {
    subj  <- all_cells[["SUBJECT"]][i]
    grade <- all_cells[["GRADE"]][i]

    cell_rows <- loss_hoss_table[
      loss_hoss_table[["SUBJECT"]] == subj &
      (if (is.na(grade)) is.na(loss_hoss_table[["GRADE"]])
       else !is.na(loss_hoss_table[["GRADE"]]) &
            loss_hoss_table[["GRADE"]] == grade), ]

    first_row <- utils::head(cell_rows, 1)
    loss_val  <- first_row[["LOSS"]]
    hoss_val  <- first_row[["HOSS"]]
    x_lo      <- loss_val - x_margin
    x_hi      <- hoss_val + x_margin

    # Cut scores from spec
    spec_key <- if (is.na(grade)) subj else paste(subj, grade, sep = "_")
    cuts     <- assessment_spec$scale_scores[[spec_key]]$cuts

    grade_lbl <- if (is.na(grade)) "EOC" else paste0("Grade ", grade)
    title_str <- paste0(subj, " \u2014 ", grade_lbl)

    # Per-year annotation: % near HOSS/LOSS and (if available) shape stats
    yrs <- sort(unique(cell_rows[["YEAR"]]))
    ann_rows <- vapply(yrs, function(yr) {
      lh    <- cell_rows[cell_rows[["YEAR"]] == yr, ]
      pct_h <- if (nrow(lh) > 0 && !is.na(lh[["pct_near_hoss"]][1]))
                 sprintf("%.1f%%", lh[["pct_near_hoss"]][1]) else "\u2014"
      pct_l <- if (nrow(lh) > 0 && !is.na(lh[["pct_near_loss"]][1]))
                 sprintf("%.1f%%", lh[["pct_near_loss"]][1]) else "\u2014"
      line  <- sprintf("%s  \u2191HOSS %s  \u2193LOSS %s", yr, pct_h, pct_l)

      if (has_dist) {
        dt <- distribution_table[
          distribution_table[["SUBJECT"]] == subj &
          (if (is.na(grade)) is.na(distribution_table[["GRADE"]])
           else !is.na(distribution_table[["GRADE"]]) &
                distribution_table[["GRADE"]] == grade) &
          distribution_table[["YEAR"]] == yr, ]
        skew <- if (nrow(dt) > 0 && !is.na(dt[["skewness"]][1]))
                  sprintf("%.2f", dt[["skewness"]][1]) else "\u2014"
        bc   <- if (nrow(dt) > 0 && !is.na(dt[["bimodality_coefficient"]][1]))
                  sprintf("%.3f", dt[["bimodality_coefficient"]][1]) else "\u2014"
        line <- paste0(line, sprintf("  Skew %s  BC %s", skew, bc))
      }
      line
    }, character(1))

    ann_text <- paste(ann_rows, collapse = "\n")

    # Student data for this subject/grade, all years
    dat <- student_results_long |>
      dplyr::filter(
        .data[["SUBJECT"]] == subj,
        if (is.na(grade)) is.na(.data[["GRADE"]]) else .data[["GRADE"]] == grade
      ) |>
      dplyr::mutate(year_lbl = factor(.data[["YEAR"]]))

    p <- ggplot2::ggplot(
      dat,
      ggplot2::aes(x = .data[["SCALE_SCORE"]], color = .data[["year_lbl"]])
    ) +
      ggplot2::geom_density(linewidth = 0.7) +
      # HOSS boundary (red dashed)
      ggplot2::geom_vline(xintercept = hoss_val, linetype = "dashed",
                          color = "#cc0000", linewidth = 0.9) +
      ggplot2::annotate("text", x = hoss_val, y = Inf, label = " HOSS",
                        hjust = 0, vjust = 1.5, color = "#cc0000", size = 2.8) +
      # LOSS boundary (blue dashed)
      ggplot2::geom_vline(xintercept = loss_val, linetype = "dashed",
                          color = "#1565c0", linewidth = 0.9) +
      ggplot2::annotate("text", x = loss_val, y = Inf, label = "LOSS ",
                        hjust = 1, vjust = 1.5, color = "#1565c0", size = 2.8)

    # Cut score lines within plot range
    if (!is.null(cuts) && length(cuts) > 0) {
      cuts_in <- cuts[cuts > x_lo & cuts < x_hi]
      if (length(cuts_in) > 0) {
        p <- p +
          ggplot2::geom_vline(
            data = data.frame(cut = cuts_in),
            ggplot2::aes(xintercept = .data[["cut"]]),
            linetype = "solid", color = "grey60", linewidth = 0.4,
            inherit.aes = FALSE
          )
      }
    }

    # Per-year stats annotation (top-left)
    p +
      ggplot2::annotate(
        "text",
        x = -Inf, y = Inf,
        label = ann_text,
        hjust = -0.03, vjust = 1.1,
        size = 2.3, color = "grey25"
      ) +
      ggplot2::scale_x_continuous(limits = c(x_lo, x_hi)) +
      ggplot2::labs(
        title = title_str,
        x     = "Scale Score",
        y     = "Density",
        color = "Year"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(face = "bold"),
        legend.position = "right"
      )
  })

  nms <- paste0(
    all_cells[["SUBJECT"]], "_",
    ifelse(is.na(all_cells[["GRADE"]]), "EOC",
           paste0("Grade", all_cells[["GRADE"]]))
  )
  stats::setNames(plots, nms)
}


# ==============================================================================
# 7. Sub-list entry point
# ==============================================================================

#' Run the HOSS/LOSS pileup analysis and return the results sub-list
#'
#' Orchestrates `compute_loss_hoss_table()`, all four plot functions, and
#' `summarize_loss_hoss_findings()` into the standardised sub-list consumed by
#' [run_anomaly_analysis()].
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param point_adjust Integer. Points from boundary to flag as "near".
#'   Default `5`.
#' @param min_n Integer. Minimum group size for suppression. Default `10`.
#' @param mild_thresh Numeric. Percent threshold for Mild flag. Default `5`.
#' @param moderate_thresh Numeric. Percent threshold for Moderate flag.
#'   Default `10`.
#' @param severe_thresh Numeric. Percent threshold for Severe flag.
#'   Default `15`.
#'
#' @return A named list with elements:
#'   `$table`, `$plot_hoss_cleveland`, `$plot_loss_cleveland`,
#'   `$plot_hoss_heat`, `$plot_loss_heat`,
#'   `$plot_hoss_line`, `$plot_loss_line`, `$plot_distributions`, `$text`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' sub <- run_loss_hoss(demo_data, assessment_spec)
#' }
run_loss_hoss <- function(student_results_long,
                          assessment_spec,
                          point_adjust    = 5,
                          min_n           = 10,
                          mild_thresh     = 5,
                          moderate_thresh = 10,
                          severe_thresh   = 15) {

  tbl <- compute_loss_hoss_table(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    point_adjust         = point_adjust,
    min_n                = min_n,
    mild_thresh          = mild_thresh,
    moderate_thresh      = moderate_thresh,
    severe_thresh        = severe_thresh
  )

  list(
    table               = tbl,
    plot_hoss_cleveland = plot_loss_hoss_cleveland(tbl, metric = "hoss"),
    plot_loss_cleveland = plot_loss_hoss_cleveland(tbl, metric = "loss"),
    plot_hoss_heat      = plot_loss_hoss_heatmap(tbl,   metric = "hoss"),
    plot_loss_heat      = plot_loss_hoss_heatmap(tbl,   metric = "loss"),
    plot_hoss_line      = plot_loss_hoss_over_time(tbl, metric = "hoss"),
    plot_loss_line      = plot_loss_hoss_over_time(tbl, metric = "loss"),
    plot_distributions  = plot_loss_hoss_distributions(
                            student_results_long = student_results_long,
                            assessment_spec      = assessment_spec,
                            loss_hoss_table      = tbl,
                            point_adjust         = point_adjust
                          ),
    text                = summarize_loss_hoss_findings(tbl)
  )
}
