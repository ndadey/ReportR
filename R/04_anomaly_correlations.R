# ==============================================================================
# Line 4 — Score Quality Analysis: Year-to-Year Correlation Diagnostics
#
# Computes cohort-style Spearman correlations: for each adjacent year-pair
# (T, T+1) and each SUBJECT, matches students whose Grade G score in year T is
# correlated with their Grade G+1 score in year T+1.  Each row in the output
# represents one SUBJECT x grade-pair (G -> G+1) x year-pair combination.
#
# Unusually low correlations or large drops across year-pairs may signal
# measurement or data issues.
#
# Functions:
#   compute_correlations()   — cohort-style score correlations
#   .make_corr_wide()        — internal helper: pivot to wide display table
#   plot_correlations()      — visualise correlation trends
#   summarize_correlations() — narrative summary + wide table
#   run_correlations()       — entry point; returns the sub-list
# ==============================================================================


#' Compute cohort-style adjacent-grade score correlations
#'
#' For each SUBJECT and adjacent year-pair (T, T+1), matches students whose
#' scale score at grade G in year T is correlated with their scale score at
#' grade G+1 in year T+1.  Each row represents one SUBJECT x grade-pair x
#' year-pair combination.
#'
#' Flags are added for unusually low correlations (< 0.5) and for drops of
#' more than 0.15 from the prior year-pair within the same SUBJECT x
#' grade-pair.
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum number of matched pairs required to report a
#'   correlation. Default `10`.
#'
#' @return A data frame with one row per SUBJECT x grade-pair x year-pair
#'   containing `YEAR_1`, `YEAR_2`, `SUBJECT`, `GRADE_1`, `GRADE_2`, `n`,
#'   `correlation`, `low_flag`, and `drop_flag`. Returns an empty data frame
#'   (with a message) if `STUDENT_ID` is absent.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' tbl <- compute_correlations(demo_data, assessment_spec)
#' }
compute_correlations <- function(student_results_long,
                                 assessment_spec,
                                 min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  empty_result <- data.frame(
    YEAR_1      = integer(0),
    YEAR_2      = integer(0),
    SUBJECT     = character(0),
    GRADE_1     = integer(0),
    GRADE_2     = integer(0),
    n           = integer(0),
    correlation = numeric(0),
    low_flag    = logical(0),
    drop_flag   = logical(0),
    stringsAsFactors = FALSE
  )

  if (!("STUDENT_ID" %in% names(student_results_long))) {
    message("STUDENT_ID required for correlation analysis -- skipping.")
    return(empty_result)
  }

  base <- student_results_long |>
    dplyr::select("STUDENT_ID", "YEAR", "SUBJECT", "GRADE", "SCALE_SCORE") |>
    dplyr::filter(
      !is.na(.data[["SCALE_SCORE"]]),
      !is.na(.data[["YEAR"]]),
      !is.na(.data[["GRADE"]])
    ) |>
    dplyr::mutate(
      YEAR  = as.integer(.data[["YEAR"]]),
      GRADE = as.integer(.data[["GRADE"]])
    )

  # year1: Grade G in year T
  # year2: Grade G+1 in year T+1
  # Join: same STUDENT_ID, same SUBJECT, GRADE_2 == GRADE_1 + 1, YEAR_2 == YEAR_1 + 1
  year1 <- base |>
    dplyr::rename(YEAR_1 = "YEAR", GRADE_1 = "GRADE", SCORE_1 = "SCALE_SCORE")
  year2 <- base |>
    dplyr::rename(YEAR_2 = "YEAR", GRADE_2 = "GRADE", SCORE_2 = "SCALE_SCORE")

  joined <- dplyr::inner_join(
    year1, year2,
    by     = c("STUDENT_ID", "SUBJECT"),
    suffix = c("", "")
  ) |>
    dplyr::filter(
      .data[["YEAR_2"]]  == .data[["YEAR_1"]]  + 1L,
      .data[["GRADE_2"]] == .data[["GRADE_1"]] + 1L
    )

  if (nrow(joined) == 0) return(empty_result)

  cor_tbl <- joined |>
    dplyr::group_by(
      .data[["YEAR_1"]], .data[["YEAR_2"]],
      .data[["SUBJECT"]],
      .data[["GRADE_1"]], .data[["GRADE_2"]]
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      correlation = dplyr::if_else(
        dplyr::n() >= min_n,
        stats::cor(.data[["SCORE_1"]], .data[["SCORE_2"]],
                   method = "spearman", use = "complete.obs"),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data[["SUBJECT"]], .data[["GRADE_1"]],
      .data[["YEAR_1"]], .data[["YEAR_2"]]
    )

  # Flags: group by SUBJECT x grade-pair to track change across year-pairs
  cor_tbl <- cor_tbl |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE_1"]], .data[["GRADE_2"]]) |>
    dplyr::mutate(
      prior_corr = dplyr::lag(.data[["correlation"]]),
      low_flag   = !is.na(.data[["correlation"]]) &
                   .data[["correlation"]] < 0.5,
      drop_flag  = dplyr::if_else(
        !is.na(.data[["correlation"]]) & !is.na(.data[["prior_corr"]]),
        (.data[["prior_corr"]] - .data[["correlation"]]) > 0.15,
        NA
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"prior_corr")

  cor_tbl
}


# Internal helper: pivot correlation table to wide display format -------------
# Rows: SUBJECT x grade_pair label
# Column groups: one per year-pair, each with Correlation + N sub-columns
.make_corr_wide <- function(correlation_table) {
  if (nrow(correlation_table) == 0 ||
      !("GRADE_1" %in% names(correlation_table))) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  tbl <- correlation_table |>
    dplyr::mutate(
      grade_pair = paste0("Grades ", .data[["GRADE_1"]],
                          "\u2192", .data[["GRADE_2"]]),
      year_pair  = paste0(.data[["YEAR_1"]], "\u2013", .data[["YEAR_2"]])
    )

  # Determine ordered year-pair levels
  yp_levels <- tbl |>
    dplyr::distinct(.data[["YEAR_1"]], .data[["year_pair"]]) |>
    dplyr::arrange(.data[["YEAR_1"]]) |>
    dplyr::pull("year_pair")

  # Determine ordered grade-pair levels
  gp_levels <- tbl |>
    dplyr::distinct(.data[["GRADE_1"]], .data[["grade_pair"]]) |>
    dplyr::arrange(.data[["GRADE_1"]]) |>
    dplyr::pull("grade_pair")

  wide <- tbl |>
    dplyr::select("SUBJECT", "grade_pair", "year_pair", "n", "correlation") |>
    tidyr::pivot_wider(
      names_from  = "year_pair",
      values_from = c("correlation", "n"),
      names_glue  = "{.value}__{year_pair}"
    )

  # Re-order columns: Subject, Grade Pair, then interleaved Corr / N per year-pair
  col_order <- c("SUBJECT", "grade_pair")
  for (yp in yp_levels) {
    col_order <- c(col_order,
                   paste0("correlation__", yp),
                   paste0("n__", yp))
  }
  col_order <- intersect(col_order, names(wide))

  wide <- wide |>
    dplyr::mutate(
      grade_pair = factor(.data[["grade_pair"]], levels = gp_levels)
    ) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["grade_pair"]]) |>
    dplyr::select(dplyr::all_of(col_order))

  attr(wide, "year_pair_levels") <- yp_levels
  wide
}


#' Plot cohort-style score correlations
#'
#' Returns a named list of two plots visualising year-to-year cohort Spearman
#' correlations by grade-pair within each subject.
#'
#' @param correlation_table Data frame returned by [compute_correlations()].
#' @param assessment_spec Named list describing the assessment program (used for
#'   subject labels).
#'
#' @return A named list with elements `$line` (line plot over time, one line per
#'   grade-pair) and `$heatmap` (correlation heatmap by grade-pair and year-pair),
#'   each a `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_correlations(demo_data, assessment_spec)
#' plots <- plot_correlations(tbl, assessment_spec)
#' plots$line
#' }
plot_correlations <- function(correlation_table, assessment_spec) {
  stopifnot(
    is.data.frame(correlation_table),
    is.list(assessment_spec)
  )

  # Build a lookup data frame for subject labels (avoids deprecated recode/!!!)
  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(
      names(assessment_spec$subjects),
      function(s) assessment_spec$subjects[[s]]$label,
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  empty_plot <- function(msg) {
    ggplot2::ggplot(data.frame(x = 0.5, y = 0.5, label = msg)) +
      ggplot2::geom_text(
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                     label = .data[["label"]]),
        size = 5, color = "grey40"
      ) +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::theme_void()
  }

  if (nrow(correlation_table) == 0 ||
      !("GRADE_1" %in% names(correlation_table))) {
    ep <- empty_plot("No correlation data available\n(persistent STUDENT_ID required)")
    return(list(line = ep, heatmap = ep))
  }

  tbl <- correlation_table |>
    dplyr::left_join(subj_lookup, by = "SUBJECT") |>
    dplyr::mutate(
      subj_label = dplyr::coalesce(.data[["subj_label"]], .data[["SUBJECT"]]),
      year_pair  = paste0(.data[["YEAR_1"]], "\u2013", .data[["YEAR_2"]]),
      grade_pair = paste0("Gr. ", .data[["GRADE_1"]],
                          "\u2192", .data[["GRADE_2"]]),
      point_type = dplyr::case_when(
        !is.na(.data[["low_flag"]])  & .data[["low_flag"]]  ~ "Low",
        !is.na(.data[["drop_flag"]]) & .data[["drop_flag"]] ~ "Drop",
        TRUE                                                 ~ "Normal"
      )
    ) |>
    dplyr::mutate(
      year_pair  = factor(
        .data[["year_pair"]],
        levels = unique(.data[["year_pair"]][order(.data[["YEAR_1"]])])
      ),
      grade_pair = factor(
        .data[["grade_pair"]],
        levels = unique(.data[["grade_pair"]][order(.data[["GRADE_1"]])])
      )
    )

  # --- Line plot ---
  line_plot <- ggplot2::ggplot(
    tbl,
    ggplot2::aes(
      x     = .data[["year_pair"]],
      y     = .data[["correlation"]],
      color = .data[["grade_pair"]],
      group = .data[["grade_pair"]]
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0.5, linetype = "dashed",
      color = "grey40", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = Inf, y = 0.5, label = "Low threshold (0.50)",
      hjust = 1.1, vjust = -0.4, size = 3, color = "grey40"
    ) +
    ggplot2::geom_line(alpha = 0.7, linewidth = 0.7) +
    ggplot2::geom_point(
      ggplot2::aes(
        size  = .data[["point_type"]],
        shape = .data[["point_type"]]
      )
    ) +
    ggplot2::facet_wrap(~ subj_label, ncol = 1, scales = "free_x") +
    ggplot2::scale_y_continuous(
      limits = c(NA, 1), breaks = seq(0, 1, 0.1)
    ) +
    ggplot2::scale_size_manual(
      values = c("Normal" = 2, "Low" = 4, "Drop" = 4), name = "Flag"
    ) +
    ggplot2::scale_shape_manual(
      values = c("Normal" = 16, "Low" = 25, "Drop" = 17), name = "Flag"
    ) +
    ggplot2::scale_color_viridis_d(
      name = "Grade pair", option = "D", end = 0.9
    ) +
    ggplot2::labs(
      title = "Cohort Spearman Correlations by Grade Pair",
      x     = "Year transition",
      y     = "Spearman correlation"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position  = "right"
    )

  # --- Heatmap ---
  heatmap_plot <- ggplot2::ggplot(
    tbl,
    ggplot2::aes(
      x    = .data[["year_pair"]],
      y    = .data[["grade_pair"]],
      fill = .data[["correlation"]]
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(
          is.na(.data[["correlation"]]),
          "NA",
          sprintf("%.2f", .data[["correlation"]])
        )
      ),
      size = 3
    ) +
    ggplot2::facet_wrap(~ subj_label, ncol = 1, scales = "free_x") +
    ggplot2::scale_fill_gradient2(
      low      = "#d73027",
      mid      = "#ffffbf",
      high     = "#1a9850",
      midpoint = 0.7,
      limits   = c(0, 1),
      na.value = "grey80",
      name     = "Correlation"
    ) +
    ggplot2::labs(
      title = "Cohort Correlation Heatmap by Grade Pair",
      x     = "Year transition",
      y     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  list(line = line_plot, heatmap = heatmap_plot)
}


#' Summarize cohort correlation findings
#'
#' Returns a narrative, by-subject summary, the full long-format correlation
#' table, and a wide-format display table suitable for rendering with
#' flextable.
#'
#' @param correlation_table Data frame returned by [compute_correlations()].
#'
#' @return A named list with:
#'   * `$table` — full long-format correlation table
#'   * `$wide_table` — wide-format data frame (Subject + Grade Pair rows,
#'     year-pair column groups with Correlation and N sub-columns)
#'   * `$by_subject` — median correlation and flagged cell counts by subject
#'   * `$narrative` — character vector of 1–3 sentences
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_correlations(demo_data, assessment_spec)
#' summarize_correlations(tbl)
#' }
summarize_correlations <- function(correlation_table) {
  stopifnot(is.data.frame(correlation_table))

  empty_by_subject <- data.frame(
    SUBJECT     = character(0),
    median_corr = numeric(0),
    n_low_flag  = integer(0),
    n_drop_flag = integer(0),
    stringsAsFactors = FALSE
  )

  if (nrow(correlation_table) == 0 ||
      !("correlation" %in% names(correlation_table))) {
    return(list(
      table      = correlation_table,
      wide_table = data.frame(stringsAsFactors = FALSE),
      by_subject = empty_by_subject,
      narrative  = "No correlation data were available (STUDENT_ID required)."
    ))
  }

  wide_table <- .make_corr_wide(correlation_table)

  has_corr <- correlation_table |>
    dplyr::filter(!is.na(.data[["correlation"]]))

  if (nrow(has_corr) == 0) {
    return(list(
      table      = correlation_table,
      wide_table = wide_table,
      by_subject = empty_by_subject,
      narrative  = "No correlations could be computed (insufficient matched pairs)."
    ))
  }

  by_subject <- has_corr |>
    dplyr::group_by(.data[["SUBJECT"]]) |>
    dplyr::summarise(
      median_corr = stats::median(.data[["correlation"]], na.rm = TRUE),
      n_low_flag  = sum(.data[["low_flag"]],  na.rm = TRUE),
      n_drop_flag = sum(.data[["drop_flag"]], na.rm = TRUE),
      .groups     = "drop"
    )

  corr_vals <- has_corr$correlation
  corr_min  <- round(min(corr_vals,              na.rm = TRUE), 3)
  corr_max  <- round(max(corr_vals,              na.rm = TRUE), 3)
  corr_med  <- round(stats::median(corr_vals,    na.rm = TRUE), 3)

  n_low  <- sum(correlation_table$low_flag,  na.rm = TRUE)
  n_drop <- sum(correlation_table$drop_flag, na.rm = TRUE)

  sent1 <- sprintf(
    "Across all subject, grade-pair, and year-pair combinations, cohort Spearman correlations ranged from %.3f to %.3f (median %.3f).",
    corr_min, corr_max, corr_med
  )

  if (n_low == 0 && n_drop == 0) {
    sent2 <- "All correlations were at or above 0.50 and no grade-pair showed a drop greater than 0.15 across consecutive year-pairs, indicating stable cohort performance stability."
    return(list(
      table      = correlation_table,
      wide_table = wide_table,
      by_subject = by_subject,
      narrative  = c(sent1, sent2)
    ))
  }

  sent2 <- if (n_low > 0) {
    low_rows <- correlation_table |>
      dplyr::filter(!is.na(.data[["low_flag"]]) & .data[["low_flag"]]) |>
      dplyr::mutate(
        cell_lbl = paste0(
          .data[["SUBJECT"]], " Gr.", .data[["GRADE_1"]],
          "\u2192", .data[["GRADE_2"]],
          " (", .data[["YEAR_1"]], "\u2013", .data[["YEAR_2"]], ")",
          " r=", round(.data[["correlation"]], 3)
        )
      )
    sprintf(
      "%d grade-pair combination%s had unusually low correlations (below 0.50): %s.",
      n_low, if (n_low == 1L) "" else "s",
      paste(low_rows$cell_lbl, collapse = "; ")
    )
  } else {
    "No grade-pair combinations had correlations below 0.50."
  }

  sent3 <- if (n_drop > 0) {
    drop_rows <- correlation_table |>
      dplyr::filter(!is.na(.data[["drop_flag"]]) & .data[["drop_flag"]]) |>
      dplyr::mutate(
        cell_lbl = paste0(
          .data[["SUBJECT"]], " Gr.", .data[["GRADE_1"]],
          "\u2192", .data[["GRADE_2"]],
          " (", .data[["YEAR_1"]], "\u2013", .data[["YEAR_2"]], ")"
        )
      )
    sprintf(
      "%d combination%s showed a drop greater than 0.15 from the prior year-pair: %s.",
      n_drop, if (n_drop == 1L) "" else "s",
      paste(drop_rows$cell_lbl, collapse = "; ")
    )
  } else {
    "No grade-pair combinations showed a drop greater than 0.15 from the prior year-pair."
  }

  list(
    table      = correlation_table,
    wide_table = wide_table,
    by_subject = by_subject,
    narrative  = c(sent1, sent2, sent3)
  )
}


#' Run the correlation sub-analysis and return the results sub-list
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum number of matched student pairs required to
#'   report a correlation. Default `10`.
#'
#' @return A named list with `$table` (long format), `$plot` (list of `$line`
#'   and `$heatmap`), and `$text` (list of `$table`, `$wide_table`,
#'   `$by_subject`, `$narrative`).
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' sub <- run_correlations(demo_data, assessment_spec)
#' }
run_correlations <- function(student_results_long,
                             assessment_spec,
                             min_n = 10) {
  tbl <- compute_correlations(student_results_long, assessment_spec,
                               min_n = min_n)
  list(
    table = tbl,
    plot  = plot_correlations(tbl, assessment_spec),
    text  = summarize_correlations(tbl)
  )
}
