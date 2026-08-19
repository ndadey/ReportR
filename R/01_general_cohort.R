# ==============================================================================
# Line 1.3 — General Cohort Analysis: Cohort Composition and Performance
#
# Package-level helpers (.derive_cohort, .theme_cohort, .build_*) are defined
# first so their environments never capture student_results_long — per the
# critical ggplot memory rule in CLAUDE.md §11.
#
# Functions (exported unless prefixed with .):
#   .derive_cohort()
#   .theme_cohort()
#   .build_cohort_score_plot()
#   .build_cohort_proficiency_plot()
#   .build_cohort_dist_plot()
#   .build_sankey_plot()
#   .build_demographic_composition_plot()
#   .build_roc_plot()
#   .build_correlation_plot()
#   .build_mobility_dist_plot()
#   .build_mobility_achievement_plot()
#   compute_cohort_summary()
#   compute_cohort_demographics()
#   compute_cohort_transitions()
#   compute_cohort_rate_of_change()
#   compute_cohort_correlations()
#   compute_cohort_vs_crosssectional()
#   compute_cohort_mobility()
#   compute_mobility_by_achievement()
#   compute_cohort_persistence()
#   compute_enrollment_diagnostics()
#   plot_cohort_mean_scores()
#   plot_cohort_proficiency_rates()
#   plot_cohort_score_distributions()
#   plot_sankey_transitions()
#   plot_cohort_demographic_composition()
#   plot_rate_of_change()
#   plot_score_correlations()
#   plot_mobility_distribution()
#   plot_mobility_by_achievement()
#   summarize_cohort_analysis()
#   run_cohort_analysis()
# ==============================================================================

#' @importFrom rlang .data :=
#' @importFrom dplyr filter mutate select group_by summarize left_join arrange
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_col
#'   geom_area facet_wrap facet_grid labs theme scale_fill_manual
#'   geom_hline geom_blank coord_cartesian geom_errorbar geom_crossbar
NULL

# ==============================================================================
# Package-level non-exported helpers
# ==============================================================================

# Shared ggplot2 theme for all cohort plots
.theme_cohort <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor      = ggplot2::element_blank(),
      legend.position       = "bottom",
      legend.box.background = ggplot2::element_blank(),
      strip.text            = ggplot2::element_text(face = "bold")
    )
}

# Add COHORT_YEAR = YEAR - (GRADE - cohort_anchor_grade); NA for EOC records
.derive_cohort <- function(dat, assessment_spec) {
  anchor <- as.integer(assessment_spec$years$cohort_anchor_grade)
  dat |>
    dplyr::mutate(
      COHORT_YEAR = dplyr::if_else(
        is.na(.data[["GRADE"]]),
        NA_integer_,
        as.integer(.data[["YEAR"]]) - (as.integer(.data[["GRADE"]]) - anchor)
      )
    )
}

# Mobility palette (consistent across mobility plots)
.mobility_palette <- c(
  "Entering"   = "#2ca02c",
  "Continuing" = "#aec7e8",
  "Exiting"    = "#d62728"
)

# ==============================================================================
# Package-level ggplot builders (.build_*) — defined outside compute/plot
# functions so their environments never hold student_results_long
# ==============================================================================

.build_cohort_score_plot <- function(dat, subj_label, y_lo, y_hi, cs_df = NULL) {
  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data[["GRADE_CHR"]], y = .data[["MEAN_SCORE"]],
                 color = .data[["COHORT_YR_CHR"]], group = .data[["COHORT_YR_CHR"]])
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = subj_label, x = "Grade", y = "Mean Scale Score",
                  color = "Cohort Year") +
    .theme_cohort()

  if (!is.null(cs_df) && nrow(cs_df) > 0L) {
    p <- p + ggplot2::geom_line(
      data  = cs_df,
      ggplot2::aes(x = .data[["GRADE_CHR"]], y = .data[["MEAN_SCORE"]],
                   group = 1L),
      color = "black", linetype = "dashed", linewidth = 0.8,
      inherit.aes = FALSE
    )
  }

  if (!is.na(y_lo) && !is.na(y_hi)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(y_lo, y_hi))
  }
  p
}

.build_cohort_proficiency_plot <- function(dat, subj_label) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data[["GRADE_CHR"]], y = .data[["PCT_PROFICIENT"]],
                 color = .data[["COHORT_YR_CHR"]], group = .data[["COHORT_YR_CHR"]])
  ) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                labels = function(x) paste0(x, "%")) +
    ggplot2::labs(title = subj_label, x = "Grade",
                  y = "% Proficient or Above", color = "Cohort Year") +
    .theme_cohort()
}

.build_cohort_dist_plot <- function(dat, subj_label) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data[["COHORT_YR_CHR"]], group = .data[["COHORT_YR_CHR"]])
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["IQR_LO"]], ymax = .data[["IQR_HI"]]),
      width = 0.25, color = "grey50"
    ) +
    ggplot2::geom_crossbar(
      ggplot2::aes(y = .data[["MEDIAN_SCORE"]],
                   ymin = .data[["Q1_SCORE"]], ymax = .data[["Q3_SCORE"]]),
      fill = "#aec7e8", color = "steelblue", width = 0.4
    ) +
    ggplot2::facet_wrap(~ GRADE_CHR, scales = "free_y") +
    ggplot2::labs(title = subj_label, x = "Cohort Year", y = "Scale Score") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    .theme_cohort()
}

.build_sankey_plot <- function(dat, subj_label, level_palette) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(y     = .data[["N"]],
                 axis1 = .data[["FROM_LEVEL"]],
                 axis2 = .data[["TO_LEVEL"]])
  ) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = .data[["TO_LEVEL"]]),
      width = 0.3, alpha = 0.75, knot.pos = 0.4
    ) +
    ggalluvial::geom_stratum(width = 0.3, fill = "grey85", color = "grey40") +
    ggplot2::geom_text(
      stat = ggalluvial::StatStratum,
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      size = 2.8
    ) +
    ggplot2::scale_fill_manual(values = level_palette, name = "To Level") +
    ggplot2::scale_x_discrete(
      limits = c("From Grade", "To Grade"), expand = c(0.1, 0.1)
    ) +
    ggplot2::facet_wrap(~ COHORT_YR_CHR) +
    ggplot2::labs(title = subj_label, y = "Students") +
    .theme_cohort()
}

.build_demographic_composition_plot <- function(dat, subj_label) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x    = .data[["COHORT_YR_CHR"]],
                 y    = .data[["PCT"]],
                 fill = .data[["GROUP"]],
                 group = .data[["GROUP"]])
  ) +
    ggplot2::geom_area(position = "stack", alpha = 0.8) +
    ggplot2::facet_wrap(~ DEMOGRAPHIC, scales = "free_y") +
    ggplot2::labs(title = subj_label, x = "Cohort Year",
                  y = "Percent", fill = "Group") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    .theme_cohort()
}

.build_roc_plot <- function(dat, subj_label) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x    = .data[["COHORT_YR_CHR"]],
                 y    = .data[["PCT"]],
                 fill = .data[["TRAJECTORY"]])
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_manual(
      values = c("Improving" = "#2ca02c", "Stable" = "#aec7e8",
                 "Declining" = "#d62728"),
      name = "Trajectory"
    ) +
    ggplot2::facet_grid(.data[["FROM_GRADE"]] ~ .data[["TO_GRADE"]],
                        labeller = ggplot2::label_both) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    ggplot2::labs(title = subj_label, x = "Cohort Year", y = "Percent") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    .theme_cohort()
}

.build_correlation_plot <- function(dat, subj_label) {
  ann_df <- dat |>
    dplyr::distinct(.data[["COHORT_YR_CHR"]], .data[["GRADE_X"]],
                    .data[["GRADE_Y"]], .data[["R"]], .data[["N"]]) |>
    dplyr::mutate(
      ann = sprintf("r = %.2f\nn = %d", .data[["R"]], .data[["N"]]),
      panel_key = paste0(.data[["COHORT_YR_CHR"]], "_",
                         .data[["GRADE_X"]], "_", .data[["GRADE_Y"]])
    )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data[["R"]], y = .data[["N"]],
                 label = .data[["COHORT_YR_CHR"]])
  ) +
    ggplot2::geom_point(size = 2.5, color = "steelblue") +
    ggplot2::geom_text(size = 2.5, vjust = -0.6, show.legend = FALSE) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::facet_wrap(~ paste0("G", GRADE_X, "→G", GRADE_Y)) +
    ggplot2::labs(title = subj_label,
                  x = "Pearson r (adjacent-grade scale scores)", y = "N") +
    .theme_cohort()
}

.build_mobility_dist_plot <- function(dat, subj_label) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x    = .data[["GRADE_CHR"]],
                 y    = .data[["PCT"]],
                 fill = .data[["MOBILITY_STATUS"]])
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_manual(
      values = .mobility_palette,
      breaks = c("Entering", "Continuing", "Exiting"),
      name   = "Status"
    ) +
    ggplot2::facet_wrap(~ COHORT_YR_CHR) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    ggplot2::labs(title = subj_label, x = "Grade", y = "Percent") +
    .theme_cohort()
}

.build_mobility_achievement_plot <- function(dat, subj_label) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x    = .data[["ACHIEVEMENT_LEVEL"]],
                 y    = .data[["PCT_WITHIN_MOBILITY"]],
                 fill = .data[["MOBILITY_STATUS"]])
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(
      values = .mobility_palette,
      breaks = c("Entering", "Continuing", "Exiting"),
      name   = "Status"
    ) +
    ggplot2::facet_grid(.data[["FROM_GRADE"]] ~ .data[["TO_GRADE"]],
                        labeller = ggplot2::label_both) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    ggplot2::labs(title = subj_label, x = "Achievement Level",
                  y = "% Within Mobility Group") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    .theme_cohort()
}

# Internal: match students across adjacent grades within the same COHORT_YEAR.
# Returns rows: STUDENT_ID, SUBJECT, COHORT_YEAR, FROM_GRADE, TO_GRADE,
# plus any extra columns renamed _FROM / _TO.
.match_adjacent_cohort <- function(cohort_df, extra_from = character(0),
                                    extra_to = character(0)) {
  grades <- sort(unique(cohort_df[["GRADE"]]))
  if (length(grades) < 2L) return(data.frame(stringsAsFactors = FALSE))

  pairs <- lapply(seq_len(length(grades) - 1L), function(i) {
    g1 <- grades[i]
    g2 <- grades[i + 1L]

    sel_from <- unique(c("STUDENT_ID", "SUBJECT", "COHORT_YEAR", "GRADE",
                         extra_from))
    sel_to   <- unique(c("STUDENT_ID", "SUBJECT", "COHORT_YEAR", "GRADE",
                         extra_to))

    d1 <- cohort_df[cohort_df[["GRADE"]] == g1,
                    intersect(sel_from, names(cohort_df)),
                    drop = FALSE]
    d2 <- cohort_df[cohort_df[["GRADE"]] == g2,
                    intersect(sel_to, names(cohort_df)),
                    drop = FALSE]

    names(d1)[names(d1) == "GRADE"] <- "FROM_GRADE"
    names(d2)[names(d2) == "GRADE"] <- "TO_GRADE"
    for (col in extra_from) if (col %in% names(d1))
      names(d1)[names(d1) == col] <- paste0(col, "_FROM")
    for (col in extra_to) if (col %in% names(d2))
      names(d2)[names(d2) == col] <- paste0(col, "_TO")

    dplyr::inner_join(d1, d2, by = c("STUDENT_ID", "SUBJECT", "COHORT_YEAR"))
  })
  dplyr::bind_rows(pairs)
}


# ==============================================================================
# compute_cohort_summary()
# ==============================================================================

#' Compute cohort-level scale score and proficiency summaries
#'
#' Follows each cohort (defined by `COHORT_YEAR`) through grades, computing
#' mean scores, standard deviations, proficiency rates, and per-level counts.
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Cells with fewer students have numeric columns set to
#'   `NA`. Default `10`.
#'
#' @return A data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE`, `YEAR`, `N`,
#'   `MEAN_SCORE`, `SD_SCORE`, `MEDIAN_SCORE`, `Q1_SCORE`, `Q3_SCORE`,
#'   `PCT_PROFICIENT`, plus one `N_<Level>` column per achievement level.
#' @export
compute_cohort_summary <- function(student_results_long, assessment_spec,
                                   min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT",
          "SCALE_SCORE", "ACHIEVEMENT_LEVEL") %in%
          names(student_results_long)),
    !"COHORT_YEAR" %in% names(student_results_long)
  )

  labels     <- assessment_spec$achievement_levels$labels
  bench_idx  <- benchmark_index(assessment_spec)
  prof_labels <- labels[seq(bench_idx, length(labels))]

  dat <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["ACHIEVEMENT_LEVEL"]])) |>
    .derive_cohort(assessment_spec)

  # Core statistics per SUBJECT × COHORT_YEAR × GRADE × YEAR
  stats <- dat |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                    .data[["GRADE"]], .data[["YEAR"]]) |>
    dplyr::summarize(
      N            = dplyr::n(),
      MEAN_SCORE   = mean(.data[["SCALE_SCORE"]], na.rm = TRUE),
      SD_SCORE     = stats::sd(.data[["SCALE_SCORE"]], na.rm = TRUE),
      MEDIAN_SCORE = stats::median(.data[["SCALE_SCORE"]], na.rm = TRUE),
      Q1_SCORE     = stats::quantile(.data[["SCALE_SCORE"]], 0.25, na.rm = TRUE,
                                     names = FALSE),
      Q3_SCORE     = stats::quantile(.data[["SCALE_SCORE"]], 0.75, na.rm = TRUE,
                                     names = FALSE),
      n_prof       = sum(.data[["ACHIEVEMENT_LEVEL"]] %in% prof_labels,
                         na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      PCT_PROFICIENT = dplyr::if_else(
        .data[["N"]] >= min_n,
        100 * .data[["n_prof"]] / .data[["N"]],
        NA_real_
      )
    ) |>
    dplyr::select(-"n_prof")

  # Per-level counts via pivot
  level_counts <- dat |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                    .data[["GRADE"]], .data[["YEAR"]],
                    .data[["ACHIEVEMENT_LEVEL"]]) |>
    dplyr::summarize(n_level = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from  = "ACHIEVEMENT_LEVEL",
      values_from = "n_level",
      values_fill = 0L,
      names_prefix = "N_"
    )

  # Ensure all label columns exist (labels not in data get NA)
  for (lbl in paste0("N_", labels)) {
    if (!lbl %in% names(level_counts)) level_counts[[lbl]] <- NA_integer_
  }

  result <- stats |>
    dplyr::left_join(
      level_counts[, c("SUBJECT", "COHORT_YEAR", "GRADE", "YEAR",
                       paste0("N_", labels))],
      by = c("SUBJECT", "COHORT_YEAR", "GRADE", "YEAR")
    ) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                   .data[["GRADE"]])

  # Small-n suppression — base-R row indexing per CLAUDE.md §7
  suppress_cols <- c("MEAN_SCORE", "SD_SCORE", "MEDIAN_SCORE",
                     "Q1_SCORE", "Q3_SCORE", "PCT_PROFICIENT",
                     paste0("N_", labels))
  small <- !is.na(result[["N"]]) & result[["N"]] < min_n
  result[small, intersect(suppress_cols, names(result))] <- NA_real_

  result
}


# ==============================================================================
# compute_cohort_demographics()
# ==============================================================================

#' Compute demographic composition of cohorts by grade
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Cells below this size have `PCT` set to `NA`. Default
#'   `10`.
#'
#' @return A long data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE`, `YEAR`,
#'   `DEMOGRAPHIC`, `GROUP`, `N`, `PCT`.
#' @export
compute_cohort_demographics <- function(student_results_long, assessment_spec,
                                        min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("YEAR", "GRADE", "SUBJECT") %in% names(student_results_long))
  )

  demo_names <- names(assessment_spec$demographics)
  if (length(demo_names) == 0L) {
    return(data.frame(SUBJECT = character(0), COHORT_YEAR = integer(0),
                      GRADE = integer(0), YEAR = integer(0),
                      DEMOGRAPHIC = character(0), GROUP = character(0),
                      N = integer(0), PCT = numeric(0),
                      stringsAsFactors = FALSE))
  }

  dat <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]])) |>
    .derive_cohort(assessment_spec)

  rows <- lapply(demo_names, function(dv) {
    if (!dv %in% names(dat)) return(NULL)
    d_type <- assessment_spec$demographics[[dv]]$type

    cell <- dat |>
      dplyr::filter(!is.na(.data[[dv]])) |>
      dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                      .data[["GRADE"]], .data[["YEAR"]],
                      GROUP = .data[[dv]]) |>
      dplyr::summarize(N = dplyr::n(), .groups = "drop")

    cell_total <- cell |>
      dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                      .data[["GRADE"]], .data[["YEAR"]]) |>
      dplyr::mutate(N_TOTAL = sum(.data[["N"]])) |>
      dplyr::ungroup()

    cell_total |>
      dplyr::mutate(
        DEMOGRAPHIC = dv,
        GROUP       = as.character(.data[["GROUP"]]),
        PCT         = dplyr::if_else(
          .data[["N_TOTAL"]] >= min_n,
          100 * .data[["N"]] / .data[["N_TOTAL"]],
          NA_real_
        )
      ) |>
      dplyr::select("SUBJECT", "COHORT_YEAR", "GRADE", "YEAR",
                    "DEMOGRAPHIC", "GROUP", "N", "PCT")
  })

  dplyr::bind_rows(rows) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                   .data[["GRADE"]], .data[["DEMOGRAPHIC"]])
}


# ==============================================================================
# compute_cohort_transitions()
# ==============================================================================

#' Compute achievement level transition rates across adjacent grades within cohort
#'
#' @param student_results_long A long-format student results data frame. Must
#'   contain `STUDENT_ID`.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#'
#' @return A data frame: `SUBJECT`, `COHORT_YEAR`, `FROM_GRADE`, `TO_GRADE`,
#'   `FROM_LEVEL`, `TO_LEVEL`, `N`, `PCT_OF_FROM`. Returns 0 rows when no
#'   persistent student IDs are available.
#' @export
compute_cohort_transitions <- function(student_results_long, assessment_spec,
                                       min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT",
          "ACHIEVEMENT_LEVEL") %in% names(student_results_long))
  )

  labels <- assessment_spec$achievement_levels$labels
  empty  <- data.frame(SUBJECT = character(0), COHORT_YEAR = integer(0),
                       FROM_GRADE = integer(0), TO_GRADE = integer(0),
                       FROM_LEVEL = character(0), TO_LEVEL = character(0),
                       N = integer(0), PCT_OF_FROM = numeric(0),
                       stringsAsFactors = FALSE)

  base <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["STUDENT_ID"]]),
                  !is.na(.data[["ACHIEVEMENT_LEVEL"]])) |>
    .derive_cohort(assessment_spec) |>
    dplyr::select("STUDENT_ID", "SUBJECT", "COHORT_YEAR",
                  "GRADE", "ACHIEVEMENT_LEVEL")

  if (nrow(base) == 0L || length(unique(base[["STUDENT_ID"]])) <= 1L)
    return(empty)

  matched <- .match_adjacent_cohort(base, "ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL")
  if (nrow(matched) == 0L) return(empty)

  result <- matched |>
    dplyr::rename(FROM_LEVEL = "ACHIEVEMENT_LEVEL_FROM",
                  TO_LEVEL   = "ACHIEVEMENT_LEVEL_TO") |>
    dplyr::mutate(
      FROM_LEVEL = factor(.data[["FROM_LEVEL"]], levels = labels, ordered = TRUE),
      TO_LEVEL   = factor(.data[["TO_LEVEL"]],   levels = labels, ordered = TRUE)
    ) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                    .data[["FROM_GRADE"]], .data[["TO_GRADE"]],
                    .data[["FROM_LEVEL"]], .data[["TO_LEVEL"]]) |>
    dplyr::summarize(N = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                    .data[["FROM_GRADE"]], .data[["TO_GRADE"]],
                    .data[["FROM_LEVEL"]]) |>
    dplyr::mutate(
      n_from     = sum(.data[["N"]]),
      PCT_OF_FROM = dplyr::if_else(
        .data[["n_from"]] >= min_n,
        100 * .data[["N"]] / .data[["n_from"]],
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select("SUBJECT", "COHORT_YEAR", "FROM_GRADE", "TO_GRADE",
                  "FROM_LEVEL", "TO_LEVEL", "N", "PCT_OF_FROM") |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                   .data[["FROM_GRADE"]])

  result
}


# ==============================================================================
# compute_cohort_rate_of_change()
# ==============================================================================

#' Compute rates of improving, stable, and declining trajectories across grades
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#'
#' @return A data frame: `SUBJECT`, `COHORT_YEAR`, `FROM_GRADE`, `TO_GRADE`,
#'   `TRAJECTORY`, `N`, `PCT`. Returns 0 rows when no persistent IDs exist.
#' @export
compute_cohort_rate_of_change <- function(student_results_long, assessment_spec,
                                           min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT",
          "ACHIEVEMENT_LEVEL") %in% names(student_results_long))
  )

  labels <- assessment_spec$achievement_levels$labels
  empty  <- data.frame(SUBJECT = character(0), COHORT_YEAR = integer(0),
                       FROM_GRADE = integer(0), TO_GRADE = integer(0),
                       TRAJECTORY = character(0), N = integer(0),
                       PCT = numeric(0), stringsAsFactors = FALSE)

  base <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["STUDENT_ID"]]),
                  !is.na(.data[["ACHIEVEMENT_LEVEL"]])) |>
    .derive_cohort(assessment_spec) |>
    dplyr::select("STUDENT_ID", "SUBJECT", "COHORT_YEAR",
                  "GRADE", "ACHIEVEMENT_LEVEL")

  if (nrow(base) == 0L || length(unique(base[["STUDENT_ID"]])) <= 1L)
    return(empty)

  matched <- .match_adjacent_cohort(base, "ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL")
  if (nrow(matched) == 0L) return(empty)

  matched |>
    dplyr::mutate(
      lvl_from   = match(.data[["ACHIEVEMENT_LEVEL_FROM"]], labels),
      lvl_to     = match(.data[["ACHIEVEMENT_LEVEL_TO"]],   labels),
      TRAJECTORY = dplyr::case_when(
        .data[["lvl_to"]] > .data[["lvl_from"]] ~ "Improving",
        .data[["lvl_to"]] < .data[["lvl_from"]] ~ "Declining",
        TRUE ~ "Stable"
      )
    ) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                    .data[["FROM_GRADE"]], .data[["TO_GRADE"]],
                    .data[["TRAJECTORY"]]) |>
    dplyr::summarize(N = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                    .data[["FROM_GRADE"]], .data[["TO_GRADE"]]) |>
    dplyr::mutate(
      n_total = sum(.data[["N"]]),
      PCT     = dplyr::if_else(
        .data[["n_total"]] >= min_n,
        100 * .data[["N"]] / .data[["n_total"]],
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select("SUBJECT", "COHORT_YEAR", "FROM_GRADE", "TO_GRADE",
                  "TRAJECTORY", "N", "PCT") |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                   .data[["FROM_GRADE"]])
}


# ==============================================================================
# compute_cohort_correlations()
# ==============================================================================

#' Compute Pearson correlations of scale scores across adjacent grades within cohort
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#'
#' @return A data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE_X`, `GRADE_Y`,
#'   `YEAR_X`, `YEAR_Y`, `R`, `N`, `P_VALUE`. Returns 0 rows when no
#'   persistent IDs exist.
#' @export
compute_cohort_correlations <- function(student_results_long, assessment_spec,
                                         min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT",
          "SCALE_SCORE") %in% names(student_results_long))
  )

  anchor <- as.integer(assessment_spec$years$cohort_anchor_grade)
  empty  <- data.frame(SUBJECT = character(0), COHORT_YEAR = integer(0),
                       GRADE_X = integer(0), GRADE_Y = integer(0),
                       YEAR_X  = integer(0), YEAR_Y  = integer(0),
                       R = numeric(0), N = integer(0), P_VALUE = numeric(0),
                       stringsAsFactors = FALSE)

  base <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["STUDENT_ID"]]),
                  !is.na(.data[["SCALE_SCORE"]])) |>
    .derive_cohort(assessment_spec) |>
    dplyr::select("STUDENT_ID", "SUBJECT", "COHORT_YEAR",
                  "GRADE", "YEAR", "SCALE_SCORE")

  if (nrow(base) == 0L || length(unique(base[["STUDENT_ID"]])) <= 1L)
    return(empty)

  matched <- .match_adjacent_cohort(base,
                                     c("YEAR", "SCALE_SCORE"),
                                     c("YEAR", "SCALE_SCORE"))
  if (nrow(matched) == 0L) return(empty)

  matched <- matched |>
    dplyr::rename(GRADE_X   = "FROM_GRADE", GRADE_Y   = "TO_GRADE",
                  YEAR_X    = "YEAR_FROM",  YEAR_Y    = "YEAR_TO",
                  SCORE_X   = "SCALE_SCORE_FROM",
                  SCORE_Y   = "SCALE_SCORE_TO")

  subjects <- unique(matched[["SUBJECT"]])
  rows <- lapply(subjects, function(s) {
    d_s <- matched[matched[["SUBJECT"]] == s, ]
    grade_pairs <- unique(d_s[, c("COHORT_YEAR", "GRADE_X", "GRADE_Y",
                                   "YEAR_X", "YEAR_Y")])
    lapply(seq_len(nrow(grade_pairs)), function(j) {
      gp <- grade_pairs[j, ]
      d_cell <- d_s[
        d_s[["COHORT_YEAR"]] == gp[["COHORT_YEAR"]] &
        d_s[["GRADE_X"]]     == gp[["GRADE_X"]] &
        d_s[["GRADE_Y"]]     == gp[["GRADE_Y"]], ]
      n <- nrow(d_cell)
      if (n < min_n) return(NULL)
      ct <- tryCatch(
        stats::cor.test(d_cell[["SCORE_X"]], d_cell[["SCORE_Y"]],
                        method = "pearson"),
        error = function(e) NULL
      )
      if (is.null(ct)) return(NULL)
      data.frame(
        SUBJECT     = s,
        COHORT_YEAR = gp[["COHORT_YEAR"]],
        GRADE_X     = gp[["GRADE_X"]],
        GRADE_Y     = gp[["GRADE_Y"]],
        YEAR_X      = gp[["YEAR_X"]],
        YEAR_Y      = gp[["YEAR_Y"]],
        R           = as.numeric(ct$estimate),
        N           = n,
        P_VALUE     = ct$p.value,
        stringsAsFactors = FALSE
      )
    })
  })
  out <- dplyr::bind_rows(unlist(rows, recursive = FALSE))
  if (nrow(out) == 0L) return(empty)
  out |> dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                         .data[["GRADE_X"]])
}


# ==============================================================================
# compute_cohort_vs_crosssectional()
# ==============================================================================

#' Compare cohort trajectories to cross-sectional grade snapshots
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#'
#' @return A named list:
#'   * `$cohort` — data frame from [compute_cohort_summary()]
#'   * `$crosssectional` — data frame: `SUBJECT`, `GRADE`, `YEAR`, `N`,
#'     `MEAN_SCORE`, `PCT_PROFICIENT`
#' @export
compute_cohort_vs_crosssectional <- function(student_results_long, assessment_spec,
                                              min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  bench_idx   <- benchmark_index(assessment_spec)
  prof_labels <- assessment_spec$achievement_levels$labels[
    seq(bench_idx, length(assessment_spec$achievement_levels$labels))
  ]

  cohort_df <- compute_cohort_summary(student_results_long, assessment_spec,
                                       min_n = min_n)

  xs <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["ACHIEVEMENT_LEVEL"]])) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]]) |>
    dplyr::summarize(
      N              = dplyr::n(),
      MEAN_SCORE     = mean(.data[["SCALE_SCORE"]], na.rm = TRUE),
      n_prof         = sum(.data[["ACHIEVEMENT_LEVEL"]] %in% prof_labels,
                           na.rm = TRUE),
      .groups        = "drop"
    ) |>
    dplyr::mutate(
      PCT_PROFICIENT = dplyr::if_else(
        .data[["N"]] >= min_n,
        100 * .data[["n_prof"]] / .data[["N"]],
        NA_real_
      )
    ) |>
    dplyr::select(-"n_prof")

  list(cohort = cohort_df, crosssectional = xs)
}


# ==============================================================================
# compute_cohort_mobility()
# ==============================================================================

#' Classify students as Entering, Continuing, or Exiting between adjacent grades
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#'
#' @return A data frame: `SUBJECT`, `COHORT_YEAR`, `FROM_GRADE`, `TO_GRADE`,
#'   `YEAR`, `MOBILITY_STATUS`, `N`, `PCT`. Returns 0 rows when no persistent
#'   student IDs are present.
#' @export
compute_cohort_mobility <- function(student_results_long, assessment_spec,
                                    min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT") %in%
          names(student_results_long))
  )

  mob_levels <- c("Entering", "Continuing", "Exiting")
  empty <- data.frame(SUBJECT = character(0), COHORT_YEAR = integer(0),
                      FROM_GRADE = integer(0), TO_GRADE = integer(0),
                      YEAR = integer(0), MOBILITY_STATUS = character(0),
                      N = integer(0), PCT = numeric(0),
                      stringsAsFactors = FALSE)

  base <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["STUDENT_ID"]])) |>
    .derive_cohort(assessment_spec) |>
    dplyr::select("STUDENT_ID", "SUBJECT", "COHORT_YEAR", "GRADE", "YEAR")

  if (nrow(base) == 0L || length(unique(base[["STUDENT_ID"]])) <= 1L)
    return(empty)

  grades   <- sort(unique(base[["GRADE"]]))
  subjects <- unique(base[["SUBJECT"]])
  anchor   <- as.integer(assessment_spec$years$cohort_anchor_grade)

  rows <- lapply(seq_len(length(grades) - 1L), function(i) {
    g1 <- grades[i]
    g2 <- grades[i + 1L]

    lapply(subjects, function(s) {
      cohort_yrs <- unique(base[["COHORT_YEAR"]][base[["SUBJECT"]] == s &
                                                  !is.na(base[["COHORT_YEAR"]])])
      lapply(cohort_yrs, function(cy) {
        yr1 <- cy + (g1 - anchor)
        yr2 <- cy + (g2 - anchor)

        ids1 <- base[["STUDENT_ID"]][
          base[["SUBJECT"]] == s & base[["GRADE"]] == g1 &
          base[["YEAR"]] == yr1 & !is.na(base[["COHORT_YEAR"]]) &
          base[["COHORT_YEAR"]] == cy]
        ids2 <- base[["STUDENT_ID"]][
          base[["SUBJECT"]] == s & base[["GRADE"]] == g2 &
          base[["YEAR"]] == yr2 & !is.na(base[["COHORT_YEAR"]]) &
          base[["COHORT_YEAR"]] == cy]

        if (length(ids1) == 0L && length(ids2) == 0L) return(NULL)

        n_cont  <- length(intersect(ids1, ids2))
        n_enter <- length(setdiff(ids2, ids1))
        n_exit  <- length(setdiff(ids1, ids2))
        n_tot   <- n_cont + n_enter + n_exit

        data.frame(
          SUBJECT        = s,
          COHORT_YEAR    = cy,
          FROM_GRADE     = g1,
          TO_GRADE       = g2,
          YEAR           = yr2,
          MOBILITY_STATUS = mob_levels,
          N              = c(n_enter, n_cont, n_exit),
          N_TOTAL        = n_tot,
          stringsAsFactors = FALSE
        )
      })
    })
  })

  result <- dplyr::bind_rows(unlist(rows, recursive = FALSE))
  if (nrow(result) == 0L) return(empty)

  result |>
    dplyr::mutate(
      PCT = dplyr::if_else(
        .data[["N_TOTAL"]] >= min_n,
        100 * .data[["N"]] / .data[["N_TOTAL"]],
        NA_real_
      ),
      MOBILITY_STATUS = factor(.data[["MOBILITY_STATUS"]],
                                levels = mob_levels)
    ) |>
    dplyr::select("SUBJECT", "COHORT_YEAR", "FROM_GRADE", "TO_GRADE",
                  "YEAR", "MOBILITY_STATUS", "N", "PCT") |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                   .data[["FROM_GRADE"]], .data[["MOBILITY_STATUS"]])
}


# ==============================================================================
# compute_mobility_by_achievement()
# ==============================================================================

#' Cross mobility status with achievement level to identify selection patterns
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#'
#' @return A data frame: `SUBJECT`, `COHORT_YEAR`, `FROM_GRADE`, `TO_GRADE`,
#'   `MOBILITY_STATUS`, `ACHIEVEMENT_LEVEL`, `N`, `PCT_WITHIN_MOBILITY`.
#'   Returns 0 rows when no persistent student IDs are present.
#' @export
compute_mobility_by_achievement <- function(student_results_long, assessment_spec,
                                             min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT",
          "ACHIEVEMENT_LEVEL") %in% names(student_results_long))
  )

  labels     <- assessment_spec$achievement_levels$labels
  mob_levels <- c("Entering", "Continuing", "Exiting")
  anchor     <- as.integer(assessment_spec$years$cohort_anchor_grade)
  empty      <- data.frame(SUBJECT = character(0), COHORT_YEAR = integer(0),
                            FROM_GRADE = integer(0), TO_GRADE = integer(0),
                            MOBILITY_STATUS = character(0),
                            ACHIEVEMENT_LEVEL = character(0),
                            N = integer(0), PCT_WITHIN_MOBILITY = numeric(0),
                            stringsAsFactors = FALSE)

  base <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["STUDENT_ID"]]),
                  !is.na(.data[["ACHIEVEMENT_LEVEL"]])) |>
    .derive_cohort(assessment_spec) |>
    dplyr::select("STUDENT_ID", "SUBJECT", "COHORT_YEAR",
                  "GRADE", "YEAR", "ACHIEVEMENT_LEVEL")

  if (nrow(base) == 0L || length(unique(base[["STUDENT_ID"]])) <= 1L)
    return(empty)

  grades   <- sort(unique(base[["GRADE"]]))
  subjects <- unique(base[["SUBJECT"]])

  rows <- lapply(seq_len(length(grades) - 1L), function(i) {
    g1 <- grades[i]; g2 <- grades[i + 1L]
    lapply(subjects, function(s) {
      cyrs <- unique(base[["COHORT_YEAR"]][
        base[["SUBJECT"]] == s & !is.na(base[["COHORT_YEAR"]])])
      lapply(cyrs, function(cy) {
        yr1 <- cy + (g1 - anchor); yr2 <- cy + (g2 - anchor)
        d1  <- base[base[["SUBJECT"]] == s & base[["GRADE"]] == g1 &
                    base[["YEAR"]] == yr1 & !is.na(base[["COHORT_YEAR"]]) &
                    base[["COHORT_YEAR"]] == cy, ]
        d2  <- base[base[["SUBJECT"]] == s & base[["GRADE"]] == g2 &
                    base[["YEAR"]] == yr2 & !is.na(base[["COHORT_YEAR"]]) &
                    base[["COHORT_YEAR"]] == cy, ]
        if (nrow(d1) == 0L && nrow(d2) == 0L) return(NULL)

        cont_ids  <- intersect(d1[["STUDENT_ID"]], d2[["STUDENT_ID"]])
        enter_ids <- setdiff(d2[["STUDENT_ID"]], d1[["STUDENT_ID"]])
        exit_ids  <- setdiff(d1[["STUDENT_ID"]], d2[["STUDENT_ID"]])

        # Achievement level for each group
        make_rows <- function(ids, status, ref_data) {
          if (length(ids) == 0L) return(NULL)
          achiev <- ref_data[["ACHIEVEMENT_LEVEL"]][
            ref_data[["STUDENT_ID"]] %in% ids]
          counts <- table(achiev)
          data.frame(
            SUBJECT         = s, COHORT_YEAR = cy,
            FROM_GRADE = g1, TO_GRADE = g2,
            MOBILITY_STATUS = status,
            ACHIEVEMENT_LEVEL = names(counts),
            N               = as.integer(counts),
            stringsAsFactors = FALSE
          )
        }

        dplyr::bind_rows(
          make_rows(cont_ids,  "Continuing", d1),
          make_rows(enter_ids, "Entering",   d2),
          make_rows(exit_ids,  "Exiting",    d1)
        )
      })
    })
  })

  result <- dplyr::bind_rows(unlist(rows, recursive = FALSE))
  if (nrow(result) == 0L) return(empty)

  result |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                    .data[["FROM_GRADE"]], .data[["TO_GRADE"]],
                    .data[["MOBILITY_STATUS"]]) |>
    dplyr::mutate(
      n_mob = sum(.data[["N"]]),
      PCT_WITHIN_MOBILITY = dplyr::if_else(
        .data[["n_mob"]] >= min_n,
        100 * .data[["N"]] / .data[["n_mob"]],
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      MOBILITY_STATUS   = factor(.data[["MOBILITY_STATUS"]], levels = mob_levels),
      ACHIEVEMENT_LEVEL = factor(.data[["ACHIEVEMENT_LEVEL"]],
                                  levels = labels, ordered = TRUE)
    ) |>
    dplyr::select("SUBJECT", "COHORT_YEAR", "FROM_GRADE", "TO_GRADE",
                  "MOBILITY_STATUS", "ACHIEVEMENT_LEVEL", "N",
                  "PCT_WITHIN_MOBILITY") |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                   .data[["FROM_GRADE"]], .data[["MOBILITY_STATUS"]])
}


# ==============================================================================
# compute_cohort_persistence()
# ==============================================================================

#' Track retention of the original anchor-grade cohort across subsequent grades
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#'
#' @return A data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE`, `YEAR`,
#'   `N_ORIGINAL`, `N_ENTERING`, `N_TOTAL`, `PCT_RETAINED`. Returns 0 rows
#'   when no persistent student IDs are present.
#' @export
compute_cohort_persistence <- function(student_results_long, assessment_spec,
                                        min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT") %in%
          names(student_results_long))
  )

  anchor <- as.integer(assessment_spec$years$cohort_anchor_grade)
  empty  <- data.frame(SUBJECT = character(0), COHORT_YEAR = integer(0),
                       GRADE = integer(0), YEAR = integer(0),
                       N_ORIGINAL = integer(0), N_ENTERING = integer(0),
                       N_TOTAL = integer(0), PCT_RETAINED = numeric(0),
                       stringsAsFactors = FALSE)

  base <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]]),
                  !is.na(.data[["STUDENT_ID"]])) |>
    .derive_cohort(assessment_spec) |>
    dplyr::select("STUDENT_ID", "SUBJECT", "COHORT_YEAR", "GRADE", "YEAR")

  if (nrow(base) == 0L || length(unique(base[["STUDENT_ID"]])) <= 1L)
    return(empty)

  # Anchor-grade cohorts
  anchor_ids <- base[base[["GRADE"]] == anchor & !is.na(base[["COHORT_YEAR"]]), ]
  if (nrow(anchor_ids) == 0L) return(empty)

  grades   <- sort(unique(base[["GRADE"]]))
  subjects <- unique(base[["SUBJECT"]])

  rows <- lapply(subjects, function(s) {
    cyrs <- unique(anchor_ids[["COHORT_YEAR"]][anchor_ids[["SUBJECT"]] == s])
    lapply(cyrs, function(cy) {
      original <- anchor_ids[["STUDENT_ID"]][
        anchor_ids[["SUBJECT"]] == s & anchor_ids[["COHORT_YEAR"]] == cy]
      n_anchor <- length(original)
      if (n_anchor == 0L) return(NULL)

      lapply(grades[grades >= anchor], function(g) {
        yr <- cy + (g - anchor)
        at_grade <- base[base[["SUBJECT"]] == s & base[["GRADE"]] == g &
                         base[["YEAR"]] == yr, ]
        if (nrow(at_grade) == 0L) return(NULL)

        n_orig  <- sum(at_grade[["STUDENT_ID"]] %in% original)
        n_total <- nrow(at_grade)
        n_enter <- n_total - n_orig

        data.frame(
          SUBJECT      = s,
          COHORT_YEAR  = cy,
          GRADE        = g,
          YEAR         = yr,
          N_ORIGINAL   = n_orig,
          N_ENTERING   = n_enter,
          N_TOTAL      = n_total,
          PCT_RETAINED = dplyr::if_else(
            n_orig >= min_n,
            100 * n_orig / n_anchor,
            NA_real_
          ),
          stringsAsFactors = FALSE
        )
      })
    })
  })

  result <- dplyr::bind_rows(unlist(rows, recursive = FALSE))
  if (nrow(result) == 0L) return(empty)
  result |> dplyr::arrange(.data[["SUBJECT"]], .data[["COHORT_YEAR"]],
                             .data[["GRADE"]])
}


# ==============================================================================
# compute_enrollment_diagnostics()
# ==============================================================================

#' Flag unusual enrollment changes by grade and year
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Default `10`.
#' @param flag_sd Numeric. SD threshold for z-score flagging. Default `1.5`.
#'
#' @return A named list:
#'   * `$table`     — data frame: `SUBJECT`, `GRADE`, `YEAR`, `N`, `Z_SCORE`,
#'     `FLAGGED`
#'   * `$narrative` — plain-language markdown summary
#' @export
compute_enrollment_diagnostics <- function(student_results_long, assessment_spec,
                                            min_n = 10, flag_sd = 1.5) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("YEAR", "GRADE", "SUBJECT") %in% names(student_results_long))
  )

  tbl <- student_results_long |>
    dplyr::filter(!is.na(.data[["GRADE"]])) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]]) |>
    dplyr::summarize(N = dplyr::n(), .groups = "drop")

  tbl <- tbl |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::mutate(
      mean_n  = mean(.data[["N"]], na.rm = TRUE),
      sd_n    = stats::sd(.data[["N"]], na.rm = TRUE),
      Z_SCORE = dplyr::if_else(
        !is.na(.data[["sd_n"]]) & .data[["sd_n"]] > 0,
        (.data[["N"]] - .data[["mean_n"]]) / .data[["sd_n"]],
        NA_real_
      ),
      FLAGGED = !is.na(.data[["Z_SCORE"]]) & abs(.data[["Z_SCORE"]]) > flag_sd
    ) |>
    dplyr::ungroup() |>
    dplyr::select("SUBJECT", "GRADE", "YEAR", "N", "Z_SCORE", "FLAGGED") |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]])

  n_flagged <- sum(!is.na(tbl[["FLAGGED"]]) & tbl[["FLAGGED"]])
  if (n_flagged == 0L) {
    narrative <- paste0(
      "No enrollment cells were flagged as unusually large or small ",
      "(threshold: ±", flag_sd, " SD from each grade's mean enrollment)."
    )
  } else {
    flagged_rows <- tbl[!is.na(tbl[["FLAGGED"]]) & tbl[["FLAGGED"]], ]
    bullets <- paste0(
      "- ", flagged_rows[["SUBJECT"]], " Grade ", flagged_rows[["GRADE"]],
      " (", flagged_rows[["YEAR"]], "): N = ", flagged_rows[["N"]],
      ", z = ", round(flagged_rows[["Z_SCORE"]], 2),
      collapse = "\n"
    )
    narrative <- paste0(
      n_flagged, " enrollment cell(s) flagged (>±", flag_sd,
      " SD from grade mean):\n\n", bullets
    )
  }

  list(table = tbl, narrative = narrative)
}


# ==============================================================================
# Exported plot_*() functions — delegate to package-level .build_*() helpers
# ==============================================================================

#' Plot mean scale scores by cohort across grades
#'
#' @param cohort_summary Data frame from [compute_cohort_summary()].
#' @param assessment_spec Named list describing the assessment program.
#' @param crosssectional Optional cross-sectional data frame from
#'   [compute_cohort_vs_crosssectional()] to overlay as a dashed reference line.
#'
#' @return Named list of ggplot objects keyed by SUBJECT.
#' @export
plot_cohort_mean_scores <- function(cohort_summary, assessment_spec,
                                     crosssectional = NULL) {
  stopifnot(is.data.frame(cohort_summary), is.list(assessment_spec))
  lh <- build_loss_hoss_lookup(assessment_spec)

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- cohort_summary[cohort_summary[["SUBJECT"]] == s &
                        !is.na(cohort_summary[["MEAN_SCORE"]]), ]
    if (nrow(d) == 0L) return(NULL)
    d[["GRADE_CHR"]]    <- paste0("G", d[["GRADE"]])
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])

    lh_s   <- lh[lh[["SUBJECT"]] == s, ]
    y_lo   <- if (nrow(lh_s) > 0L) min(lh_s[["LOSS"]], na.rm = TRUE) else NA_real_
    y_hi   <- if (nrow(lh_s) > 0L) max(lh_s[["HOSS"]], na.rm = TRUE) else NA_real_
    lbl    <- subj_lookup[["subj_label"]][subj_lookup[["SUBJECT"]] == s]
    if (length(lbl) == 0L) lbl <- s

    cs_df <- NULL
    if (!is.null(crosssectional) && is.data.frame(crosssectional)) {
      cs_df <- crosssectional[crosssectional[["SUBJECT"]] == s &
                              !is.na(crosssectional[["MEAN_SCORE"]]), ]
      if (nrow(cs_df) > 0L) {
        cs_df[["GRADE_CHR"]] <- paste0("G", cs_df[["GRADE"]])
      } else {
        cs_df <- NULL
      }
    }

    .build_cohort_score_plot(d, lbl, y_lo, y_hi, cs_df)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot proficiency rates by cohort across grades
#'
#' @param cohort_summary Data frame from [compute_cohort_summary()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT.
#' @export
plot_cohort_proficiency_rates <- function(cohort_summary, assessment_spec) {
  stopifnot(is.data.frame(cohort_summary), is.list(assessment_spec))

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- cohort_summary[cohort_summary[["SUBJECT"]] == s &
                        !is.na(cohort_summary[["PCT_PROFICIENT"]]), ]
    if (nrow(d) == 0L) return(NULL)
    d[["GRADE_CHR"]]     <- paste0("G", d[["GRADE"]])
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])
    lbl <- subj_lookup[["subj_label"]][subj_lookup[["SUBJECT"]] == s]
    if (length(lbl) == 0L) lbl <- s
    .build_cohort_proficiency_plot(d, lbl)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot score distributions by cohort using pre-computed quantiles
#'
#' @param cohort_summary Data frame from [compute_cohort_summary()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT.
#' @export
plot_cohort_score_distributions <- function(cohort_summary, assessment_spec) {
  stopifnot(is.data.frame(cohort_summary), is.list(assessment_spec))

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- cohort_summary[cohort_summary[["SUBJECT"]] == s &
                        !is.na(cohort_summary[["MEDIAN_SCORE"]]), ]
    if (nrow(d) == 0L) return(NULL)
    d[["GRADE_CHR"]]     <- paste0("G", d[["GRADE"]])
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])
    iqr <- d[["Q3_SCORE"]] - d[["Q1_SCORE"]]
    d[["IQR_LO"]]  <- d[["Q1_SCORE"]] - 1.5 * iqr
    d[["IQR_HI"]]  <- d[["Q3_SCORE"]] + 1.5 * iqr
    lbl <- subj_lookup[["subj_label"]][subj_lookup[["SUBJECT"]] == s]
    if (length(lbl) == 0L) lbl <- s
    .build_cohort_dist_plot(d, lbl)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot Sankey / alluvial transition diagrams
#'
#' Requires the `ggalluvial` package (in Suggests). Returns `NULL` if not
#' installed or if `transition_df` has 0 rows.
#'
#' @param transition_df Data frame from [compute_cohort_transitions()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT, or `NULL`.
#' @export
plot_sankey_transitions <- function(transition_df, assessment_spec) {
  stopifnot(is.data.frame(transition_df), is.list(assessment_spec))

  if (nrow(transition_df) == 0L) {
    message("plot_sankey_transitions: transition_df is empty; ",
            "persistent student IDs are required.")
    return(NULL)
  }
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    message("install.packages('ggalluvial') to produce Sankey transition plots.")
    return(NULL)
  }

  labels <- assessment_spec$achievement_levels$labels
  n_lev  <- length(labels)
  palette <- grDevices::colorRampPalette(c("#d62728", "#ffbb78", "#2ca02c"))(n_lev)
  level_palette <- stats::setNames(palette, labels)

  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- transition_df[transition_df[["SUBJECT"]] == s, ]
    if (nrow(d) == 0L) return(NULL)
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])
    lbl <- assessment_spec$subjects[[s]]$label
    if (is.null(lbl)) lbl <- s
    .build_sankey_plot(d, lbl, level_palette)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot demographic composition of cohorts
#'
#' @param demo_df Data frame from [compute_cohort_demographics()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT.
#' @export
plot_cohort_demographic_composition <- function(demo_df, assessment_spec) {
  stopifnot(is.data.frame(demo_df), is.list(assessment_spec))

  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- demo_df[demo_df[["SUBJECT"]] == s & !is.na(demo_df[["PCT"]]), ]
    if (nrow(d) == 0L) return(NULL)
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])
    lbl <- assessment_spec$subjects[[s]]$label
    if (is.null(lbl)) lbl <- s
    .build_demographic_composition_plot(d, lbl)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot rates of improving, stable, and declining trajectories
#'
#' @param roc_df Data frame from [compute_cohort_rate_of_change()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT, or `NULL`.
#' @export
plot_rate_of_change <- function(roc_df, assessment_spec) {
  stopifnot(is.data.frame(roc_df), is.list(assessment_spec))
  if (nrow(roc_df) == 0L) {
    message("plot_rate_of_change: no data; persistent student IDs are required.")
    return(NULL)
  }
  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- roc_df[roc_df[["SUBJECT"]] == s & !is.na(roc_df[["PCT"]]), ]
    if (nrow(d) == 0L) return(NULL)
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])
    lbl <- assessment_spec$subjects[[s]]$label
    if (is.null(lbl)) lbl <- s
    .build_roc_plot(d, lbl)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot Pearson correlations of scale scores across adjacent grades
#'
#' @param corr_df Data frame from [compute_cohort_correlations()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT, or `NULL`.
#' @export
plot_score_correlations <- function(corr_df, assessment_spec) {
  stopifnot(is.data.frame(corr_df), is.list(assessment_spec))
  if (nrow(corr_df) == 0L) {
    message("plot_score_correlations: no data; persistent student IDs required.")
    return(NULL)
  }
  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- corr_df[corr_df[["SUBJECT"]] == s, ]
    if (nrow(d) == 0L) return(NULL)
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])
    lbl <- assessment_spec$subjects[[s]]$label
    if (is.null(lbl)) lbl <- s
    .build_correlation_plot(d, lbl)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot mobility status distribution across grades
#'
#' @param mobility_df Data frame from [compute_cohort_mobility()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT, or `NULL`.
#' @export
plot_mobility_distribution <- function(mobility_df, assessment_spec) {
  stopifnot(is.data.frame(mobility_df), is.list(assessment_spec))
  if (nrow(mobility_df) == 0L) {
    message("plot_mobility_distribution: no data; persistent student IDs required.")
    return(NULL)
  }
  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- mobility_df[mobility_df[["SUBJECT"]] == s & !is.na(mobility_df[["PCT"]]), ]
    if (nrow(d) == 0L) return(NULL)
    d[["GRADE_CHR"]]     <- paste0("G", d[["TO_GRADE"]])
    d[["COHORT_YR_CHR"]] <- as.character(d[["COHORT_YEAR"]])
    lbl <- assessment_spec$subjects[[s]]$label
    if (is.null(lbl)) lbl <- s
    .build_mobility_dist_plot(d, lbl)
  }), subjects) |> Filter(Negate(is.null), x = _)
}

#' Plot mobility status crossed with achievement level
#'
#' @param mob_ach_df Data frame from [compute_mobility_by_achievement()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return Named list of ggplot objects keyed by SUBJECT, or `NULL`.
#' @export
plot_mobility_by_achievement <- function(mob_ach_df, assessment_spec) {
  stopifnot(is.data.frame(mob_ach_df), is.list(assessment_spec))
  if (nrow(mob_ach_df) == 0L) {
    message("plot_mobility_by_achievement: no data; persistent student IDs required.")
    return(NULL)
  }
  subjects <- names(assessment_spec$subjects)
  setNames(lapply(subjects, function(s) {
    d <- mob_ach_df[mob_ach_df[["SUBJECT"]] == s &
                    !is.na(mob_ach_df[["PCT_WITHIN_MOBILITY"]]), ]
    if (nrow(d) == 0L) return(NULL)
    lbl <- assessment_spec$subjects[[s]]$label
    if (is.null(lbl)) lbl <- s
    .build_mobility_achievement_plot(d, lbl)
  }), subjects) |> Filter(Negate(is.null), x = _)
}


# ==============================================================================
# summarize_cohort_analysis()
# ==============================================================================

#' Generate plain-language narrative summary of cohort analysis findings
#'
#' @param cohort_summary Data frame from [compute_cohort_summary()].
#' @param transition_df Data frame from [compute_cohort_transitions()].
#' @param roc_df Data frame from [compute_cohort_rate_of_change()].
#' @param corr_df Data frame from [compute_cohort_correlations()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list: `$narrative`, `$by_subject`, `$table`.
#' @export
summarize_cohort_analysis <- function(cohort_summary, transition_df, roc_df,
                                       corr_df, assessment_spec) {
  stopifnot(
    is.data.frame(cohort_summary),
    is.list(assessment_spec)
  )

  subj_lookup <- data.frame(
    SUBJECT    = names(assessment_spec$subjects),
    subj_label = vapply(names(assessment_spec$subjects),
                        function(s) assessment_spec$subjects[[s]]$label,
                        character(1)),
    stringsAsFactors = FALSE
  )

  subjects <- unique(cohort_summary[["SUBJECT"]])
  by_subject <- setNames(lapply(subjects, function(s) {
    d_s <- cohort_summary[cohort_summary[["SUBJECT"]] == s &
                          !is.na(cohort_summary[["MEAN_SCORE"]]), ]
    lbl <- subj_lookup[["subj_label"]][subj_lookup[["SUBJECT"]] == s]
    if (length(lbl) == 0L) lbl <- s

    if (nrow(d_s) == 0L) return(paste0(lbl, ": insufficient data."))

    n_cohorts <- length(unique(d_s[["COHORT_YEAR"]]))
    grades    <- sort(unique(d_s[["GRADE"]]))
    n_grades  <- length(grades)

    paste0(lbl, ": ", n_cohorts, " cohort(s) tracked across ",
           n_grades, " grade(s) (Grades ",
           min(grades), "–", max(grades), ").")
  }), subjects)

  # ROC summary
  roc_note <- if (!is.null(roc_df) && nrow(roc_df) > 0L) {
    pct_improving <- mean(roc_df[["PCT"]][roc_df[["TRAJECTORY"]] == "Improving"],
                          na.rm = TRUE)
    pct_declining <- mean(roc_df[["PCT"]][roc_df[["TRAJECTORY"]] == "Declining"],
                          na.rm = TRUE)
    sprintf("On average across grade transitions, %.1f%% of matched students improved their achievement level and %.1f%% declined.",
            pct_improving, pct_declining)
  } else {
    "Trajectory analysis requires persistent student IDs across years."
  }

  corr_note <- if (!is.null(corr_df) && nrow(corr_df) > 0L) {
    avg_r <- mean(corr_df[["R"]], na.rm = TRUE)
    sprintf("Adjacent-grade score correlations averaged r = %.2f across all cohorts and subjects.", avg_r)
  } else {
    "Score correlation analysis requires persistent student IDs."
  }

  n_cohorts_total <- length(unique(cohort_summary[["COHORT_YEAR"]]))
  n_subj          <- length(unique(cohort_summary[["SUBJECT"]]))

  narrative <- paste0(
    "This section presents cohort composition and performance results for ",
    n_subj, " subject(s) across ", n_cohorts_total, " cohort(s). ",
    roc_note, " ",
    corr_note, " ",
    "See section tables and plots for subject- and grade-specific details."
  )

  list(
    narrative  = narrative,
    by_subject = by_subject,
    table      = cohort_summary
  )
}


# ==============================================================================
# run_cohort_analysis()
# ==============================================================================

#' Run all Line 1.3 cohort analysis sub-analyses
#'
#' Calls all `compute_*()` and `plot_*()` functions and returns a single
#' named results list. This is the primary function for cohort analysis;
#' [run_general_cohort()] wraps it with RDS saving and report rendering.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract).
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum cell size for suppression. Default `10`.
#'
#' @return A named list containing all tables, plots, and narrative text.
#' @export
run_cohort_analysis <- function(student_results_long, assessment_spec,
                                 min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT", "SCALE_SCORE",
          "ACHIEVEMENT_LEVEL") %in% names(student_results_long))
  )

  message("Computing cohort summary...")
  cohort_summary <- compute_cohort_summary(student_results_long, assessment_spec,
                                            min_n = min_n)

  message("Computing cohort demographics...")
  demo_df <- compute_cohort_demographics(student_results_long, assessment_spec,
                                          min_n = min_n)

  message("Computing cohort transitions...")
  transition_df <- compute_cohort_transitions(student_results_long, assessment_spec,
                                               min_n = min_n)

  message("Computing rate of change...")
  roc_df <- compute_cohort_rate_of_change(student_results_long, assessment_spec,
                                           min_n = min_n)

  message("Computing cohort correlations...")
  corr_df <- compute_cohort_correlations(student_results_long, assessment_spec,
                                          min_n = min_n)

  message("Computing cohort vs. cross-sectional...")
  cohort_vs_xs <- compute_cohort_vs_crosssectional(student_results_long,
                                                    assessment_spec, min_n = min_n)

  message("Computing cohort mobility...")
  mobility_df <- compute_cohort_mobility(student_results_long, assessment_spec,
                                          min_n = min_n)

  message("Computing mobility by achievement...")
  mob_ach_df <- compute_mobility_by_achievement(student_results_long, assessment_spec,
                                                 min_n = min_n)

  message("Computing cohort persistence...")
  persistence_df <- compute_cohort_persistence(student_results_long, assessment_spec,
                                                min_n = min_n)

  message("Computing enrollment diagnostics...")
  enrollment_diag <- compute_enrollment_diagnostics(student_results_long,
                                                     assessment_spec, min_n = min_n)

  message("Building plots...")
  mean_scores_plots  <- plot_cohort_mean_scores(cohort_summary, assessment_spec,
                                                 cohort_vs_xs$crosssectional)
  prof_plots         <- plot_cohort_proficiency_rates(cohort_summary, assessment_spec)
  dist_plots         <- plot_cohort_score_distributions(cohort_summary, assessment_spec)
  sankey_plots       <- plot_sankey_transitions(transition_df, assessment_spec)
  demo_plots         <- plot_cohort_demographic_composition(demo_df, assessment_spec)
  roc_plots          <- plot_rate_of_change(roc_df, assessment_spec)
  corr_plots         <- plot_score_correlations(corr_df, assessment_spec)
  mob_dist_plots     <- plot_mobility_distribution(mobility_df, assessment_spec)
  mob_ach_plots      <- plot_mobility_by_achievement(mob_ach_df, assessment_spec)

  message("Summarizing findings...")
  txt <- summarize_cohort_analysis(cohort_summary, transition_df, roc_df,
                                    corr_df, assessment_spec)

  list(
    summary            = cohort_summary,
    demographics       = demo_df,
    transitions        = transition_df,
    rate_of_change     = roc_df,
    correlations       = corr_df,
    crosssectional     = cohort_vs_xs,
    mobility           = mobility_df,
    mobility_by_achiev = mob_ach_df,
    persistence        = persistence_df,
    enrollment_diag    = enrollment_diag,
    plot = list(
      mean_scores             = mean_scores_plots,
      proficiency_rates       = prof_plots,
      distributions           = dist_plots,
      sankey                  = sankey_plots,
      demographics            = demo_plots,
      rate_of_change          = roc_plots,
      correlations            = corr_plots,
      mobility_distribution   = mob_dist_plots,
      mobility_by_achievement = mob_ach_plots
    ),
    text = txt
  )
}
