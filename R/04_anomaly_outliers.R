# ==============================================================================
# Line 4 — Score Quality Analysis: Year-to-Year Outlier Detection
#
# Identifies unusual year-to-year changes in scale scores, achievement level
# distributions, and demographic composition at the statewide level
# (YEAR x SUBJECT x GRADE).
#
# Functions:
#   compute_outliers()     — flag year-to-year outlier cells
#   plot_outliers()        — visualise flagged outliers
#   summarize_outliers()   — narrative summary of flagged cells
#   run_outliers()         — entry point; returns the sub-list
# ==============================================================================


# Classify a character/logical value as the "positive" category for a binary
# demographic (Yes / Y / TRUE / 1). Returns FALSE for NA, "No", "N", "FALSE",
# "0", and empty strings.
.is_positive_flag <- function(x) {
  !is.na(x) &
    !grepl("^(no|n|false|0)$", trimws(tolower(as.character(x))))
}


#' Compute year-to-year outliers in assessment results
#'
#' Identifies cells (YEAR x SUBJECT x GRADE) where the year-to-year change in
#' mean scale score, percent proficient, or binary demographic composition falls
#' more than 2 standard deviations from the mean change across all cells.
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size for suppression. Default `10`.
#'
#' @return A data frame with one row per YEAR x SUBJECT x GRADE containing
#'   base metrics (`mean_score`, `pct_proficient`, `pct_<demo>`), year-to-year
#'   delta columns (`delta_*`), and outlier flag columns (`outlier_flag`,
#'   `outlier_metric`, `outlier_direction`).
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' tbl <- compute_outliers(demo_data, assessment_spec)
#' }
compute_outliers <- function(student_results_long,
                             assessment_spec,
                             min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  # Proficiency levels at or above the policy benchmark
  bm_idx           <- benchmark_index(assessment_spec)
  n_levels         <- length(assessment_spec$achievement_levels$labels)
  proficient_levels <- assessment_spec$achievement_levels$labels[bm_idx:n_levels]

  # Binary demographic variables present in the data
  all_demo    <- names(assessment_spec$demographics)
  binary_vars <- all_demo[
    vapply(assessment_spec$demographics,
           function(d) identical(d$type, "binary"), logical(1))
  ]
  binary_vars <- intersect(binary_vars, names(student_results_long))

  # ------------------------------------------------------------------
  # Step 1a — base metrics per YEAR x SUBJECT x GRADE
  # ------------------------------------------------------------------
  base_tbl <- student_results_long |>
    dplyr::group_by(
      .data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_score = mean(.data[["SCALE_SCORE"]], na.rm = TRUE),
      pct_proficient = mean(
        .data[["ACHIEVEMENT_LEVEL"]] %in% proficient_levels,
        na.rm = TRUE
      ) * 100,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean_score = dplyr::if_else(
        .data[["n"]] < min_n, NA_real_, .data[["mean_score"]]
      ),
      pct_proficient = dplyr::if_else(
        .data[["n"]] < min_n, NA_real_, .data[["pct_proficient"]]
      )
    )

  # ------------------------------------------------------------------
  # Step 1b — binary demographic percents per YEAR x SUBJECT x GRADE
  # ------------------------------------------------------------------
  if (length(binary_vars) > 0) {
    demo_tbls <- lapply(binary_vars, function(v) {
      student_results_long |>
        dplyr::group_by(
          .data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]
        ) |>
        dplyr::summarise(
          n_grp = sum(!is.na(.data[[v]])),
          pct_v = mean(.is_positive_flag(.data[[v]]), na.rm = TRUE) * 100,
          .groups = "drop"
        ) |>
        dplyr::mutate(
          pct_v = dplyr::if_else(
            .data[["n_grp"]] < min_n, NA_real_, .data[["pct_v"]]
          )
        ) |>
        dplyr::select(-"n_grp") |>
        dplyr::rename(!!paste0("pct_", v) := "pct_v")
    })

    metrics_tbl <- Reduce(
      function(a, b) dplyr::left_join(a, b, by = c("YEAR", "SUBJECT", "GRADE")),
      c(list(base_tbl), demo_tbls)
    )
  } else {
    metrics_tbl <- base_tbl
  }

  # ------------------------------------------------------------------
  # Step 2 — year-to-year deltas (consecutive years only)
  # ------------------------------------------------------------------
  delta_cols <- c("mean_score", "pct_proficient", paste0("pct_", binary_vars))

  metrics_tbl <- metrics_tbl |>
    dplyr::mutate(YEAR = as.integer(.data[["YEAR"]])) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]]) |>
    dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::mutate(
      prior_yr_    = dplyr::lag(.data[["YEAR"]]),
      consecutive_ = !is.na(.data[["prior_yr_"]]) &
                     .data[["YEAR"]] == .data[["prior_yr_"]] + 1L
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(delta_cols),
        ~ dplyr::if_else(
          .data[["consecutive_"]] & !is.na(.x) & !is.na(dplyr::lag(.x)),
          .x - dplyr::lag(.x),
          NA_real_
        ),
        .names = "delta_{.col}"
      )
    ) |>
    dplyr::select(-"prior_yr_", -"consecutive_") |>
    dplyr::ungroup()

  # ------------------------------------------------------------------
  # Step 3 — standardise deltas; flag outliers at ±2 SD (Mild) / ±3 SD (Severe)
  # ------------------------------------------------------------------
  delta_names <- paste0("delta_", delta_cols)

  # Global mean and SD for each delta metric (pooled across all cells)
  global_stats <- lapply(delta_names, function(col) {
    vals <- metrics_tbl[[col]]
    list(
      mu    = mean(vals, na.rm = TRUE),
      sigma = stats::sd(vals, na.rm = TRUE)
    )
  })
  names(global_stats) <- delta_names

  # Add z-score columns
  z_cols <- paste0("z_", delta_names)
  for (i in seq_along(delta_names)) {
    col   <- delta_names[i]
    mu    <- global_stats[[col]]$mu
    sigma <- global_stats[[col]]$sigma
    metrics_tbl[[z_cols[i]]] <- if (!is.na(sigma) && sigma > 0) {
      (metrics_tbl[[col]] - mu) / sigma
    } else {
      NA_real_
    }
  }

  # For each YEAR x SUBJECT x GRADE cell find the metric with the highest |z|
  worst_z <- metrics_tbl |>
    dplyr::select("YEAR", "SUBJECT", "GRADE", dplyr::all_of(z_cols)) |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(z_cols),
      names_to  = "z_col_nm",
      values_to = "z_val"
    ) |>
    dplyr::mutate(
      abs_z       = abs(.data[["z_val"]]),
      metric_name = sub("^z_delta_", "", .data[["z_col_nm"]])
    ) |>
    dplyr::group_by(.data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::slice_max(.data[["abs_z"]], n = 1L,
                     with_ties = FALSE, na_rm = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      outlier_flag = dplyr::case_when(
        is.na(.data[["abs_z"]])  ~ "None",
        .data[["abs_z"]] >= 3    ~ "Severe",
        .data[["abs_z"]] >= 2    ~ "Mild",
        TRUE                     ~ "None"
      ),
      outlier_metric = dplyr::if_else(
        .data[["outlier_flag"]] != "None",
        .data[["metric_name"]],
        NA_character_
      ),
      outlier_direction = dplyr::case_when(
        .data[["outlier_flag"]] != "None" &
          !is.na(.data[["z_val"]]) & .data[["z_val"]] > 0 ~ "increase",
        .data[["outlier_flag"]] != "None" &
          !is.na(.data[["z_val"]]) & .data[["z_val"]] < 0 ~ "decrease",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select("YEAR", "SUBJECT", "GRADE",
                  "outlier_flag", "outlier_metric", "outlier_direction")

  # Join flags back; cells with no prior-year comparison get flag = "None"
  metrics_tbl |>
    dplyr::select(-dplyr::all_of(z_cols)) |>
    dplyr::left_join(worst_z, by = c("YEAR", "SUBJECT", "GRADE")) |>
    dplyr::mutate(
      outlier_flag = dplyr::coalesce(.data[["outlier_flag"]], "None")
    )
}


#' Plot year-to-year outliers
#'
#' Returns a named list of two Cleveland-style connected dot plots showing
#' year-to-year changes in mean scale score and percent proficient by grade,
#' faceted by subject. Outlier cells are highlighted with a distinct point shape
#' and size.
#'
#' @param outlier_table Data frame returned by [compute_outliers()].
#' @param assessment_spec Named list describing the assessment program (used for
#'   subject labels).
#'
#' @return A named list with elements `$score` and `$proficiency`, each a
#'   `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_outliers(demo_data, assessment_spec)
#' plots <- plot_outliers(tbl, assessment_spec)
#' plots$score
#' plots$proficiency
#' }
plot_outliers <- function(outlier_table, assessment_spec) {
  stopifnot(
    is.data.frame(outlier_table),
    is.list(assessment_spec)
  )

  # Build subject label lookup
  subj_labels <- vapply(
    names(assessment_spec$subjects),
    function(s) assessment_spec$subjects[[s]]$label,
    character(1)
  )

  make_dot_plot <- function(delta_col, y_label, title) {
    tbl <- outlier_table |>
      dplyr::filter(!is.na(.data[[delta_col]])) |>
      dplyr::mutate(
        grade_lbl  = dplyr::if_else(
          is.na(.data[["GRADE"]]),
          "EOC",
          paste0("Grade ", .data[["GRADE"]])
        ),
        # Year-pair label: "2014-2015" meaning change from 2014 to 2015
        year_pair  = paste0(.data[["YEAR"]] - 1L, "\u2013", .data[["YEAR"]]),
        point_type = dplyr::case_when(
          .data[["outlier_flag"]] == "Severe" ~ "Severe",
          .data[["outlier_flag"]] == "Mild"   ~ "Mild",
          TRUE                                ~ "Normal"
        ),
        subj_label = dplyr::recode(
          .data[["SUBJECT"]], !!!subj_labels, .default = .data[["SUBJECT"]]
        )
      ) |>
      # Keep year_pair ordered chronologically
      dplyr::mutate(
        year_pair = factor(
          .data[["year_pair"]],
          levels = unique(.data[["year_pair"]][order(.data[["YEAR"]])])
        )
      )

    if (nrow(tbl) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(title = paste(title, "(no data)")) +
          ggplot2::theme_minimal()
      )
    }

    ggplot2::ggplot(
      tbl,
      ggplot2::aes(
        x     = .data[["year_pair"]],
        y     = .data[[delta_col]],
        color = .data[["grade_lbl"]],
        group = .data[["grade_lbl"]]
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed",
        color = "grey40", linewidth = 0.4
      ) +
      ggplot2::geom_line(alpha = 0.6, linewidth = 0.7) +
      ggplot2::geom_point(
        ggplot2::aes(
          size  = .data[["point_type"]],
          shape = .data[["point_type"]]
        )
      ) +
      ggplot2::facet_wrap(~ subj_label, scales = "free_y", ncol = 1) +
      ggplot2::scale_size_manual(
        values = c("Normal" = 2, "Mild" = 3.5, "Severe" = 5),
        name   = "Severity"
      ) +
      ggplot2::scale_shape_manual(
        values = c("Normal" = 16, "Mild" = 17, "Severe" = 8),
        name   = "Severity"
      ) +
      ggplot2::scale_color_viridis_d(name = "Grade", option = "D", end = 0.9) +
      ggplot2::labs(
        title = title,
        x     = "Year transition",
        y     = y_label
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor  = ggplot2::element_blank(),
        axis.text.x       = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position   = "right"
      )
  }

  list(
    score = make_dot_plot(
      delta_col = "delta_mean_score",
      y_label   = "Change in mean scale score (current \u2212 prior year)",
      title     = "Year-to-Year Changes in Mean Scale Score by Grade"
    ),
    proficiency = make_dot_plot(
      delta_col = "delta_pct_proficient",
      y_label   = "Change in % proficient (percentage points)",
      title     = "Year-to-Year Changes in Percent Proficient by Grade"
    )
  )
}


#' Summarize year-to-year outlier findings
#'
#' Returns a narrative and summary table describing the number and nature of
#' flagged year-to-year outlier cells.
#'
#' @param outlier_table Data frame returned by [compute_outliers()].
#'
#' @return A named list with `$table` (the full outlier table), `$by_subject`
#'   (count of flagged cells by subject and severity), and `$narrative`
#'   (character vector of 1–3 sentences).
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_outliers(demo_data, assessment_spec)
#' summarize_outliers(tbl)
#' }
summarize_outliers <- function(outlier_table) {
  stopifnot(is.data.frame(outlier_table))

  no_data_result <- function() {
    list(
      table      = outlier_table,
      by_subject = data.frame(
        SUBJECT      = character(0),
        outlier_flag = character(0),
        n_cells      = integer(0),
        stringsAsFactors = FALSE
      ),
      narrative  = "No year-to-year comparison data were available."
    )
  }

  if (nrow(outlier_table) == 0 ||
      !("outlier_flag" %in% names(outlier_table))) {
    return(no_data_result())
  }

  n_flagged <- sum(outlier_table$outlier_flag != "None", na.rm = TRUE)
  n_total   <- sum(!is.na(outlier_table$outlier_flag) &
                   !is.na(outlier_table$delta_mean_score |
                          outlier_table$delta_pct_proficient))

  # Summary of flagged cells by subject and severity
  by_subject <- outlier_table |>
    dplyr::filter(.data[["outlier_flag"]] != "None") |>
    dplyr::count(.data[["SUBJECT"]], .data[["outlier_flag"]],
                 name = "n_cells")

  # --- Narrative ---
  if (n_flagged == 0) {
    narrative <- paste0(
      "Year-to-year changes in mean scale scores, percent proficient, and ",
      "demographic composition were within the expected range (\u00b12 SD) ",
      "across all subject-grade combinations."
    )
    return(list(table = outlier_table, by_subject = by_subject,
                narrative = narrative))
  }

  n_severe <- sum(outlier_table$outlier_flag == "Severe", na.rm = TRUE)
  n_mild   <- sum(outlier_table$outlier_flag == "Mild",   na.rm = TRUE)

  sent1 <- sprintf(
    "%d year-subject-grade combination%s showed unusual year-to-year changes ",
    n_flagged, if (n_flagged == 1) "" else "s"
  )
  sent1 <- paste0(
    sent1,
    sprintf("(%d Severe, %d Mild; threshold: \u00b12 SD from the mean change).",
            n_severe, n_mild)
  )

  # Largest mean score change
  score_rows <- outlier_table |>
    dplyr::filter(!is.na(.data[["delta_mean_score"]])) |>
    dplyr::mutate(abs_delta_s = abs(.data[["delta_mean_score"]])) |>
    dplyr::slice_max(.data[["abs_delta_s"]], n = 1L,
                     with_ties = FALSE)

  prof_rows <- outlier_table |>
    dplyr::filter(!is.na(.data[["delta_pct_proficient"]])) |>
    dplyr::mutate(abs_delta_p = abs(.data[["delta_pct_proficient"]])) |>
    dplyr::slice_max(.data[["abs_delta_p"]], n = 1L,
                     with_ties = FALSE)

  sent2_parts <- character(0)
  if (nrow(score_rows) > 0) {
    r   <- score_rows[1, ]
    dir <- if (r$delta_mean_score > 0) "increase" else "decrease"
    sent2_parts <- c(sent2_parts, sprintf(
      "the largest change in mean scale score was a %.1f-point %s in %s Grade %s (%d)",
      abs(r$delta_mean_score), dir,
      r$SUBJECT, r$GRADE, as.integer(r$YEAR)
    ))
  }
  if (nrow(prof_rows) > 0) {
    r   <- prof_rows[1, ]
    dir <- if (r$delta_pct_proficient > 0) "increase" else "decrease"
    sent2_parts <- c(sent2_parts, sprintf(
      "the largest change in percent proficient was a %.1f pp %s in %s Grade %s (%d)",
      abs(r$delta_pct_proficient), dir,
      r$SUBJECT, r$GRADE, as.integer(r$YEAR)
    ))
  }
  sent2 <- if (length(sent2_parts) > 0) {
    paste0("Among all cells, ",
           paste(sent2_parts, collapse = "; "), ".")
  } else {
    ""
  }

  # Demographic composition flags
  demo_flagged_metrics <- outlier_table$outlier_metric[
    !is.na(outlier_table$outlier_metric) &
    outlier_table$outlier_flag != "None" &
    grepl("^pct_", outlier_table$outlier_metric, perl = TRUE) &
    outlier_table$outlier_metric != "pct_proficient"
  ]
  sent3 <- if (length(demo_flagged_metrics) > 0) {
    "At least one demographic composition shift was also flagged as an outlier."
  } else {
    "No unusual shifts in demographic composition were flagged."
  }

  narrative <- c(sent1, sent2, sent3)
  narrative <- narrative[nchar(narrative) > 0]

  list(table = outlier_table, by_subject = by_subject, narrative = narrative)
}


#' Run the outlier sub-analysis and return the results sub-list
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size for suppression. Default `10`.
#'
#' @return A named list with `$table`, `$plot` (list of `$score` and
#'   `$proficiency`), and `$text` (list of `$table`, `$by_subject`,
#'   `$narrative`).
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' sub <- run_outliers(demo_data, assessment_spec)
#' }
run_outliers <- function(student_results_long,
                         assessment_spec,
                         min_n = 10) {
  tbl <- compute_outliers(student_results_long, assessment_spec, min_n = min_n)
  list(
    table = tbl,
    plot  = plot_outliers(tbl, assessment_spec),
    text  = summarize_outliers(tbl)
  )
}
