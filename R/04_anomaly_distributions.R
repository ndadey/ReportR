# ==============================================================================
# Line 4 — Score Quality Analysis: Distribution Shape Diagnostics
#
# Screens for unusual score distributions at the statewide level within each
# YEAR x SUBJECT x GRADE cell: flat distributions, bimodal patterns, or
# extreme skewness that may indicate data quality concerns.
#
# Functions:
#   compute_distributions()   — distribution shape statistics and flags
#   plot_distributions()      — visualise flagged distributions
#   summarize_distributions() — narrative summary
#   run_distributions()       — entry point; returns the sub-list
# ==============================================================================


# Inline shape statistic helpers (no extra package dependencies) ---------------

.skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  mu <- mean(x)
  s  <- stats::sd(x)
  if (s == 0) return(NA_real_)
  mean((x - mu)^3) / s^3
}

.kurtosis_excess <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4L) return(NA_real_)
  mu <- mean(x)
  s  <- stats::sd(x)
  if (s == 0) return(NA_real_)
  mean((x - mu)^4) / s^4 - 3
}

.bimodality_coeff <- function(skew, kurt_excess, n) {
  if (is.na(skew) || is.na(kurt_excess) || is.na(n) || n < 4L) return(NA_real_)
  denom <- kurt_excess + 3 * (n - 1)^2 / ((n - 2) * (n - 3))
  if (is.na(denom) || denom == 0) return(NA_real_)
  (skew^2 + 1) / denom
}


#' Compute distribution shape diagnostics
#'
#' Calculates summary statistics for scale score distributions within each
#' YEAR x SUBJECT x GRADE cell to screen for unusual shapes (flat, bimodal,
#' or highly skewed). All statistics are suppressed to `NA` where `n < min_n`.
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size for suppression. Default `10`.
#'
#' @return A data frame with one row per YEAR x SUBJECT x GRADE containing
#'   shape statistics (`mean`, `sd`, `skewness`, `kurtosis`,
#'   `bimodality_coefficient`, `p05`–`p95`) plus `distribution_flag` and
#'   `flag_reason`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' tbl <- compute_distributions(demo_data, assessment_spec)
#' }
compute_distributions <- function(student_results_long,
                                  assessment_spec,
                                  min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  # Build LOSS/HOSS lookup to determine score range per cell
  lh_lookup <- build_loss_hoss_lookup(assessment_spec)

  # Compute shape statistics per YEAR x SUBJECT x GRADE
  stats_tbl <- student_results_long |>
    dplyr::filter(!is.na(.data[["SCALE_SCORE"]])) |>
    dplyr::group_by(
      .data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]
    ) |>
    dplyr::summarise(
      n        = dplyr::n(),
      mean     = mean(.data[["SCALE_SCORE"]], na.rm = TRUE),
      sd       = stats::sd(.data[["SCALE_SCORE"]], na.rm = TRUE),
      skewness = .skewness(.data[["SCALE_SCORE"]]),
      kurtosis = .kurtosis_excess(.data[["SCALE_SCORE"]]),
      p05      = stats::quantile(.data[["SCALE_SCORE"]], 0.05, na.rm = TRUE),
      p25      = stats::quantile(.data[["SCALE_SCORE"]], 0.25, na.rm = TRUE),
      p50      = stats::quantile(.data[["SCALE_SCORE"]], 0.50, na.rm = TRUE),
      p75      = stats::quantile(.data[["SCALE_SCORE"]], 0.75, na.rm = TRUE),
      p95      = stats::quantile(.data[["SCALE_SCORE"]], 0.95, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      bimodality_coefficient = as.numeric(mapply(
        .bimodality_coeff,
        .data[["skewness"]], .data[["kurtosis"]], .data[["n"]]
      ))
    )

  # Suppress all shape stats where n < min_n
  shape_cols <- c("mean", "sd", "skewness", "kurtosis",
                  "bimodality_coefficient",
                  "p05", "p25", "p50", "p75", "p95")
  small <- stats_tbl[["n"]] < min_n
  stats_tbl[small, shape_cols] <- NA_real_

  # Join LOSS/HOSS for flatness threshold
  stats_tbl <- stats_tbl |>
    dplyr::left_join(lh_lookup, by = c("SUBJECT", "GRADE"))

  # Apply flags (Bimodal > Skewed > Flat in priority)
  stats_tbl <- stats_tbl |>
    dplyr::mutate(
      flat_flag     = !is.na(.data[["sd"]]) &
                      !is.na(.data[["LOSS"]]) &
                      !is.na(.data[["HOSS"]]) &
                      .data[["sd"]] < 0.25 * (.data[["HOSS"]] - .data[["LOSS"]]),
      bimodal_flag  = !is.na(.data[["bimodality_coefficient"]]) &
                      .data[["bimodality_coefficient"]] > 0.555,
      skewed_flag   = !is.na(.data[["skewness"]]) &
                      abs(.data[["skewness"]]) > 1.0,
      distribution_flag = dplyr::case_when(
        .data[["bimodal_flag"]] ~ "Bimodal",
        .data[["skewed_flag"]]  ~ "Skewed",
        .data[["flat_flag"]]    ~ "Flat",
        TRUE                    ~ "None"
      ),
      flag_reason = dplyr::case_when(
        .data[["bimodal_flag"]] ~
          "Bimodality coefficient above threshold (0.555)",
        .data[["skewed_flag"]]  ~
          "Absolute skewness exceeds 1.0",
        .data[["flat_flag"]]    ~
          "SD unusually low relative to score range",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      "YEAR", "SUBJECT", "GRADE", "n",
      "mean", "sd", "skewness", "kurtosis", "bimodality_coefficient",
      "p05", "p25", "p50", "p75", "p95",
      "distribution_flag", "flag_reason"
    ) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]])

  stats_tbl
}


#' Plot distribution shape diagnostics
#'
#' Returns a named list of three plots summarising the distribution shape
#' screening results.
#'
#' @param distribution_table Data frame returned by [compute_distributions()].
#' @param student_results_long A long-format student results data frame (used
#'   to draw kernel density plots for flagged cells).
#' @param assessment_spec Named list describing the assessment program (used for
#'   subject labels and LOSS/HOSS boundaries).
#'
#' @return A named list with:
#'   * `$flag_summary` — bar chart of flagged cell counts by subject and flag type;
#'     `NULL` if no cells are flagged
#'   * `$shape_heatmap` — heatmap of `bimodality_coefficient` by grade × year,
#'     faceted by subject
#'   * `$density_flagged` — named list of kernel density plots for flagged cells
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_distributions(demo_data, assessment_spec)
#' plots <- plot_distributions(tbl, demo_data, assessment_spec)
#' plots$shape_heatmap
#' }
plot_distributions <- function(distribution_table,
                                student_results_long,
                                assessment_spec) {
  stopifnot(
    is.data.frame(distribution_table),
    is.data.frame(student_results_long),
    is.list(assessment_spec)
  )

  subj_labels <- vapply(
    names(assessment_spec$subjects),
    function(s) assessment_spec$subjects[[s]]$label,
    character(1)
  )

  empty_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::labs(title = msg) +
      ggplot2::theme_minimal()
  }

  # --- $flag_summary ---
  flagged <- distribution_table |>
    dplyr::filter(.data[["distribution_flag"]] != "None")

  flag_summary_plot <- if (nrow(flagged) == 0) {
    message("plot_distributions: no flagged cells -- $flag_summary is NULL")
    NULL
  } else {
    flag_counts <- flagged |>
      dplyr::mutate(
        subj_label = dplyr::recode(
          .data[["SUBJECT"]], !!!subj_labels, .default = .data[["SUBJECT"]]
        )
      ) |>
      dplyr::count(.data[["subj_label"]], .data[["distribution_flag"]])

    ggplot2::ggplot(
      flag_counts,
      ggplot2::aes(
        x    = .data[["subj_label"]],
        y    = .data[["n"]],
        fill = .data[["distribution_flag"]]
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(
        values = c("Bimodal" = "#f6b26b",
                   "Skewed"  = "#fce8b2",
                   "Flat"    = "#9fc5e8"),
        name   = "Flag"
      ) +
      ggplot2::labs(
        title = "Count of Flagged Distribution Cells by Subject",
        x     = NULL, y = "Number of cells"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "right")
  }

  # --- $shape_heatmap ---
  heatmap_tbl <- if (nrow(distribution_table) == 0) {
    distribution_table
  } else {
    distribution_table |>
      dplyr::mutate(
        grade_lbl  = dplyr::if_else(
          is.na(.data[["GRADE"]]), "EOC", paste0("Grade ", .data[["GRADE"]])
        ),
        YEAR_chr   = as.character(as.integer(.data[["YEAR"]])),
        subj_label = dplyr::recode(
          .data[["SUBJECT"]], !!!subj_labels, .default = .data[["SUBJECT"]]
        )
      )
  }

  shape_heatmap <- if (nrow(heatmap_tbl) == 0) {
    empty_plot("No distribution data available")
  } else {
    ggplot2::ggplot(
      heatmap_tbl,
      ggplot2::aes(
        x    = .data[["YEAR_chr"]],
        y    = .data[["grade_lbl"]],
        fill = .data[["bimodality_coefficient"]]
      )
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(
            is.na(.data[["bimodality_coefficient"]]),
            "NA",
            sprintf("%.2f", .data[["bimodality_coefficient"]])
          )
        ),
        size = 3
      ) +
      ggplot2::facet_wrap(~ subj_label, ncol = 1, scales = "free_x") +
      ggplot2::scale_fill_gradient2(
        low      = "#1a9850",
        mid      = "#ffffbf",
        high     = "#d73027",
        midpoint = 0.555,
        limits   = c(0, NA),
        na.value = "grey80",
        name     = "Bimodality\nCoefficient"
      ) +
      ggplot2::labs(
        title = "Bimodality Coefficient by Year and Grade",
        x     = "Year", y = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid  = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  # --- $density_flagged ---
  density_list <- list()

  if (nrow(flagged) > 0) {
    lh_lookup <- build_loss_hoss_lookup(assessment_spec)

    for (i in seq_len(nrow(flagged))) {
      row   <- flagged[i, ]
      subj  <- row$SUBJECT
      grade <- row$GRADE
      yr    <- as.integer(row$YEAR)
      flag  <- row$distribution_flag
      rsn   <- row$flag_reason

      cell_data <- student_results_long |>
        dplyr::filter(
          .data[["SUBJECT"]] == subj,
          .data[["YEAR"]]    == yr,
          !is.na(.data[["SCALE_SCORE"]])
        )

      if (!is.na(grade)) {
        cell_data <- cell_data |>
          dplyr::filter(.data[["GRADE"]] == grade)
      }

      if (nrow(cell_data) == 0) next

      lh <- lh_lookup |>
        dplyr::filter(
          .data[["SUBJECT"]] == subj,
          if (is.na(grade)) is.na(.data[["GRADE"]])
          else (!is.na(.data[["GRADE"]]) & .data[["GRADE"]] == grade)
        )

      grade_lbl <- if (is.na(grade)) "EOC" else paste0("Grade ", grade)
      plot_key  <- paste0(subj, "_", grade_lbl, "_", yr)
      subj_lbl  <- dplyr::recode(subj, !!!subj_labels, .default = subj)

      p <- ggplot2::ggplot(
        cell_data,
        ggplot2::aes(x = .data[["SCALE_SCORE"]])
      ) +
        ggplot2::geom_density(fill = "#cfe2f3", color = "#3c78d8", alpha = 0.7) +
        ggplot2::labs(
          title    = paste0(subj_lbl, " ", grade_lbl, " (", yr, ")"),
          subtitle = paste0("Flag: ", flag, " \u2014 ", rsn),
          x        = "Scale Score", y = "Density"
        ) +
        ggplot2::theme_minimal(base_size = 11)

      if (nrow(lh) > 0) {
        p <- p +
          ggplot2::geom_vline(
            xintercept = lh$LOSS[1], linetype = "dashed",
            color = "#cc0000", linewidth = 0.7
          ) +
          ggplot2::geom_vline(
            xintercept = lh$HOSS[1], linetype = "dashed",
            color = "#cc0000", linewidth = 0.7
          ) +
          ggplot2::annotate(
            "text", x = lh$LOSS[1], y = Inf,
            label = "LOSS", vjust = 1.5, hjust = -0.1,
            size = 3, color = "#cc0000"
          ) +
          ggplot2::annotate(
            "text", x = lh$HOSS[1], y = Inf,
            label = "HOSS", vjust = 1.5, hjust = 1.1,
            size = 3, color = "#cc0000"
          )
      }

      density_list[[plot_key]] <- p
    }
  }

  list(
    flag_summary    = flag_summary_plot,
    shape_heatmap   = shape_heatmap,
    density_flagged = density_list
  )
}


#' Summarize distribution shape findings
#'
#' Returns a narrative, by-subject summary, and the full distribution table
#' describing any unusual score distribution shapes detected.
#'
#' @param distribution_table Data frame returned by [compute_distributions()].
#'
#' @return A named list with `$table` (the full distribution table),
#'   `$by_subject` (flagged cell counts by SUBJECT and flag type), and
#'   `$narrative` (character vector of 1–3 sentences).
#' @export
#' @examples
#' \dontrun{
#' tbl <- compute_distributions(demo_data, assessment_spec)
#' summarize_distributions(tbl)
#' }
summarize_distributions <- function(distribution_table) {
  stopifnot(is.data.frame(distribution_table))

  empty_by_subject <- data.frame(
    SUBJECT           = character(0),
    distribution_flag = character(0),
    n_cells           = integer(0),
    stringsAsFactors  = FALSE
  )

  if (nrow(distribution_table) == 0 ||
      !("distribution_flag" %in% names(distribution_table))) {
    return(list(
      table      = distribution_table,
      by_subject = empty_by_subject,
      narrative  = "No distribution data were available."
    ))
  }

  flagged <- distribution_table |>
    dplyr::filter(.data[["distribution_flag"]] != "None")

  by_subject <- flagged |>
    dplyr::count(.data[["SUBJECT"]], .data[["distribution_flag"]],
                 name = "n_cells")

  if (nrow(flagged) == 0) {
    return(list(
      table      = distribution_table,
      by_subject = by_subject,
      narrative  = paste0(
        "All year-subject-grade combinations had scale score distributions ",
        "within normal range: no flat, bimodal, or highly skewed distributions ",
        "were detected."
      )
    ))
  }

  n_total   <- nrow(flagged)
  n_bimodal <- sum(flagged$distribution_flag == "Bimodal", na.rm = TRUE)
  n_skewed  <- sum(flagged$distribution_flag == "Skewed",  na.rm = TRUE)
  n_flat    <- sum(flagged$distribution_flag == "Flat",    na.rm = TRUE)

  sent1 <- sprintf(
    "%d year-subject-grade combination%s showed unusual score distributions: ",
    n_total, if (n_total == 1L) "" else "s"
  )
  parts <- character(0)
  if (n_bimodal > 0) parts <- c(parts, sprintf("%d Bimodal", n_bimodal))
  if (n_skewed  > 0) parts <- c(parts, sprintf("%d Skewed",  n_skewed))
  if (n_flat    > 0) parts <- c(parts, sprintf("%d Flat",    n_flat))
  sent1 <- paste0(sent1, paste(parts, collapse = ", "), ".")

  # Sentence 2 — list the flagged cells
  cell_lbls <- flagged |>
    dplyr::mutate(
      lbl = paste0(
        .data[["SUBJECT"]], " Grade ", .data[["GRADE"]],
        " (", as.integer(.data[["YEAR"]]), "): ",
        .data[["distribution_flag"]], " \u2014 ", .data[["flag_reason"]]
      )
    ) |>
    dplyr::pull("lbl")

  sent2 <- paste0(
    "Flagged combinations: ",
    paste(cell_lbls, collapse = "; "), "."
  )

  list(
    table      = distribution_table,
    by_subject = by_subject,
    narrative  = c(sent1, sent2)
  )
}


#' Run the distribution sub-analysis and return the results sub-list
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size for suppression. Default `10`.
#'
#' @return A named list with `$table`, `$plot` (list of `$flag_summary`,
#'   `$shape_heatmap`, `$density_flagged`), and `$text` (list of `$table`,
#'   `$by_subject`, `$narrative`).
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' sub <- run_distributions(demo_data, assessment_spec)
#' }
run_distributions <- function(student_results_long,
                              assessment_spec,
                              min_n = 10) {
  tbl <- compute_distributions(student_results_long, assessment_spec,
                                min_n = min_n)
  list(
    table = tbl,
    plot  = plot_distributions(tbl, student_results_long, assessment_spec),
    text  = summarize_distributions(tbl)
  )
}
