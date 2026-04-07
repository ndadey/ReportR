# ==============================================================================
# Line 4 -- Score Quality Analysis: Score Integrity Checks
#
# Two related data integrity checks on scale scores and achievement levels:
#   Check 1 -- Invalid scores: SCALE_SCORE outside the LOSS/HOSS range
#   Check 2 -- Level mismatches: recorded ACHIEVEMENT_LEVEL does not match
#             the level predicted by the spec cutpoints for the student's score
#
# Functions:
#   compute_score_integrity()   -- run both checks; return summary + records
#   plot_score_integrity()      -- bar chart (invalid) + heatmap (mismatch)
#   summarize_score_integrity() -- narrative + flagged-rows table
#   run_score_integrity()       -- entry point; returns the sub-list
# ==============================================================================


# ==============================================================================
# 1. Compute
# ==============================================================================

#' Compute score integrity checks: invalid scores and level mismatches
#'
#' Runs two checks against `assessment_spec`:
#'
#' **Check 1 -- Invalid scores**: flags any `SCALE_SCORE` below `LOSS` or above
#' `HOSS` for its subject/grade combination.
#'
#' **Check 2 -- Level mismatches**: for scores that passed Check 1, derives the
#' expected achievement level from the `cuts` vector
#' (`sum(score > cuts) + 1` gives the level index) and compares it to the
#' recorded `ACHIEVEMENT_LEVEL`. Rows where the recorded level is not in
#' `assessment_spec$achievement_levels$labels` are excluded from the mismatch
#' count and reported separately in the narrative.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Must contain `STUDENT_ID`, `YEAR`, `SUBJECT`,
#'   `GRADE`, `SCALE_SCORE`, and `ACHIEVEMENT_LEVEL`.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Groups with fewer than `min_n` non-missing scores
#'   have `pct_invalid` and `pct_mismatch` set to `NA`. Default `10`.
#'
#' @return A named list with three elements:
#'   * `$summary` -- one row per `YEAR` x `SUBJECT` x `GRADE` with columns
#'     `n_total`, `n_below_loss`, `n_above_hoss`, `n_invalid`, `pct_invalid`,
#'     `n_mismatch`, `pct_mismatch`, `n_unknown_level`, `has_any_issue`.
#'   * `$invalid_records` -- individual rows with invalid scores; columns
#'     `STUDENT_ID`, `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, `LOSS`,
#'     `HOSS`, `direction`. Empty data frame if none found.
#'   * `$mismatch_records` -- individual rows with level mismatches (valid
#'     scores only); columns `STUDENT_ID`, `YEAR`, `SUBJECT`, `GRADE`,
#'     `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, `EXPECTED_LEVEL`. Empty data frame
#'     if none found.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' result <- compute_score_integrity(demo_data, assessment_spec)
#' result$summary
#' }
compute_score_integrity <- function(student_results_long,
                                    assessment_spec,
                                    min_n = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    !is.null(assessment_spec$subjects),
    !is.null(assessment_spec$scale_scores),
    !is.null(assessment_spec$achievement_levels$labels),
    all(c("STUDENT_ID", "YEAR", "SUBJECT", "GRADE",
          "SCALE_SCORE", "ACHIEVEMENT_LEVEL") %in%
          names(student_results_long))
  )

  valid_labels <- assessment_spec$achievement_levels$labels

  # ----------------------------------------------------------
  # 1) Join LOSS/HOSS/cuts to each student row
  # ----------------------------------------------------------
  lookup <- build_loss_hoss_lookup(assessment_spec)

  # Build cuts lookup separately (build_loss_hoss_lookup only returns LOSS/HOSS)
  cuts_rows <- lapply(names(assessment_spec$scale_scores), function(key) {
    ss    <- assessment_spec$scale_scores[[key]]
    parts <- strsplit(key, "_")[[1]]
    # Key is either "SUBJECT" (EOC) or "SUBJECT_GRADE"
    # Use numeric check on last part to distinguish
    last  <- parts[length(parts)]
    if (!is.na(suppressWarnings(as.integer(last))) && length(parts) > 1) {
      subj  <- paste(parts[-length(parts)], collapse = "_")
      grade <- as.integer(last)
    } else {
      subj  <- key
      grade <- NA_integer_
    }
    data.frame(
      SUBJECT = subj,
      GRADE   = grade,
      CUTS    = I(list(ss$cuts)),
      stringsAsFactors = FALSE
    )
  })
  cuts_lookup <- do.call(rbind, cuts_rows)

  dat <- student_results_long |>
    dplyr::filter(!is.na(.data[["SCALE_SCORE"]])) |>
    dplyr::mutate(GRADE = as.integer(.data[["GRADE"]])) |>
    dplyr::left_join(lookup,      by = c("SUBJECT", "GRADE")) |>
    dplyr::left_join(cuts_lookup, by = c("SUBJECT", "GRADE"))

  # ----------------------------------------------------------
  # 2) Check 1 -- invalid scores
  # ----------------------------------------------------------
  invalid_records <- dat |>
    dplyr::filter(
      !is.na(.data[["LOSS"]]),
      .data[["SCALE_SCORE"]] < .data[["LOSS"]] |
        .data[["SCALE_SCORE"]] > .data[["HOSS"]]
    ) |>
    dplyr::mutate(
      direction = dplyr::if_else(
        .data[["SCALE_SCORE"]] < .data[["LOSS"]],
        "below_loss", "above_hoss"
      )
    ) |>
    dplyr::select(
      "STUDENT_ID", "YEAR", "SUBJECT", "GRADE",
      "SCALE_SCORE", "LOSS", "HOSS", "direction"
    )

  # ----------------------------------------------------------
  # 3) Check 2 -- level mismatches (valid scores only)
  # ----------------------------------------------------------
  valid_scores <- dat |>
    dplyr::filter(
      !is.na(.data[["LOSS"]]),
      .data[["SCALE_SCORE"]] >= .data[["LOSS"]],
      .data[["SCALE_SCORE"]] <= .data[["HOSS"]],
      !is.na(.data[["ACHIEVEMENT_LEVEL"]])
    ) |>
    dplyr::mutate(
      EXPECTED_LEVEL = mapply(
        function(score, cuts) {
          if (is.null(cuts) || length(cuts) == 0) return(NA_character_)
          idx <- sum(score > cuts) + 1L
          if (idx < 1L || idx > length(valid_labels)) return(NA_character_)
          valid_labels[idx]
        },
        .data[["SCALE_SCORE"]],
        .data[["CUTS"]],
        SIMPLIFY = TRUE
      )
    )

  # Rows with unknown recorded level (not in spec labels)
  unknown_level_rows <- valid_scores |>
    dplyr::filter(!(.data[["ACHIEVEMENT_LEVEL"]] %in% valid_labels))

  mismatch_records <- valid_scores |>
    dplyr::filter(
      .data[["ACHIEVEMENT_LEVEL"]] %in% valid_labels,
      !is.na(.data[["EXPECTED_LEVEL"]]),
      .data[["ACHIEVEMENT_LEVEL"]] != .data[["EXPECTED_LEVEL"]]
    ) |>
    dplyr::select(
      "STUDENT_ID", "YEAR", "SUBJECT", "GRADE",
      "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "EXPECTED_LEVEL"
    )

  # ----------------------------------------------------------
  # 4) Summary table: one row per YEAR x SUBJECT x GRADE
  # ----------------------------------------------------------
  base_counts <- dat |>
    dplyr::group_by(.data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::summarize(
      n_total     = dplyr::n(),
      n_below_loss = sum(!is.na(.data[["LOSS"]]) &
                           .data[["SCALE_SCORE"]] < .data[["LOSS"]],
                         na.rm = TRUE),
      n_above_hoss = sum(!is.na(.data[["HOSS"]]) &
                           .data[["SCALE_SCORE"]] > .data[["HOSS"]],
                         na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_invalid   = .data[["n_below_loss"]] + .data[["n_above_hoss"]],
      pct_invalid = dplyr::if_else(
        .data[["n_total"]] >= min_n,
        100 * .data[["n_invalid"]] / .data[["n_total"]],
        NA_real_
      )
    )

  mismatch_counts <- mismatch_records |>
    dplyr::group_by(.data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::summarize(n_mismatch = dplyr::n(), .groups = "drop")

  unknown_counts <- unknown_level_rows |>
    dplyr::group_by(.data[["YEAR"]], .data[["SUBJECT"]], .data[["GRADE"]]) |>
    dplyr::summarize(n_unknown_level = dplyr::n(), .groups = "drop")

  summary_tbl <- base_counts |>
    dplyr::left_join(mismatch_counts, by = c("YEAR", "SUBJECT", "GRADE")) |>
    dplyr::left_join(unknown_counts,  by = c("YEAR", "SUBJECT", "GRADE")) |>
    dplyr::mutate(
      n_mismatch      = tidyr::replace_na(.data[["n_mismatch"]],      0L),
      n_unknown_level = tidyr::replace_na(.data[["n_unknown_level"]], 0L),
      pct_mismatch    = dplyr::if_else(
        .data[["n_total"]] >= min_n,
        100 * .data[["n_mismatch"]] / .data[["n_total"]],
        NA_real_
      ),
      has_any_issue = .data[["n_invalid"]] > 0L | .data[["n_mismatch"]] > 0L
    ) |>
    dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]])

  # ----------------------------------------------------------
  # 5) Return
  # ----------------------------------------------------------
  empty_invalid <- data.frame(
    STUDENT_ID = character(0), YEAR = integer(0),
    SUBJECT = character(0),    GRADE = integer(0),
    SCALE_SCORE = numeric(0),  LOSS = numeric(0),
    HOSS = numeric(0),         direction = character(0),
    stringsAsFactors = FALSE
  )
  empty_mismatch <- data.frame(
    STUDENT_ID = character(0),       YEAR = integer(0),
    SUBJECT = character(0),          GRADE = integer(0),
    SCALE_SCORE = numeric(0),        ACHIEVEMENT_LEVEL = character(0),
    EXPECTED_LEVEL = character(0),
    stringsAsFactors = FALSE
  )

  list(
    summary         = summary_tbl,
    invalid_records = if (nrow(invalid_records)  > 0) invalid_records  else empty_invalid,
    mismatch_records= if (nrow(mismatch_records) > 0) mismatch_records else empty_mismatch
  )
}


# ==============================================================================
# 2. Plot
# ==============================================================================

#' Plot score integrity results
#'
#' Produces two diagnostic plots from the output of [compute_score_integrity()]:
#' a bar chart of invalid score counts and a heatmap of mismatch rates.
#'
#' @param score_integrity_result Named list returned by
#'   [compute_score_integrity()].
#'
#' @return A named list with:
#'   * `$invalid` -- bar chart of `n_invalid` by subject and grade, faceted
#'     by year. `NULL` (with message) if no invalid scores exist.
#'   * `$mismatch` -- heatmap of `pct_mismatch` with year on x-axis, grade on
#'     y-axis, faceted by subject. Grey cells = no mismatches. `NULL` (with
#'     message) if no mismatches exist.
#' @export
#' @examples
#' \dontrun{
#' result <- compute_score_integrity(demo_data, assessment_spec)
#' plots  <- plot_score_integrity(result)
#' plots$mismatch
#' }
plot_score_integrity <- function(score_integrity_result) {
  stopifnot(
    is.list(score_integrity_result),
    all(c("summary", "invalid_records", "mismatch_records") %in%
          names(score_integrity_result))
  )

  smry <- score_integrity_result$summary

  # --- Invalid scores bar chart ---
  if (sum(smry$n_invalid, na.rm = TRUE) == 0) {
    p_invalid <- NULL
    message("plot_score_integrity: no invalid scores found; $invalid is NULL")
  } else {
    p_invalid <- smry |>
      dplyr::filter(.data[["n_invalid"]] > 0) |>
      dplyr::mutate(
        grade_lbl = dplyr::if_else(
          is.na(.data[["GRADE"]]), "EOC", paste0("Gr ", .data[["GRADE"]])
        )
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        x    = .data[["grade_lbl"]],
        y    = .data[["n_invalid"]],
        fill = .data[["SUBJECT"]]
      )) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ YEAR) +
      ggplot2::labs(
        title = "Invalid Scale Scores by Subject, Grade, and Year",
        x     = "Grade",
        y     = "Count of Invalid Scores",
        fill  = "Subject"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }

  # --- Mismatch heatmap ---
  if (sum(smry$n_mismatch, na.rm = TRUE) == 0) {
    p_mismatch <- NULL
    message("plot_score_integrity: no level mismatches found; $mismatch is NULL")
  } else {
    p_mismatch <- smry |>
      dplyr::mutate(
        grade_lbl = dplyr::if_else(
          is.na(.data[["GRADE"]]), "EOC", paste0("Gr ", .data[["GRADE"]])
        )
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        x    = factor(.data[["YEAR"]]),
        y    = .data[["grade_lbl"]],
        fill = .data[["pct_mismatch"]]
      )) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::facet_wrap(~ SUBJECT) +
      ggplot2::scale_fill_viridis_c(
        option   = "plasma",
        na.value = "grey90",
        name     = "% Mismatch"
      ) +
      ggplot2::labs(
        title = "Achievement Level Mismatch Rate by Subject, Grade, and Year",
        x     = "Year",
        y     = "Grade"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }

  list(invalid = p_invalid, mismatch = p_mismatch)
}


# ==============================================================================
# 3. Summarize
# ==============================================================================

#' Summarize score integrity findings
#'
#' Produces a short narrative and a table filtered to rows with issues.
#'
#' @param score_integrity_result Named list returned by
#'   [compute_score_integrity()].
#'
#' @return A named list with:
#'   * `$narrative` -- character vector of 2-3 narrative sentences.
#'   * `$table` -- summary data frame filtered to rows where
#'     `has_any_issue == TRUE`. If no issues exist, returns the full summary
#'     with an attribute `note` explaining that all checks passed.
#' @export
#' @examples
#' \dontrun{
#' result <- compute_score_integrity(demo_data, assessment_spec)
#' txt    <- summarize_score_integrity(result)
#' cat(txt$narrative, sep = "\n\n")
#' }
summarize_score_integrity <- function(score_integrity_result) {
  stopifnot(
    is.list(score_integrity_result),
    all(c("summary", "invalid_records", "mismatch_records") %in%
          names(score_integrity_result))
  )

  smry      <- score_integrity_result$summary
  n_invalid <- sum(smry$n_invalid,       na.rm = TRUE)
  n_mismatch<- sum(smry$n_mismatch,      na.rm = TRUE)
  n_unknown <- sum(smry$n_unknown_level, na.rm = TRUE)

  if (n_invalid == 0 && n_mismatch == 0 && n_unknown == 0) {
    narrative <- paste0(
      "All scale scores fall within the valid LOSS\u2013HOSS range defined in ",
      "the assessment specification, and all recorded achievement levels are ",
      "consistent with the cutpoints for their respective subject and grade. ",
      "No data integrity issues were identified."
    )
    tbl <- smry
    attr(tbl, "note") <- "All score integrity checks passed -- no issues found."
    return(list(narrative = narrative, table = tbl))
  }

  # --- Invalid scores sentence ---
  if (n_invalid > 0) {
    n_below <- sum(smry$n_below_loss, na.rm = TRUE)
    n_above <- sum(smry$n_above_hoss, na.rm = TRUE)
    dir_str <- dplyr::case_when(
      n_below > 0 & n_above > 0 ~
        paste0(n_below, " below the LOSS and ", n_above, " above the HOSS"),
      n_below > 0 ~ paste0(n_below, " below the LOSS"),
      TRUE        ~ paste0(n_above, " above the HOSS")
    )
    sent_invalid <- paste0(
      n_invalid, " score", if (n_invalid != 1) "s" else "",
      " outside the valid scale score range were identified (",
      dir_str, "). These likely represent data entry or processing errors ",
      "and should be investigated before trend results are interpreted."
    )
  } else {
    sent_invalid <- paste0(
      "All scale scores fall within the valid LOSS\u2013HOSS range defined ",
      "in the assessment specification."
    )
  }

  # --- Mismatch sentence ---
  if (n_mismatch > 0) {
    top_cells <- smry |>
      dplyr::filter(.data[["n_mismatch"]] > 0) |>
      dplyr::arrange(dplyr::desc(.data[["pct_mismatch"]])) |>
      utils::head(3) |>
      dplyr::mutate(
        cell_lbl = paste0(
          .data[["SUBJECT"]],
          dplyr::if_else(is.na(.data[["GRADE"]]), "",
                         paste0(" Grade ", .data[["GRADE"]])),
          " (", round(.data[["pct_mismatch"]], 1), "%)"
        )
      ) |>
      dplyr::pull("cell_lbl") |>
      paste(collapse = "; ")

    sent_mismatch <- paste0(
      n_mismatch, " record", if (n_mismatch != 1) "s" else "",
      " had an achievement level that did not match the level predicted by ",
      "the assessment specification cutpoints. The highest mismatch rates ",
      "were observed in: ", top_cells, "."
    )
  } else {
    sent_mismatch <- paste0(
      "All recorded achievement levels are consistent with the cutpoints ",
      "defined in the assessment specification."
    )
  }

  # --- Unknown levels sentence ---
  sent_unknown <- if (n_unknown > 0) {
    paste0(
      "Additionally, ", n_unknown, " record",
      if (n_unknown != 1) "s" else "",
      " contained an achievement level value not present in the specification ",
      "labels and were excluded from the mismatch check."
    )
  } else {
    NULL
  }

  narrative <- c(sent_invalid, sent_mismatch, sent_unknown)
  narrative <- narrative[!is.null(narrative)]

  flagged_tbl <- smry |> dplyr::filter(.data[["has_any_issue"]])

  list(narrative = narrative, table = flagged_tbl)
}


# ==============================================================================
# 4. Sub-list entry point
# ==============================================================================

#' Run the score integrity sub-analysis and return the results sub-list
#'
#' Orchestrates [compute_score_integrity()], [plot_score_integrity()], and
#' [summarize_score_integrity()] into the standardised sub-list consumed by
#' [run_anomaly_analysis()].
#'
#' @param student_results_long A long-format student results data frame.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Minimum group size for suppression. Default `10`.
#'
#' @return A named list with:
#'   `$table`, `$invalid_records`, `$mismatch_records`, `$plot`, `$text`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' sub <- run_score_integrity(demo_data, assessment_spec)
#' sub$text$narrative
#' }
run_score_integrity <- function(student_results_long,
                                assessment_spec,
                                min_n = 10) {
  computed <- compute_score_integrity(
    student_results_long = student_results_long,
    assessment_spec      = assessment_spec,
    min_n                = min_n
  )
  list(
    table            = computed$summary,
    invalid_records  = computed$invalid_records,
    mismatch_records = computed$mismatch_records,
    plot             = plot_score_integrity(computed),
    text             = summarize_score_integrity(computed)
  )
}
