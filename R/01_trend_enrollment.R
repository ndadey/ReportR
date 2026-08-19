# ==============================================================================
# Line 1.2 — General Trend Analysis: Enrollment Trends
#
# All functions follow the compute/plot/summarize/run pattern established
# in 01_trend_overall.R, 01_trend_proficiency.R, and 01_trend_distributions.R.
#
# Functions:
#   compute_enrollment_counts()          — n-size tables by arbitrary dimensions
#   compute_enrollment_proportions()     — subgroup proportions with denominator control
#   compute_subgroup_transitions()       — year-over-year composition shifts
#   compute_entry_exit_performance()     — performance of entering/exiting students
#   compute_school_district_enrollment() — school- or district-level summaries
#   plot_enrollment_sankey()             — alluvial/Sankey diagram (requires ggalluvial)
#   plot_enrollment_table()              — flextable-formatted output
#   summarize_enrollment()              — narrative text
#   run_enrollment_analysis()            — orchestrator
# ==============================================================================


# ==============================================================================
# compute_enrollment_counts()
# ==============================================================================

#' Count enrolled students by arbitrary grouping dimensions
#'
#' Deduplicates records to unique student-year combinations (or
#' student-year-subject when `SUBJECT` is in `by`) before counting, so
#' students tested in multiple subjects in the same year are not double-counted
#' in cross-sectional tallies.
#'
#' @param data A long-format student results data frame (see data contract).
#'   Must contain `STUDENT_ID` and `YEAR`.
#' @param by Character vector of grouping column names, or `NULL` for an
#'   overall total. Supported values: `"YEAR"`, `"GRADE"`, `"SUBJECT"`,
#'   demographic column names (e.g. `"RACE_ETHNICITY"`, `"FRL"`),
#'   `"SCHOOL_ID"`, `"DISTRICT_ID"`, and their `_NAME` equivalents.
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. Year-over-year change columns are suppressed (set to
#'   `NA`) for rows where `N < min_n`. Default `10`.
#'
#' @return A data frame with the columns in `by` plus:
#'   * `N` — count of unique student-year (or student-year-subject) records
#'   * `N_change` — year-over-year change in `N` (only when `YEAR` is in `by`)
#'   * `N_pct_change` — percent change (only when `YEAR` is in `by`)
#'   * `suppressed` — logical; `TRUE` when `N < min_n`
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' compute_enrollment_counts(demo_data, by = c("YEAR", "GRADE"), assessment_spec)
#' }
compute_enrollment_counts <- function(data,
                                      by              = NULL,
                                      assessment_spec,
                                      min_n           = 10) {
  stopifnot(
    is.data.frame(data),
    is.null(by) || is.character(by),
    is.list(assessment_spec),
    "STUDENT_ID" %in% names(data),
    "YEAR" %in% names(data)
  )

  # Columns that vary at the test level (within student-year).
  # If these appear in `by`, they are included in the deduplication key so
  # students are counted once per subject/grade rather than collapsed across.
  test_level_cols <- c("SUBJECT", "GRADE")

  by_eff <- if (is.null(by)) character(0) else intersect(by, names(data))

  dedup_key  <- unique(c("STUDENT_ID", "YEAR", intersect(by_eff, test_level_cols)))
  all_needed <- unique(c(dedup_key, by_eff))
  avail_cols <- intersect(all_needed, names(data))

  data_dedup <- data |>
    dplyr::select(dplyr::all_of(avail_cols)) |>
    dplyr::distinct()

  group_vars <- intersect(by_eff, names(data_dedup))

  if (length(group_vars) == 0) {
    counts <- data.frame(N = nrow(data_dedup), stringsAsFactors = FALSE)
  } else {
    counts <- data_dedup |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarize(N = dplyr::n(), .groups = "drop")
  }

  # Year-over-year change columns when YEAR is a grouping variable
  if ("YEAR" %in% group_vars) {
    other_vars <- setdiff(group_vars, "YEAR")

    counts <- counts |>
      dplyr::arrange(
        dplyr::across(dplyr::all_of(other_vars)),
        .data[["YEAR"]]
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(other_vars))) |>
      dplyr::mutate(
        N_change = .data[["N"]] - dplyr::lag(.data[["N"]]),
        N_pct_change = dplyr::if_else(
          !is.na(dplyr::lag(.data[["N"]])) & dplyr::lag(.data[["N"]]) > 0,
          100 * (.data[["N"]] - dplyr::lag(.data[["N"]])) /
            dplyr::lag(.data[["N"]]),
          NA_real_
        )
      ) |>
      dplyr::ungroup()

    # Suppress change columns for small groups; N itself is always reported
    counts <- suppress_small_n_enroll(
      counts, "N", min_n, c("N_change", "N_pct_change")
    )
  } else {
    counts[["suppressed"]] <- !is.na(counts[["N"]]) & counts[["N"]] < min_n
  }

  counts
}


# ==============================================================================
# compute_enrollment_proportions()
# ==============================================================================

#' Compute subgroup proportions within an enrollment grouping
#'
#' For each variable in `subgroup_vars`, computes the proportion of students
#' in each category within the specified `by` grouping. Returns a long-format
#' data frame with one row per `by`-group × subgroup variable × subgroup value.
#'
#' @param data A long-format student results data frame (see data contract).
#' @param by Character vector of grouping column names (e.g. `c("YEAR",
#'   "GRADE")`).
#' @param subgroup_vars Character vector of demographic column names to compute
#'   proportions for. Defaults to `names(assessment_spec$demographics)`.
#' @param assessment_spec Named list describing the assessment program.
#' @param denominator Character scalar controlling what counts as the total
#'   enrollment for proportion calculation. One of:
#'   * `"total_by_year"` — all students in that year (default)
#'   * `"total_overall"` — all students across all years
#'   * `"within_grade"` — students in that year × grade
#'   * `"within_school"` — students in that year × school
#'   * `"within_district"` — students in that year × district
#' @param min_n Integer. Cells with `N < min_n` have their `pct` set to `NA`.
#'   Default `10`.
#'
#' @return A long-format data frame with columns: `by` grouping columns,
#'   `subgroup_var` (the demographic variable name), `subgroup_val` (the
#'   category), `N` (count), `total_N` (denominator), `pct` (proportion ×
#'   100), `suppressed`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' compute_enrollment_proportions(demo_data, by = "YEAR", assessment_spec)
#' }
compute_enrollment_proportions <- function(data,
                                            by,
                                            subgroup_vars = NULL,
                                            assessment_spec,
                                            denominator   = "total_by_year",
                                            min_n         = 10) {
  stopifnot(
    is.data.frame(data),
    is.character(by),
    is.list(assessment_spec),
    is.character(denominator), length(denominator) == 1L,
    denominator %in% c("total_by_year", "total_overall",
                        "within_grade", "within_school", "within_district")
  )

  if (is.null(subgroup_vars)) {
    subgroup_vars <- names(assessment_spec$demographics)
  }

  subgroup_vars <- intersect(subgroup_vars, names(data))
  by_eff        <- intersect(by, names(data))

  # Deduplication key: student-year grain; add SUBJECT/GRADE if in by
  test_level_cols <- c("SUBJECT", "GRADE")
  dedup_key  <- unique(c("STUDENT_ID", "YEAR", intersect(by_eff, test_level_cols)))
  all_needed <- unique(c(dedup_key, by_eff, subgroup_vars))
  avail_cols <- intersect(all_needed, names(data))

  data_dedup <- data |>
    dplyr::select(dplyr::all_of(avail_cols)) |>
    dplyr::distinct()

  # Determine denominator grouping columns
  denom_vars <- switch(
    denominator,
    "total_by_year"   = intersect("YEAR",        names(data_dedup)),
    "total_overall"   = character(0),
    "within_grade"    = intersect(c("YEAR", "GRADE"),       names(data_dedup)),
    "within_school"   = intersect(c("YEAR", "SCHOOL_ID"),   names(data_dedup)),
    "within_district" = intersect(c("YEAR", "DISTRICT_ID"), names(data_dedup))
  )

  # Total enrollment per denominator group
  total_by_denom <- data_dedup |>
    dplyr::group_by(dplyr::across(dplyr::all_of(denom_vars))) |>
    dplyr::summarize(total_N = dplyr::n(), .groups = "drop")

  if (length(subgroup_vars) == 0) {
    return(data.frame())
  }

  # For each subgroup variable, compute counts and proportions
  results <- lapply(subgroup_vars, function(sg) {
    if (!sg %in% names(data_dedup)) return(NULL)

    grp_vars_raw  <- unique(c(by_eff, sg))
    grp_vars_avail <- intersect(grp_vars_raw, names(data_dedup))
    join_denom    <- intersect(denom_vars, grp_vars_avail)

    d_sg <- data_dedup |>
      dplyr::filter(!is.na(.data[[sg]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars_avail))) |>
      dplyr::summarize(N = dplyr::n(), .groups = "drop") |>
      dplyr::left_join(total_by_denom, by = if (length(join_denom) > 0) join_denom else character(0)) |>
      dplyr::mutate(
        pct = dplyr::if_else(
          .data[["N"]] >= min_n & !is.na(.data[["total_N"]]) &
            .data[["total_N"]] > 0,
          100 * .data[["N"]] / .data[["total_N"]],
          NA_real_
        ),
        subgroup_var = sg,
        subgroup_val = as.character(.data[[sg]]),
        suppressed   = .data[["N"]] < min_n
      ) |>
      dplyr::select(-dplyr::all_of(sg))

    d_sg
  })

  result <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(result)) return(data.frame())

  # Reorder columns for readability
  id_cols    <- unique(c(by_eff, "subgroup_var", "subgroup_val"))
  id_cols    <- intersect(id_cols, names(result))
  stat_cols  <- setdiff(names(result), id_cols)
  result |> dplyr::select(dplyr::all_of(c(id_cols, stat_cols)))
}


# ==============================================================================
# compute_subgroup_transitions()
# ==============================================================================

#' Compute year-over-year subgroup composition shifts
#'
#' Tracks how the proportion of students in each category of a single
#' demographic variable changes across consecutive testing years.
#' When `cohort = TRUE`, the analysis tracks cohorts (grade-aligned year
#' sequences) rather than raw cross-sectional counts.
#'
#' @param data A long-format student results data frame (see data contract).
#' @param subgroup_var Character scalar. Name of the demographic column to
#'   track (e.g. `"RACE_ETHNICITY"`, `"FRL"`).
#' @param assessment_spec Named list describing the assessment program.
#' @param cohort Logical. If `TRUE`, include `GRADE` in the grouping so
#'   year-over-year comparisons are within the same grade level (cohort
#'   tracking). Default `FALSE`.
#' @param min_n Integer. Cells with `N < min_n` have `pct` set to `NA`.
#'   Default `10`.
#'
#' @return A named list:
#'   * `$table` — data frame with `YEAR` (plus `GRADE` when `cohort = TRUE`),
#'     `subgroup_var`, `subgroup_val`, `N`, `total_N`, `pct`, `suppressed`
#'   * `$summary` — data frame of year-over-year percentage-point changes
#'     per subgroup category: `subgroup_val`, `YEAR`, `prior_year`,
#'     `pct_change`
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' compute_subgroup_transitions(demo_data, "RACE_ETHNICITY", assessment_spec)$summary
#' }
compute_subgroup_transitions <- function(data,
                                          subgroup_var,
                                          assessment_spec,
                                          cohort = FALSE,
                                          min_n  = 10) {
  stopifnot(
    is.data.frame(data),
    is.character(subgroup_var), length(subgroup_var) == 1L,
    is.list(assessment_spec),
    is.logical(cohort), length(cohort) == 1L
  )

  if (!subgroup_var %in% names(data)) {
    message(sprintf(
      "compute_subgroup_transitions: '%s' not in data; returning empty list.",
      subgroup_var
    ))
    return(list(table = data.frame(), summary = data.frame()))
  }

  # NOTE: cohort = TRUE adds GRADE to the grouping, enabling within-grade
  # year-over-year comparisons. This is a simple cohort approximation —
  # it does not require STUDENT_ID, so it works even when IDs are absent.
  dedup_key <- if (cohort) c("STUDENT_ID", "YEAR", "GRADE") else c("STUDENT_ID", "YEAR")
  dedup_key <- intersect(dedup_key, names(data))

  avail_cols <- intersect(c(dedup_key, subgroup_var), names(data))
  data_dedup <- data |>
    dplyr::select(dplyr::all_of(avail_cols)) |>
    dplyr::distinct()

  grp_vars <- if (cohort) {
    intersect(c("YEAR", "GRADE", subgroup_var), names(data_dedup))
  } else {
    intersect(c("YEAR", subgroup_var), names(data_dedup))
  }

  tbl <- data_dedup |>
    dplyr::filter(!is.na(.data[[subgroup_var]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) |>
    dplyr::summarize(N = dplyr::n(), .groups = "drop")

  denom_vars <- intersect(c("YEAR", if (cohort) "GRADE"), names(tbl))

  totals <- tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(denom_vars))) |>
    dplyr::summarize(total_N = sum(.data[["N"]]), .groups = "drop")

  tbl <- tbl |>
    dplyr::left_join(totals, by = denom_vars) |>
    dplyr::mutate(
      pct = dplyr::if_else(
        .data[["N"]] >= min_n & .data[["total_N"]] > 0,
        100 * .data[["N"]] / .data[["total_N"]],
        NA_real_
      ),
      suppressed  = .data[["N"]] < min_n,
      subgroup_var = subgroup_var,
      subgroup_val = as.character(.data[[subgroup_var]])
    ) |>
    dplyr::select(-dplyr::all_of(subgroup_var))

  # Year-over-year percentage-point changes per subgroup category
  sort_vars <- intersect(c("subgroup_val", if (cohort) "GRADE"), names(tbl))

  summary_tbl <- tbl |>
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_vars)), .data[["YEAR"]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(sort_vars))) |>
    dplyr::mutate(
      prior_year = dplyr::lag(.data[["YEAR"]]),
      pct_change = .data[["pct"]] - dplyr::lag(.data[["pct"]])
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data[["pct_change"]]))

  list(table = tbl, summary = summary_tbl)
}


# ==============================================================================
# compute_entry_exit_performance()
# ==============================================================================

#' Compute performance of entering and exiting students
#'
#' Identifies students who entered (present in year T but not T-1) or exited
#' (present in year T-1 but not T) the tested population, then computes their
#' mean scale score and percent proficient. Requires `STUDENT_ID` to track
#' students across years.
#'
#' @param data A long-format student results data frame (see data contract).
#'   Must contain `STUDENT_ID`, `YEAR`, `SCALE_SCORE`, and (optionally)
#'   `ACHIEVEMENT_LEVEL`.
#' @param direction Character scalar: `"entering"`, `"exiting"`, or `"both"`.
#'   Default `"entering"`.
#' @param assessment_spec Named list describing the assessment program.
#' @param by Character vector of grouping columns for performance statistics.
#'   Default `c("YEAR", "SUBJECT")`.
#' @param min_n Integer. Cells with fewer than `min_n` students have their
#'   statistics set to `NA`. Default `10`.
#'
#' @return A data frame with columns: `YEAR`, `group` (entering/exiting),
#'   grouping columns from `by`, `N`, `mean_score`, `pct_proficient`,
#'   `suppressed`. Returns an empty data frame if `STUDENT_ID` is absent or
#'   fewer than 2 years are present.
#'
#' @details
#'   Presence is determined at the year level (a student is "present" in year T
#'   if they have at least one record in the data for that year, regardless of
#'   subject). For exiting students, performance statistics come from year T-1
#'   (their last year of testing), but the row is labelled with `YEAR = T` (the
#'   transition year) so that entering and exiting rows align on the same YEAR
#'   axis in output tables.
#'
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' compute_entry_exit_performance(demo_data, direction = "both", assessment_spec)
#' }
compute_entry_exit_performance <- function(data,
                                            direction       = "entering",
                                            assessment_spec,
                                            by              = c("YEAR", "SUBJECT"),
                                            min_n           = 10) {
  stopifnot(
    is.data.frame(data),
    is.character(direction), length(direction) == 1L,
    direction %in% c("entering", "exiting", "both"),
    is.list(assessment_spec),
    is.character(by)
  )

  if (!"STUDENT_ID" %in% names(data)) {
    message("compute_entry_exit_performance: STUDENT_ID not in data; returning empty frame.")
    return(data.frame())
  }

  all_years <- sort(unique(data[["YEAR"]]))
  if (length(all_years) < 2) {
    message("compute_entry_exit_performance: fewer than 2 years in data; returning empty frame.")
    return(data.frame())
  }

  bench_idx   <- benchmark_index(assessment_spec)
  labels      <- assessment_spec$achievement_levels$labels
  prof_labels <- labels[seq(bench_idx, length(labels))]

  has_achievement <- "ACHIEVEMENT_LEVEL" %in% names(data)

  # Year-level presence: unique STUDENT_ID per YEAR (ignoring subject)
  presence <- data |>
    dplyr::select(dplyr::all_of(c("STUDENT_ID", "YEAR"))) |>
    dplyr::distinct()

  by_eff <- intersect(by, names(data))

  # Helper: compute performance stats for a set of student IDs from one year
  compute_perf <- function(ids, data_year, label_year, group_label) {
    if (length(ids) == 0) return(NULL)

    d_grp <- data |>
      dplyr::filter(
        .data[["STUDENT_ID"]] %in% ids,
        .data[["YEAR"]]       == data_year
      )
    if (nrow(d_grp) == 0) return(NULL)

    grp_vars <- intersect(by_eff, names(d_grp))
    # Remove YEAR from grouping — we set it explicitly after summarize
    grp_vars_nosf_yr <- setdiff(grp_vars, "YEAR")

    result <- d_grp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars_nosf_yr))) |>
      dplyr::group_modify(function(chunk, keys) {
        n_total  <- nrow(chunk)
        has_ach  <- "ACHIEVEMENT_LEVEL" %in% names(chunk)
        has_scr  <- "SCALE_SCORE" %in% names(chunk)

        if (n_total >= min_n) {
          mean_sc  <- if (has_scr) mean(chunk[["SCALE_SCORE"]], na.rm = TRUE) else NA_real_
          n_prof   <- if (has_ach) sum(chunk[["ACHIEVEMENT_LEVEL"]] %in% prof_labels,
                                       na.rm = TRUE) else 0L
          pct_prof <- if (has_ach) 100 * n_prof / n_total else NA_real_
        } else {
          mean_sc  <- NA_real_
          pct_prof <- NA_real_
        }

        data.frame(
          N              = n_total,
          mean_score     = mean_sc,
          pct_proficient = pct_prof,
          suppressed     = n_total < min_n,
          stringsAsFactors = FALSE
        )
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        YEAR  = label_year,
        group = group_label
      )

    result
  }

  pair_results <- lapply(seq_along(all_years)[-1], function(i) {
    yr_curr <- all_years[i]
    yr_prev <- all_years[i - 1L]

    ids_curr <- presence[["STUDENT_ID"]][presence[["YEAR"]] == yr_curr]
    ids_prev <- presence[["STUDENT_ID"]][presence[["YEAR"]] == yr_prev]

    parts <- list()
    if (direction %in% c("entering", "both")) {
      entering_ids <- setdiff(ids_curr, ids_prev)
      # NOTE: entering student data comes from yr_curr; YEAR label = yr_curr
      parts$entering <- compute_perf(entering_ids, yr_curr, yr_curr, "Entering")
    }
    if (direction %in% c("exiting", "both")) {
      exiting_ids <- setdiff(ids_prev, ids_curr)
      # NOTE: exiting student performance is from yr_prev but labelled yr_curr
      # so all rows for the same transition period share the same YEAR value
      parts$exiting <- compute_perf(exiting_ids, yr_prev, yr_curr, "Exiting")
    }
    if (direction == "both") {
      continuing_ids <- intersect(ids_curr, ids_prev)
      parts$continuing <- compute_perf(continuing_ids, yr_curr, yr_curr, "Continuing")
    }

    do.call(rbind, Filter(Negate(is.null), parts))
  })

  result <- do.call(rbind, Filter(Negate(is.null), pair_results))
  if (is.null(result) || nrow(result) == 0) return(data.frame())

  # Move YEAR and group to front for readability
  id_cols   <- unique(c("YEAR", "group", setdiff(by_eff, "YEAR")))
  id_cols   <- intersect(id_cols, names(result))
  stat_cols <- setdiff(names(result), id_cols)
  result |> dplyr::select(dplyr::all_of(c(id_cols, stat_cols)))
}


# ==============================================================================
# compute_school_score_summary()
# ==============================================================================

#' Compute school-level mean scale scores and year-over-year changes
#'
#' Aggregates student scale scores to the school × year × subject level and
#' computes year-over-year changes in mean score. Intended to be joined with
#' enrollment change data to examine the relationship between enrollment shifts
#' and score trends.
#'
#' @param data A long-format student results data frame (see data contract).
#'   Must contain `SCALE_SCORE`, `YEAR`, and `SUBJECT`, plus a school ID
#'   column (`SCHOOL_NUMBER` or `SCHOOL_ID`).
#' @param assessment_spec Named list describing the assessment program.
#' @param min_n Integer. School × year × subject cells with fewer than `min_n`
#'   students have their `mean_score` set to `NA`. Default `10`.
#'
#' @return A data frame with columns: school ID, `YEAR`, `SUBJECT`, `N`,
#'   `mean_score`, `delta_mean` (year-over-year change in mean score),
#'   `year_pair` (e.g. `"2023-2024"`), `suppressed`.
#'   Returns an empty data frame if no school ID column is found.
#' @export
compute_school_score_summary <- function(data, assessment_spec, min_n = 10) {
  stopifnot(
    is.data.frame(data),
    is.list(assessment_spec),
    all(c("YEAR", "SUBJECT", "SCALE_SCORE") %in% names(data))
  )

  school_id_col <- c("SCHOOL_NUMBER", "SCHOOL_ID")[
    c("SCHOOL_NUMBER", "SCHOOL_ID") %in% names(data)][1]

  if (is.na(school_id_col)) {
    message("compute_school_score_summary: no school ID column found; returning empty frame.")
    return(data.frame())
  }

  grp_vars <- c(school_id_col, "YEAR", "SUBJECT")

  summary <- data |>
    dplyr::filter(!is.na(.data[["SCALE_SCORE"]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) |>
    dplyr::summarize(
      N          = dplyr::n(),
      mean_score = dplyr::if_else(
        dplyr::n() >= min_n,
        mean(.data[["SCALE_SCORE"]], na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(c(school_id_col, "SUBJECT"))),
      .data[["YEAR"]]
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(school_id_col, "SUBJECT")))) |>
    dplyr::mutate(
      delta_mean = .data[["mean_score"]] - dplyr::lag(.data[["mean_score"]]),
      year_pair  = paste0(dplyr::lag(.data[["YEAR"]]), "–", .data[["YEAR"]])
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      suppressed = .data[["N"]] < min_n,
      delta_mean = dplyr::if_else(.data[["suppressed"]], NA_real_, .data[["delta_mean"]])
    )

  summary
}


# Package-level helper: overall enrollment vs. score scatter (one point per
# year-pair × subject). Defined at package level so ggplot aes() quosures
# do not capture student-level data from the calling environment.
.build_overall_enroll_score_plot <- function(df, prog_name) {
  df <- df |>
    dplyr::mutate(grade_lbl = factor(
      paste0("Gr ", .data[["GRADE"]]),
      levels = paste0("Gr ", sort(unique(as.integer(.data[["GRADE"]]))))
    ))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = N_change, y = delta_mean,
                 color = grade_lbl, shape = year_pair, label = year_pair)
  ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.85, na.rm = TRUE) +
    ggplot2::scale_shape_manual(
      values = c(16, 17, 15, 18, 8, 3, 4, 1, 2, 0)[
        seq_len(length(unique(df$year_pair)))
      ]
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "grey50", linewidth = 0.4) +
    ggplot2::facet_wrap(~ SUBJECT, scales = "free") +
    ggplot2::labs(
      title   = paste("Enrollment Change vs. Mean Score Change —", prog_name),
      x       = "Year-to-Year Change in Enrollment (N)",
      y       = "Year-to-Year Change in Mean Scale Score",
      color   = "Grade",
      shape   = "Year Pair",
      caption = "Each point = one year transition for one subject/grade. Shape = year pair; color = grade. Reference lines at zero."
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}


# ==============================================================================
# compute_school_district_enrollment()
# ==============================================================================

#' Compute school- or district-level enrollment counts and subgroup proportions
#'
#' Aggregates enrollment to the school or district level, computing total
#' enrollment and year-over-year changes. Optionally computes subgroup
#' proportions for each entity.
#'
#' @param data A long-format student results data frame (see data contract).
#' @param level Character scalar: `"school"` or `"district"`.
#' @param assessment_spec Named list describing the assessment program.
#' @param subgroup_vars Character vector of demographic columns to compute
#'   proportions for. Defaults to `names(assessment_spec$demographics)`.
#' @param min_n Integer. Subgroup cells with `N < min_n` have `pct_sg` set
#'   to `NA`. Default `10`.
#'
#' @return A named list:
#'   * `$totals` — data frame: entity ID/name columns, `YEAR`, `N`,
#'     `N_change`, `N_pct_change`
#'   * `$by_subgroup` — long-format data frame: entity columns, `YEAR`,
#'     `subgroup_var`, `subgroup_val`, `N_sg`, `pct_sg`, `suppressed`
#'   Returns an empty list if entity ID columns are not present in the data.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' # sgpData_LONG has no SCHOOL_ID — this returns an empty list for that dataset
#' compute_school_district_enrollment(demo_data, level = "school", assessment_spec)
#' }
compute_school_district_enrollment <- function(data,
                                                level        = "school",
                                                assessment_spec,
                                                subgroup_vars = NULL,
                                                min_n        = 10) {
  stopifnot(
    is.data.frame(data),
    is.character(level), length(level) == 1L, level %in% c("school", "district"),
    is.list(assessment_spec)
  )

  if (is.null(subgroup_vars)) {
    subgroup_vars <- names(assessment_spec$demographics)
  }

  # Accept both raw SGP names (SCHOOL_NUMBER / DISTRICT_NUMBER) and the
  # standardized names produced by standardize_student_results() (SCHOOL_ID /
  # DISTRICT_ID). Use whichever is present; raw names take priority so the
  # function works even when standardization has not been applied.
  id_candidates <- if (level == "school") c("SCHOOL_NUMBER", "SCHOOL_ID") else
                                          c("DISTRICT_NUMBER", "DISTRICT_ID")
  entity_id_col <- id_candidates[id_candidates %in% names(data)][1]

  if (is.na(entity_id_col)) {
    message(sprintf(
      "compute_school_district_enrollment: none of %s found in data; returning empty list.",
      paste(id_candidates, collapse = ", ")
    ))
    return(list(totals = data.frame(), by_subgroup = data.frame()))
  }

  avail_entity <- entity_id_col   # ID only; name columns are excluded to keep
                                  # the dedup key minimal and ID-stable

  # Deduplicate to student × year × entity grain
  dedup_key  <- unique(c("STUDENT_ID", "YEAR", avail_entity))
  avail_sg   <- intersect(subgroup_vars, names(data))
  all_needed <- unique(c(dedup_key, avail_sg))

  data_dedup <- data |>
    dplyr::select(dplyr::all_of(intersect(all_needed, names(data)))) |>
    dplyr::distinct()

  entity_vars <- unique(c("YEAR", avail_entity))

  # Total enrollment per entity × year with year-over-year changes
  totals <- data_dedup |>
    dplyr::group_by(dplyr::across(dplyr::all_of(entity_vars))) |>
    dplyr::summarize(N = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(avail_entity)),
      .data[["YEAR"]]
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(avail_entity))) |>
    dplyr::mutate(
      N_change = .data[["N"]] - dplyr::lag(.data[["N"]]),
      N_pct_change = dplyr::if_else(
        !is.na(dplyr::lag(.data[["N"]])) & dplyr::lag(.data[["N"]]) > 0,
        100 * (.data[["N"]] - dplyr::lag(.data[["N"]])) /
          dplyr::lag(.data[["N"]]),
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  # Subgroup proportions per entity × year
  if (length(avail_sg) == 0) {
    return(list(totals = totals, by_subgroup = data.frame()))
  }

  sg_results <- lapply(avail_sg, function(sg) {
    data_dedup |>
      dplyr::filter(!is.na(.data[[sg]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(entity_vars, sg)))) |>
      dplyr::summarize(N_sg = dplyr::n(), .groups = "drop") |>
      dplyr::left_join(
        totals |> dplyr::select(dplyr::all_of(c(entity_vars, "N"))),
        by = entity_vars
      ) |>
      dplyr::mutate(
        pct_sg = dplyr::if_else(
          .data[["N_sg"]] >= min_n & .data[["N"]] > 0,
          100 * .data[["N_sg"]] / .data[["N"]],
          NA_real_
        ),
        subgroup_var = sg,
        subgroup_val = as.character(.data[[sg]]),
        suppressed   = .data[["N_sg"]] < min_n
      ) |>
      dplyr::select(-dplyr::all_of(sg), -"N")
  })

  sg_long <- do.call(rbind, sg_results)

  list(totals = totals, by_subgroup = sg_long)
}


# ==============================================================================
# plot_enrollment_sankey()
# ==============================================================================

#' Plot enrollment composition as an alluvial (Sankey) diagram
#'
#' Visualizes how the composition of the tested population shifts across years
#' for one subgroup variable. Requires the `ggalluvial` package.
#'
#' @param transition_table A data frame — the `$table` element returned by
#'   [compute_subgroup_transitions()]. Must contain columns `YEAR`,
#'   `subgroup_val`, `N`, and `suppressed`.
#' @param subgroup_var Character scalar. The demographic variable being plotted
#'   (used for axis labels and the plot title).
#' @param years Integer vector of years to include. `NULL` includes all years
#'   present in `transition_table`. Default `NULL`.
#' @param assessment_spec Named list describing the assessment program.
#' @param palette Named character vector of colors, keyed by `subgroup_val`.
#'   `NULL` uses `ggplot2::scale_fill_brewer(palette = "Set2")`. Default `NULL`.
#'
#' @return A ggplot2 object (ggalluvial layers). Returns an informative blank
#'   plot if all data is suppressed or if `ggalluvial` is not installed.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' trans <- compute_subgroup_transitions(demo_data, "RACE_ETHNICITY", assessment_spec)
#' plot_enrollment_sankey(trans$table, "RACE_ETHNICITY", assessment_spec = assessment_spec)
#' }
plot_enrollment_sankey <- function(transition_table,
                                    subgroup_var,
                                    years           = NULL,
                                    assessment_spec,
                                    palette         = NULL) {
  stopifnot(
    is.data.frame(transition_table),
    is.character(subgroup_var), length(subgroup_var) == 1L,
    is.list(assessment_spec)
  )

  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    stop(
      "Package 'ggalluvial' is required for plot_enrollment_sankey(). ",
      "Install it with: install.packages('ggalluvial')"
    )
  }

  prog_name <- {
    sn <- assessment_spec$program$short_name
    if (!is.null(sn) && nzchar(sn)) sn else
      if (!is.null(assessment_spec$program$name)) assessment_spec$program$name else
        "Assessment"
  }

  if (!is.null(years)) {
    transition_table <- transition_table |>
      dplyr::filter(.data[["YEAR"]] %in% years)
  }

  # Remove suppressed cells before plotting
  plot_data <- transition_table |>
    dplyr::filter(!.data[["suppressed"]])

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::labs(
        title    = paste("Enrollment Composition —", subgroup_var, "—", prog_name),
        subtitle = msg
      ) +
      ggplot2::theme_bw()
  }

  if (nrow(plot_data) == 0) {
    return(blank_plot("No data meets the minimum sample size threshold."))
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      YEAR_chr    = as.character(.data[["YEAR"]]),
      alluvium_id = .data[["subgroup_val"]]
    )

  fill_scale <- if (!is.null(palette)) {
    ggplot2::scale_fill_manual(values = palette, name = subgroup_var)
  } else {
    ggplot2::scale_fill_brewer(palette = "Set2", name = subgroup_var)
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x        = YEAR_chr,
      y        = N,
      alluvium = alluvium_id,
      stratum  = subgroup_val,
      fill     = subgroup_val,
      label    = subgroup_val
    )
  ) +
    ggalluvial::geom_flow(alpha = 0.6) +
    ggalluvial::geom_stratum() +
    ggplot2::labs(
      title = paste("Enrollment Composition —", subgroup_var, "—", prog_name),
      x     = NULL,
      y     = "Number of Students"
    ) +
    fill_scale +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x     = ggplot2::element_text(size = 10)
    )
}


# ==============================================================================
# plot_enrollment_table()
# ==============================================================================

#' Format an enrollment data frame as a flextable
#'
#' Applies the package's standard `flextable` styling to an enrollment counts
#' or proportions data frame. Suppressed cells are shown as `"—"` and rows
#' with year-over-year proportion changes greater than 5 percentage points are
#' highlighted in yellow.
#'
#' @param counts_or_proportions A data frame — output of
#'   [compute_enrollment_counts()] or [compute_enrollment_proportions()].
#' @param type Character scalar: `"counts"`, `"proportions"`, or `"combined"`.
#'   Controls which highlight logic is applied. Default `"counts"`.
#' @param assessment_spec Named list describing the assessment program.
#' @param shift_threshold Numeric. Absolute percentage-point change above which
#'   a row is highlighted. Applies only when `pct_change` is in the data.
#'   Default `5`.
#'
#' @return A `flextable` object.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' counts <- compute_enrollment_counts(demo_data, by = "YEAR", assessment_spec)
#' plot_enrollment_table(counts, assessment_spec = assessment_spec)
#' }
plot_enrollment_table <- function(counts_or_proportions,
                                   type             = "counts",
                                   assessment_spec,
                                   shift_threshold  = 5) {
  stopifnot(
    is.data.frame(counts_or_proportions),
    is.character(type), length(type) == 1L,
    type %in% c("counts", "proportions", "combined"),
    is.list(assessment_spec)
  )

  df <- counts_or_proportions

  # Identify rows with large year-over-year shifts (proportions only)
  large_shift <- rep(FALSE, nrow(df))
  if (type %in% c("proportions", "combined") && "pct_change" %in% names(df)) {
    large_shift <- !is.na(df[["pct_change"]]) &
      abs(df[["pct_change"]]) > shift_threshold
  }

  # Build display copy: replace suppressed numeric cells with "—"
  display_df <- df
  if ("suppressed" %in% names(display_df)) {
    supp_rows <- !is.na(display_df[["suppressed"]]) & display_df[["suppressed"]]
    stat_cols <- intersect(
      c("N", "pct", "N_change", "N_pct_change", "pct_change",
        "N_sg", "pct_sg", "mean_score", "pct_proficient"),
      names(display_df)
    )
    for (col in stat_cols) {
      if (is.numeric(display_df[[col]])) {
        display_df[[col]] <- as.character(round(display_df[[col]], 1))
        display_df[[col]][supp_rows] <- "n/a"
      }
    }
  }

  # Round remaining numeric columns
  num_cols <- names(display_df)[vapply(display_df, is.numeric, logical(1))]
  for (col in num_cols) {
    display_df[[col]] <- round(display_df[[col]], 1)
  }

  # Drop helper columns before rendering
  drop_cols <- intersect(c("suppressed"), names(display_df))
  display_df <- display_df[, setdiff(names(display_df), drop_cols), drop = FALSE]

  ft <- flextable::flextable(display_df) |>
    flextable::bold(part = "header") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::set_table_properties(layout = "autofit", width = 1)

  # Highlight rows with large shifts in yellow (#fce8b2 per package convention)
  if (any(large_shift)) {
    shift_idx <- which(large_shift)
    ft <- flextable::bg(ft, i = shift_idx, bg = "#fce8b2", part = "body")
  }

  ft
}


# ==============================================================================
# summarize_enrollment()
# ==============================================================================

#' Summarize enrollment trends in plain text
#'
#' Generates a markdown narrative from the pre-computed enrollment results
#' list, describing overall enrollment size and year-over-year trends.
#'
#' @param enrollment_results Named list returned by [run_enrollment_analysis()].
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A named list:
#'   * `$narrative` — character string (markdown)
#'   * `$by_subject` — named list of per-subject strings (empty for enrollment)
#'   * `$table` — by-year counts data frame, or empty data frame if unavailable
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' res <- run_enrollment_analysis(demo_data, assessment_spec)
#' cat(res$text$narrative)
#' }
summarize_enrollment <- function(enrollment_results, assessment_spec) {
  stopifnot(
    is.list(enrollment_results),
    is.list(assessment_spec)
  )

  prog_name <- if (!is.null(assessment_spec$program$name))
    assessment_spec$program$name else "the assessment"

  parts <- character(0)

  # ---- Sentence 1: overall size and trend ----------------------------------
  by_yr <- enrollment_results$counts$by_year
  if (!is.null(by_yr) && nrow(by_yr) > 0) {
    all_yrs  <- sort(unique(by_yr[["YEAR"]]))
    first_yr <- all_yrs[1]
    last_yr  <- all_yrs[length(all_yrs)]
    n_first  <- by_yr[["N"]][by_yr[["YEAR"]] == first_yr][1]
    n_last   <- by_yr[["N"]][by_yr[["YEAR"]] == last_yr][1]

    if (!is.na(n_first) && !is.na(n_last)) {
      delta    <- n_last - n_first
      pct_chg  <- round(100 * delta / n_first, 1)
      dir_word <- if (delta > 0) "increased" else if (delta < 0) "decreased" else
        "remained stable"
      parts <- c(parts, sprintf(
        "Enrollment in %s %s from %s students in %s to %s in %s, a net change of %s (%s%%).",
        prog_name, dir_word,
        format(n_first, big.mark = ","), first_yr,
        format(n_last,  big.mark = ","), last_yr,
        format(delta,   big.mark = ","), pct_chg
      ))
    }
  }

  # ---- Sentence 2: grade with largest absolute change ----------------------
  by_grade <- enrollment_results$counts$by_year_grade
  if (!is.null(by_grade) && nrow(by_grade) > 0 &&
      "N_change" %in% names(by_grade)) {
    grade_net <- by_grade |>
      dplyr::filter(!is.na(.data[["GRADE"]]), !is.na(.data[["N_change"]])) |>
      dplyr::group_by(.data[["GRADE"]]) |>
      dplyr::summarize(total_chg = sum(.data[["N_change"]], na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(abs(.data[["total_chg"]])))

    if (nrow(grade_net) > 0) {
      top_grade <- grade_net[["GRADE"]][1]
      top_chg   <- grade_net[["total_chg"]][1]
      dir_g     <- if (top_chg > 0) "gained" else "lost"
      parts <- c(parts, sprintf(
        "Grade %s showed the largest cumulative shift, having %s %s students over the period.",
        top_grade, dir_g, format(abs(top_chg), big.mark = ",")
      ))
    }
  }

  # ---- Sentence 3: notable subgroup composition shift ----------------------
  sg_props <- enrollment_results$proportions$by_subgroup
  if (!is.null(sg_props) && nrow(sg_props) > 0) {
    all_yrs_sg <- sort(unique(sg_props[["YEAR"]]))
    if (length(all_yrs_sg) >= 2) {
      first_sg <- all_yrs_sg[1]
      last_sg  <- all_yrs_sg[length(all_yrs_sg)]

      sg_shift <- sg_props |>
        dplyr::filter(.data[["YEAR"]] %in% c(first_sg, last_sg),
                      !.data[["suppressed"]]) |>
        dplyr::select("YEAR", "subgroup_var", "subgroup_val", "pct") |>
        tidyr::pivot_wider(names_from = "YEAR", values_from = "pct",
                           names_prefix = "yr_") |>
        dplyr::mutate(
          chg = .data[[paste0("yr_", last_sg)]] -
                .data[[paste0("yr_", first_sg)]]
        ) |>
        dplyr::filter(!is.na(.data[["chg"]])) |>
        dplyr::arrange(dplyr::desc(abs(.data[["chg"]])))

      if (nrow(sg_shift) > 0) {
        top_sg  <- sg_shift[1, ]
        dir_sg  <- if (top_sg[["chg"]] > 0) "increased" else "decreased"
        parts <- c(parts, sprintf(
          "Among subgroups, the proportion of %s (%s) %s by %.1f percentage points between %s and %s.",
          top_sg[["subgroup_val"]], top_sg[["subgroup_var"]],
          dir_sg, abs(top_sg[["chg"]]), first_sg, last_sg
        ))
      }
    }
  }

  # ---- Sentence 4: enrollment-score co-movement ----------------------------
  se <- enrollment_results$score_enrollment_trends
  if (!is.null(se) && nrow(se) > 0 &&
      all(c("N_change", "delta_mean") %in% names(se))) {
    cor_val <- tryCatch(
      stats::cor(se[["N_change"]], se[["delta_mean"]], use = "complete.obs"),
      error = function(e) NA_real_
    )
    if (!is.na(cor_val)) {
      dir_cor  <- if (cor_val > 0) "positive" else "negative"
      str_cor  <- if (abs(cor_val) >= 0.5) "moderately strong" else
                  if (abs(cor_val) >= 0.25) "modest" else "weak"
      parts <- c(parts, sprintf(
        "Across subjects and grades, the relationship between enrollment change and mean score change was %s and %s (r = %.2f), suggesting that %s.",
        str_cor, dir_cor, cor_val,
        if (cor_val > 0.25)
          "years with enrollment growth tended to coincide with improving scores"
        else if (cor_val < -0.25)
          "years with enrollment growth tended to coincide with declining scores, possibly reflecting composition effects"
        else
          "enrollment changes did not consistently predict score direction"
      ))
    }
  }

  narrative <- if (length(parts) == 0)
    paste0("Enrollment data for ", prog_name, " is summarized in the tables below.")
  else
    paste(parts, collapse = " ")

  list(
    narrative  = narrative,
    by_subject = list(),
    table      = if (!is.null(enrollment_results$counts$by_year))
                   enrollment_results$counts$by_year else data.frame()
  )
}


# ==============================================================================
# run_enrollment_analysis()
# ==============================================================================

#' Run the Line 1.2 enrollment trend analysis
#'
#' Orchestrates all enrollment sub-analyses — counts, proportions, subgroup
#' transitions, entry/exit performance, and school/district aggregations —
#' and returns a single named results list suitable for passing to the
#' enrollment report templates.
#'
#' @param student_results_long A long-format student results data frame
#'   (see data contract). Should already be processed by
#'   [standardize_student_results()].
#' @param assessment_spec Named list describing the assessment program
#'   (see assessment_spec).
#' @param report_years Integer vector of years to include. `NULL` (default)
#'   uses all years present in the data.
#' @param subgroup_vars Character vector of demographic column names to use as
#'   subgroups. `NULL` (default) uses all variables listed in
#'   `assessment_spec$demographics`.
#' @param min_n Integer. Minimum group size for small-n suppression across
#'   all sub-analyses. Default `10`.
#'
#' @return A named list with sub-lists:
#'   * `$counts` — outputs of [compute_enrollment_counts()] at various grains
#'   * `$proportions` — outputs of [compute_enrollment_proportions()]
#'   * `$transitions` — named by subgroup variable; each element is the output
#'     of [compute_subgroup_transitions()]
#'   * `$entry_exit` — entering, exiting, and continuing student performance
#'   * `$aggregate` — school- and district-level summaries
#'   * `$text` — output of [summarize_enrollment()]
#'   * `$params` — run parameters
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
#' res <- run_enrollment_analysis(demo_data, assessment_spec)
#' res$counts$by_year
#' res$proportions$by_subgroup
#' }
run_enrollment_analysis <- function(student_results_long,
                                     assessment_spec,
                                     report_years  = NULL,
                                     subgroup_vars = NULL,
                                     min_n         = 10) {
  stopifnot(
    is.data.frame(student_results_long),
    is.list(assessment_spec),
    is.null(subgroup_vars) || is.character(subgroup_vars)
  )

  if (!is.null(report_years)) {
    student_results_long <- student_results_long |>
      dplyr::filter(.data[["YEAR"]] %in% report_years)
  }

  if (is.null(subgroup_vars)) {
    subgroup_vars <- names(assessment_spec$demographics)
  }
  avail_sg <- intersect(subgroup_vars, names(student_results_long))

  enrollment_results <- list(
    counts      = list(),
    proportions = list(),
    transitions = list(),
    entry_exit  = list(),
    aggregate   = list()
  )

  # ----------------------------------------------------------
  # 1. Counts
  # ----------------------------------------------------------
  message("  Running enrollment counts...")

  enrollment_results$counts$overall <- compute_enrollment_counts(
    student_results_long, by = NULL, assessment_spec, min_n = min_n
  )
  enrollment_results$counts$by_year <- compute_enrollment_counts(
    student_results_long, by = "YEAR", assessment_spec, min_n = min_n
  )
  enrollment_results$counts$by_year_grade <- compute_enrollment_counts(
    student_results_long, by = c("YEAR", "GRADE"), assessment_spec, min_n = min_n
  )
  enrollment_results$counts$by_year_subject <- compute_enrollment_counts(
    student_results_long, by = c("YEAR", "SUBJECT"), assessment_spec, min_n = min_n
  )
  if (length(avail_sg) > 0) {
    enrollment_results$counts$by_subgroup <- compute_enrollment_counts(
      student_results_long,
      by = c("YEAR", avail_sg),
      assessment_spec, min_n = min_n
    )
  }
  # Resolve school / district ID columns — accept both raw SGP names and the
  # standardized names produced by standardize_student_results().
  school_id_col   <- c("SCHOOL_NUMBER",   "SCHOOL_ID")  [
    c("SCHOOL_NUMBER",   "SCHOOL_ID")   %in% names(student_results_long)][1]
  district_id_col <- c("DISTRICT_NUMBER", "DISTRICT_ID")[
    c("DISTRICT_NUMBER", "DISTRICT_ID") %in% names(student_results_long)][1]

  if (!is.na(school_id_col)) {
    enrollment_results$counts$by_school <- compute_enrollment_counts(
      student_results_long,
      by = c("YEAR", school_id_col),
      assessment_spec, min_n = min_n
    )
  }
  if (!is.na(district_id_col)) {
    enrollment_results$counts$by_district <- compute_enrollment_counts(
      student_results_long,
      by = c("YEAR", district_id_col),
      assessment_spec, min_n = min_n
    )
  }

  # ----------------------------------------------------------
  # 2. Proportions
  # ----------------------------------------------------------
  message("  Running enrollment proportions...")

  enrollment_results$proportions$by_subgroup <- compute_enrollment_proportions(
    student_results_long, by = "YEAR",
    subgroup_vars = avail_sg, assessment_spec,
    denominator = "total_by_year", min_n = min_n
  )
  enrollment_results$proportions$by_year_grade <- compute_enrollment_proportions(
    student_results_long, by = c("YEAR", "GRADE"),
    subgroup_vars = avail_sg, assessment_spec,
    denominator = "within_grade", min_n = min_n
  )
  if (!is.na(school_id_col)) {
    enrollment_results$proportions$by_school_subgroup <- compute_enrollment_proportions(
      student_results_long, by = c("YEAR", school_id_col),
      subgroup_vars = avail_sg, assessment_spec,
      denominator = "within_school", min_n = min_n
    )
  }

  # ----------------------------------------------------------
  # 3. Transitions
  # ----------------------------------------------------------
  message("  Running subgroup transitions...")

  for (sg in avail_sg) {
    enrollment_results$transitions[[sg]] <- compute_subgroup_transitions(
      student_results_long, subgroup_var = sg, assessment_spec,
      cohort = FALSE, min_n = min_n
    )
  }

  # ----------------------------------------------------------
  # 4. Entry / Exit
  # ----------------------------------------------------------
  message("  Running entry/exit performance...")

  # Compute all three groups in one pass
  all_ee <- compute_entry_exit_performance(
    student_results_long, direction = "both",
    assessment_spec, by = c("YEAR", "SUBJECT", "GRADE"), min_n = min_n
  )
  enrollment_results$entry_exit$entering   <- all_ee[all_ee[["group"]] == "Entering",   ]
  enrollment_results$entry_exit$exiting    <- all_ee[all_ee[["group"]] == "Exiting",    ]
  enrollment_results$entry_exit$continuing <- all_ee[all_ee[["group"]] == "Continuing", ]

  # ----------------------------------------------------------
  # 5. Aggregate (school / district)
  # ----------------------------------------------------------
  message("  Running aggregate enrollment...")

  enrollment_results$aggregate$school <- compute_school_district_enrollment(
    student_results_long, level = "school",
    assessment_spec, subgroup_vars = avail_sg, min_n = min_n
  )
  enrollment_results$aggregate$district <- compute_school_district_enrollment(
    student_results_long, level = "district",
    assessment_spec, subgroup_vars = avail_sg, min_n = min_n
  )

  # ----------------------------------------------------------
  # 5b. Overall enrollment vs. score trends (by SUBJECT × GRADE)
  # ----------------------------------------------------------
  message("  Computing overall enrollment vs. score trends...")

  # Mean scores by YEAR × SUBJECT × GRADE using existing compute_score_summary()
  overall_scores <- compute_score_summary(
    student_results_long, assessment_spec, by_grade = TRUE, min_n = min_n
  )$ScaleScore_Stats_Wide |>
    dplyr::select("YEAR", "SUBJECT", "GRADE", mean_score = "mean")

  prog_name <- if (!is.null(assessment_spec$program$short_name))
    assessment_spec$program$short_name else
    if (!is.null(assessment_spec$program$name)) assessment_spec$program$name else
      "Assessment"

  # Enrollment counts by YEAR × SUBJECT × GRADE (computed inline)
  enroll_by_subj_grade <- compute_enrollment_counts(
    student_results_long,
    by = c("YEAR", "SUBJECT", "GRADE"),
    assessment_spec,
    min_n = min_n
  )

  if (nrow(enroll_by_subj_grade) > 0 && nrow(overall_scores) > 0) {
    score_enroll <- enroll_by_subj_grade |>
      dplyr::left_join(overall_scores, by = c("YEAR", "SUBJECT", "GRADE")) |>
      dplyr::arrange(.data[["SUBJECT"]], .data[["GRADE"]], .data[["YEAR"]]) |>
      dplyr::group_by(.data[["SUBJECT"]], .data[["GRADE"]]) |>
      dplyr::mutate(
        delta_mean = .data[["mean_score"]] - dplyr::lag(.data[["mean_score"]]),
        year_pair  = paste0(dplyr::lag(.data[["YEAR"]]), "–", .data[["YEAR"]])
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data[["N_change"]]), !is.na(.data[["delta_mean"]]))

    enrollment_results$score_enrollment_trends <- score_enroll
    enrollment_results$plot$enroll_vs_score <-
      .build_overall_enroll_score_plot(score_enroll, prog_name)
  } else {
    enrollment_results$score_enrollment_trends <- data.frame()
    enrollment_results$plot$enroll_vs_score    <- NULL
  }

  # ----------------------------------------------------------
  # 6. Text summary
  # ----------------------------------------------------------
  enrollment_results$text <- summarize_enrollment(enrollment_results, assessment_spec)

  enrollment_results$params <- list(min_n = min_n)

  enrollment_results
}
