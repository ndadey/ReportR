# ==============================================================================
# Section 5 — Aggregate (School and District) Analysis
#
# All analyses aggregate student_results_long to the school or district level.
# School-level grouping always uses SCHOOL_NUMBER (or SCHOOL_ID after
# standardization) as the primary key; SCHOOL_NAME / DISTRICT_NAME are
# retained as display labels only.
#
# Sub-analyses that require school_results_long (accountability grades, HQIM
# flags, locale, co-op) are NOT implemented here. Their result slots are set
# to NULL with explanatory comments in run_aggregate_analysis().
#
# Functions (exported):
#   compute_performance_trends()    — 5.1 school/district performance over time
#   compute_population_shifts()     — 5.3 demographic composition and shifts
#   compute_outlier_schools()       — 5.4 residual-based outlier identification
#   compute_school_disaggregated()  — 5.5 subgroup gaps at school level
#   compute_stability_analysis()    — 5.6 rank-order and correlation stability
# ==============================================================================


# ==============================================================================
# Internal helpers
# ==============================================================================

# Resolve the school ID, district ID, and subject column names, accepting both
# raw SGP names and the standardized names produced by standardize_student_results().
.resolve_agg_cols <- function(data) {
  list(
    school_id   = intersect(c("SCHOOL_NUMBER",   "SCHOOL_ID"),   names(data))[1],
    district_id = intersect(c("DISTRICT_NUMBER", "DISTRICT_ID"), names(data))[1],
    subject     = intersect(c("SUBJECT", "CONTENT_AREA"),        names(data))[1]
  )
}

# Derive a logical PROFICIENT column from ACHIEVEMENT_LEVEL and the spec.
.add_proficient <- function(data, spec) {
  bench_idx   <- benchmark_index(spec)
  labels      <- spec$achievement_levels$labels
  prof_labels <- labels[seq(bench_idx, length(labels))]
  data |>
    dplyr::mutate(
      PROFICIENT = !is.na(.data[["ACHIEVEMENT_LEVEL"]]) &
        .data[["ACHIEVEMENT_LEVEL"]] %in% prof_labels
    )
}

# Compute demographic proportions for binary demographic variables at the
# school × year grain. Returns a data frame with one column per variable
# named <VAR>_pct (e.g. FRL_pct, EL_pct).
.school_demo_proportions <- function(data, school_id, spec) {
  demo_vars    <- names(spec$demographics)
  binary_demos <- names(Filter(function(d) d$type == "binary", spec$demographics))
  binary_avail <- intersect(binary_demos, names(data))

  if (length(binary_avail) == 0 || is.na(school_id)) return(data.frame())

  yes_vals <- c("YES", "Y", "TRUE", "1")

  data |>
    dplyr::filter(!is.na(.data[[school_id]])) |>
    dplyr::group_by(.data[[school_id]], .data[["YEAR"]]) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(binary_avail),
        ~ round(100 * mean(!is.na(.) & toupper(as.character(.)) %in% yes_vals,
                           na.rm = TRUE), 2),
        .names = "{.col}_pct"
      ),
      .groups = "drop"
    )
}

# Compute minority_pct (% non-white) by school × year from RACE_ETHNICITY.
.school_minority_pct <- function(data, school_id) {
  if (!("RACE_ETHNICITY" %in% names(data)) || is.na(school_id)) return(data.frame())

  white_vals <- c("WHITE", "NON-HISPANIC WHITE", "W", "WHITE, NOT OF HISPANIC ORIGIN")

  data |>
    dplyr::filter(!is.na(.data[[school_id]]),
                  !is.na(.data[["RACE_ETHNICITY"]])) |>
    dplyr::group_by(.data[[school_id]], .data[["YEAR"]]) |>
    dplyr::summarize(
      minority_pct = round(
        100 * mean(!toupper(.data[["RACE_ETHNICITY"]]) %in% white_vals, na.rm = TRUE),
        2
      ),
      .groups = "drop"
    )
}


# ==============================================================================
# compute_performance_trends()
# ==============================================================================

#' Compute school- and district-level performance trends
#'
#' Aggregates `student_results_long` to the school and district level and
#' computes summary statistics, year-over-year changes, improvement
#' classifications, school-level regression slopes, rank orders, year-to-year
#' correlations, and intraclass correlations (ICCs). ICC computation requires
#' the `lme4` package (in Suggests); if not installed, `$icc_tables` is an
#' empty data frame.
#'
#' @param data A long-format student results data frame (see data contract).
#' @param spec Named list describing the assessment program (assessment_spec).
#' @param min_n_school Integer. Schools with fewer students than this in a
#'   year × subject × grade cell are suppressed. Default `10`.
#' @param improvement_threshold Numeric. Percentage-point change threshold for
#'   classifying a school as improving or declining. Default `3`.
#' @param correlation_flag_threshold Numeric. Year-to-year Spearman r below
#'   which a year pair is flagged as unstable. Default `0.85`.
#'
#' @return A named list:
#'   * `$school_level_summary` — school × year × subject × grade with N,
#'     mean_score, sd_score, p05–p95, n_proficient, pct_proficient,
#'     yoy_change, demographic proportion columns, and yoy_change_score
#'   * `$district_level_summary` — district × year × subject × grade
#'   * `$improvement_classification` — school × year × subject: classification
#'   * `$regression_slopes` — school × subject × grade: slope, se, r_squared
#'   * `$year_to_year_correlations` — year_pair × subject × grade:
#'     pearson_r, spearman_r, n_schools, flagged
#'   * `$rank_order_data` — school × year × subject × grade: rank, pct_rank
#'   * `$icc_tables` — grade × subject × year: icc, var_between, var_within
#' @export
compute_performance_trends <- function(data,
                                        spec,
                                        min_n_school             = 10,
                                        improvement_threshold    = 3,
                                        correlation_flag_threshold = 0.85) {
  stopifnot(
    is.data.frame(data),
    is.list(spec),
    all(c("YEAR", "SCALE_SCORE", "ACHIEVEMENT_LEVEL") %in% names(data))
  )

  cols        <- .resolve_agg_cols(data)
  school_id   <- cols$school_id
  district_id <- cols$district_id
  subj_col    <- cols$subject

  if (is.na(school_id) || is.na(subj_col)) {
    message("compute_performance_trends: school ID or subject column not found.")
    empty <- data.frame()
    return(list(
      school_level_summary       = empty, district_level_summary = empty,
      improvement_classification = empty, regression_slopes      = empty,
      year_to_year_correlations  = empty, rank_order_data        = empty,
      icc_tables                 = empty
    ))
  }

  data <- .add_proficient(data, spec)

  # Label columns to retain (not grouping keys)
  school_label_cols <- intersect(c("SCHOOL_NAME"), names(data))
  dist_label_cols   <- intersect(c("DISTRICT_NAME"), names(data))
  school_grp <- c(school_id, school_label_cols, district_id, dist_label_cols,
                  "YEAR", subj_col, "GRADE")
  school_grp <- intersect(school_grp, names(data))

  # ------------------------------------------------------------------
  # 1. School-level: counts + distributional stats via group_modify
  # ------------------------------------------------------------------
  school_counts <- data |>
    dplyr::filter(!is.na(.data[["SCALE_SCORE"]]),
                  !is.na(.data[["GRADE"]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(school_grp))) |>
    dplyr::summarize(
      N            = dplyr::n(),
      n_proficient = sum(.data[["PROFICIENT"]], na.rm = TRUE),
      .groups      = "drop"
    ) |>
    dplyr::mutate(
      pct_proficient = dplyr::if_else(
        .data[["N"]] >= min_n_school,
        100 * .data[["n_proficient"]] / .data[["N"]],
        NA_real_
      )
    )

  school_stats <- data |>
    dplyr::filter(!is.na(.data[["SCALE_SCORE"]]),
                  !is.na(.data[["GRADE"]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(school_grp))) |>
    dplyr::group_modify(function(d, k) {
      n <- nrow(d)
      if (n >= min_n_school) {
        x <- d[["SCALE_SCORE"]]
        data.frame(
          mean_score = mean(x, na.rm = TRUE),
          sd_score   = stats::sd(x, na.rm = TRUE),
          p05        = as.numeric(stats::quantile(x, 0.05, na.rm = TRUE)),
          p25        = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE)),
          p50        = as.numeric(stats::quantile(x, 0.50, na.rm = TRUE)),
          p75        = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE)),
          p95        = as.numeric(stats::quantile(x, 0.95, na.rm = TRUE)),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(mean_score = NA_real_, sd_score = NA_real_,
                   p05 = NA_real_, p25 = NA_real_, p50 = NA_real_,
                   p75 = NA_real_, p95 = NA_real_, stringsAsFactors = FALSE)
      }
    }) |>
    dplyr::ungroup()

  school_level <- school_counts |>
    dplyr::left_join(school_stats, by = school_grp)

  # ------------------------------------------------------------------
  # 2. Year-over-year changes
  # ------------------------------------------------------------------
  sort_vars <- setdiff(school_grp, c("YEAR", school_label_cols, dist_label_cols))

  school_level <- school_level |>
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_vars)),
                   .data[["YEAR"]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(sort_vars))) |>
    dplyr::mutate(
      yoy_change       = .data[["pct_proficient"]] - dplyr::lag(.data[["pct_proficient"]]),
      yoy_change_score = .data[["mean_score"]]     - dplyr::lag(.data[["mean_score"]])
    ) |>
    dplyr::ungroup()

  # ------------------------------------------------------------------
  # 3. Attach demographic proportions (used later in outlier regression)
  # ------------------------------------------------------------------
  demo_props <- .school_demo_proportions(data, school_id, spec)
  min_props  <- .school_minority_pct(data, school_id)

  if (nrow(demo_props) > 0) {
    join_key <- c(school_id, "YEAR")
    school_level <- school_level |>
      dplyr::left_join(demo_props, by = join_key)
  }
  if (nrow(min_props) > 0) {
    join_key <- c(school_id, "YEAR")
    school_level <- school_level |>
      dplyr::left_join(min_props, by = join_key)
  }

  # ------------------------------------------------------------------
  # 4. Improvement classification
  # ------------------------------------------------------------------
  improvement_classification <- school_level |>
    dplyr::filter(!is.na(.data[["yoy_change"]])) |>
    dplyr::mutate(
      classification = dplyr::case_when(
        .data[["yoy_change"]] >  improvement_threshold ~ "improving",
        .data[["yoy_change"]] < -improvement_threshold ~ "declining",
        TRUE ~ "stable"
      )
    ) |>
    dplyr::select(dplyr::all_of(c(
      school_id, school_label_cols, district_id, dist_label_cols,
      "YEAR", subj_col, "GRADE", "yoy_change", "classification"
    )))

  # ------------------------------------------------------------------
  # 5. Regression slopes (per school × subject × grade over years)
  # ------------------------------------------------------------------
  slope_grp <- c(school_id, school_label_cols, district_id, dist_label_cols,
                 subj_col, "GRADE")
  slope_grp <- intersect(slope_grp, names(school_level))

  regression_slopes <- school_level |>
    dplyr::group_by(dplyr::across(dplyr::all_of(slope_grp))) |>
    dplyr::group_modify(function(d, k) {
      d_clean <- d[!is.na(d[["pct_proficient"]]), ]
      if (nrow(d_clean) < 3) {
        return(data.frame(slope = NA_real_, se = NA_real_,
                          r_squared = NA_real_, n_years = nrow(d_clean),
                          stringsAsFactors = FALSE))
      }
      tryCatch({
        fit <- stats::lm(pct_proficient ~ YEAR, data = d_clean)
        s   <- summary(fit)
        data.frame(
          slope     = stats::coef(fit)[["YEAR"]],
          se        = s$coefficients["YEAR", "Std. Error"],
          r_squared = s$r.squared,
          n_years   = nrow(d_clean),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        data.frame(slope = NA_real_, se = NA_real_,
                   r_squared = NA_real_, n_years = nrow(d_clean),
                   stringsAsFactors = FALSE)
      })
    }) |>
    dplyr::ungroup()

  # ------------------------------------------------------------------
  # 6. Year-to-year correlations of school proficiency
  # ------------------------------------------------------------------
  all_yrs <- sort(unique(school_level[["YEAR"]]))
  corr_key <- c(school_id, subj_col, "GRADE", "YEAR", "pct_proficient")
  corr_key <- intersect(corr_key, names(school_level))

  prof_long <- school_level |>
    dplyr::select(dplyr::all_of(corr_key)) |>
    dplyr::filter(!is.na(.data[["pct_proficient"]]))

  corr_list <- lapply(seq_along(all_yrs)[-1], function(i) {
    yr1 <- all_yrs[i - 1L]
    yr2 <- all_yrs[i]
    d1  <- prof_long[prof_long[["YEAR"]] == yr1, ]
    d2  <- prof_long[prof_long[["YEAR"]] == yr2, ]

    join_key <- c(school_id, subj_col, "GRADE")
    join_key <- intersect(join_key, names(d1))

    merged <- dplyr::inner_join(
      d1 |> dplyr::select(dplyr::all_of(c(join_key, "pct_proficient"))),
      d2 |> dplyr::select(dplyr::all_of(c(join_key, "pct_proficient"))),
      by = join_key, suffix = c("_yr1", "_yr2")
    )

    if (nrow(merged) < 5) return(NULL)

    subj_grade_groups <- merged |>
      dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(join_key, school_id)))) |>
      dplyr::group_modify(function(d, k) {
        if (nrow(d) < 5) return(data.frame())
        pr <- tryCatch(stats::cor(d$pct_proficient_yr1, d$pct_proficient_yr2,
                                   use = "complete.obs"), error = function(e) NA_real_)
        sr <- tryCatch(stats::cor(d$pct_proficient_yr1, d$pct_proficient_yr2,
                                   use = "complete.obs", method = "spearman"),
                       error = function(e) NA_real_)
        data.frame(pearson_r = pr, spearman_r = sr, n_schools = nrow(d),
                   stringsAsFactors = FALSE)
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        year_pair = paste0(yr1, "\u2013", yr2),
        flagged   = !is.na(.data[["spearman_r"]]) &
          .data[["spearman_r"]] < correlation_flag_threshold
      )

    subj_grade_groups
  })

  year_to_year_correlations <- do.call(rbind, Filter(Negate(is.null), corr_list))
  if (is.null(year_to_year_correlations)) year_to_year_correlations <- data.frame()

  # ------------------------------------------------------------------
  # 7. Rank order data
  # ------------------------------------------------------------------
  rank_grp <- c("YEAR", subj_col, "GRADE")
  rank_grp <- intersect(rank_grp, names(school_level))

  rank_order_data <- school_level |>
    dplyr::filter(!is.na(.data[["pct_proficient"]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(rank_grp))) |>
    dplyr::mutate(
      rank     = rank(-.data[["pct_proficient"]], ties.method = "min"),
      pct_rank = .data[["rank"]] / dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(
      school_id, school_label_cols, district_id, dist_label_cols,
      "YEAR", subj_col, "GRADE", "pct_proficient", "rank", "pct_rank"
    )))

  # ------------------------------------------------------------------
  # 8. ICCs (requires lme4; returns empty data frame if not installed)
  # ------------------------------------------------------------------
  icc_grp <- c(subj_col, "GRADE", "YEAR")
  icc_grp <- intersect(icc_grp, names(data))

  icc_tables <- if (requireNamespace("lme4", quietly = TRUE)) {
    icc_formula <- paste("SCALE_SCORE ~ 1 + (1 |", school_id, ")")
    data |>
      dplyr::filter(!is.na(.data[["SCALE_SCORE"]]),
                    !is.na(.data[["GRADE"]]),
                    !is.na(.data[[school_id]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(icc_grp))) |>
      dplyr::group_modify(function(d, k) {
        n_schools <- length(unique(d[[school_id]]))
        if (n_schools < 5 || nrow(d) < 30) {
          return(data.frame(icc = NA_real_, var_between = NA_real_,
                            var_within = NA_real_, n_schools = n_schools,
                            n_students = nrow(d), stringsAsFactors = FALSE))
        }
        tryCatch({
          fit  <- lme4::lmer(as.formula(icc_formula), data = d)
          vc   <- as.data.frame(lme4::VarCorr(fit))
          vb   <- vc[["vcov"]][vc[["grp"]] == school_id]
          vw   <- vc[["vcov"]][vc[["grp"]] == "Residual"]
          data.frame(icc = vb / (vb + vw), var_between = vb, var_within = vw,
                     n_schools = n_schools, n_students = nrow(d),
                     stringsAsFactors = FALSE)
        }, error = function(e) {
          data.frame(icc = NA_real_, var_between = NA_real_, var_within = NA_real_,
                     n_schools = n_schools, n_students = nrow(d),
                     stringsAsFactors = FALSE)
        })
      }) |>
      dplyr::ungroup()
  } else {
    message("compute_performance_trends: lme4 not installed; icc_tables is empty. ",
            "Install with: install.packages('lme4')")
    data.frame()
  }

  # ------------------------------------------------------------------
  # 9. District-level summary
  # ------------------------------------------------------------------
  dist_grp <- c(district_id, dist_label_cols, "YEAR", subj_col, "GRADE")
  dist_grp <- intersect(dist_grp, names(data))

  district_counts <- data |>
    dplyr::filter(!is.na(.data[["SCALE_SCORE"]]),
                  !is.na(.data[["GRADE"]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dist_grp))) |>
    dplyr::summarize(
      N            = dplyr::n(),
      n_proficient = sum(.data[["PROFICIENT"]], na.rm = TRUE),
      .groups      = "drop"
    ) |>
    dplyr::mutate(
      pct_proficient = dplyr::if_else(
        .data[["N"]] >= min_n_school,
        100 * .data[["n_proficient"]] / .data[["N"]],
        NA_real_
      )
    )

  district_stats <- data |>
    dplyr::filter(!is.na(.data[["SCALE_SCORE"]]),
                  !is.na(.data[["GRADE"]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dist_grp))) |>
    dplyr::group_modify(function(d, k) {
      n <- nrow(d)
      if (n >= min_n_school) {
        x <- d[["SCALE_SCORE"]]
        data.frame(mean_score = mean(x, na.rm = TRUE),
                   sd_score   = stats::sd(x, na.rm = TRUE),
                   p50        = as.numeric(stats::quantile(x, 0.50, na.rm = TRUE)),
                   stringsAsFactors = FALSE)
      } else {
        data.frame(mean_score = NA_real_, sd_score = NA_real_,
                   p50 = NA_real_, stringsAsFactors = FALSE)
      }
    }) |>
    dplyr::ungroup()

  dist_sort_vars <- setdiff(dist_grp, c("YEAR", dist_label_cols))

  district_level_summary <- district_counts |>
    dplyr::left_join(district_stats, by = dist_grp) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(dist_sort_vars)), .data[["YEAR"]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dist_sort_vars))) |>
    dplyr::mutate(
      yoy_change       = .data[["pct_proficient"]] - dplyr::lag(.data[["pct_proficient"]]),
      yoy_change_score = .data[["mean_score"]]     - dplyr::lag(.data[["mean_score"]])
    ) |>
    dplyr::ungroup()

  list(
    school_level_summary       = school_level,
    district_level_summary     = district_level_summary,
    improvement_classification = improvement_classification,
    regression_slopes          = regression_slopes,
    year_to_year_correlations  = year_to_year_correlations,
    rank_order_data            = rank_order_data,
    icc_tables                 = icc_tables
  )
}


# ==============================================================================
# compute_population_shifts()
# ==============================================================================

#' Compute school- and district-level demographic composition and shifts
#'
#' Tracks how the demographic composition of the tested population changes over
#' time at the school and district level, and examines whether composition
#' shifts correlate with proficiency changes.
#'
#' @param data A long-format student results data frame.
#' @param performance_data Named list returned by [compute_performance_trends()].
#' @param spec Named list describing the assessment program.
#' @param school_data `school_results_long` data frame, or `NULL`. When `NULL`,
#'   co-op and locale breakdowns are set to `NULL`.
#' @param min_n_school Integer. Minimum school size for inclusion. Default `10`.
#'
#' @return A named list:
#'   * `$school_demographic_composition` — long format: school × year × group × pct
#'   * `$district_demographic_composition` — long format: district × year × group × pct
#'   * `$demographic_trend_slopes` — school × group: slope over years
#'   * `$composition_proficiency_join` — school × year: delta_composition columns
#'     and delta_pct_proficient for each subject
#'   * `$by_coop` — `NULL` (requires school_results_long)
#'   * `$by_locale` — `NULL` (requires school_results_long)
#' @export
compute_population_shifts <- function(data,
                                       performance_data,
                                       spec,
                                       school_data   = NULL,
                                       min_n_school  = 10) {
  stopifnot(
    is.data.frame(data),
    is.list(performance_data),
    is.list(spec)
  )

  cols        <- .resolve_agg_cols(data)
  school_id   <- cols$school_id
  district_id <- cols$district_id
  subj_col    <- cols$subject

  demo_vars <- names(spec$demographics)
  all_avail <- intersect(demo_vars, names(data))

  # ------------------------------------------------------------------
  # 1. School demographic composition in long format
  # ------------------------------------------------------------------
  school_demo_long <- if (length(all_avail) > 0 && !is.na(school_id)) {
    school_label_cols <- intersect("SCHOOL_NAME", names(data))
    dist_label_cols   <- intersect("DISTRICT_NAME", names(data))
    id_cols <- c(school_id, school_label_cols, district_id, dist_label_cols, "YEAR")
    id_cols <- intersect(id_cols, names(data))

    # For binary vars: % Yes; for categorical vars: % in each category
    binary_demos <- names(Filter(function(d) d$type == "binary", spec$demographics))
    cat_demos    <- names(Filter(function(d) d$type == "categorical", spec$demographics))

    binary_avail <- intersect(binary_demos, names(data))
    cat_avail    <- intersect(cat_demos, names(data))
    yes_vals     <- c("YES", "Y", "TRUE", "1")

    rows_list <- list()

    # Binary: one row per school × year × variable
    if (length(binary_avail) > 0) {
      for (var in binary_avail) {
        tmp <- data |>
          dplyr::filter(!is.na(.data[[school_id]])) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
          dplyr::summarize(
            N   = dplyr::n(),
            pct = round(100 * mean(!is.na(.data[[var]]) &
                                     toupper(as.character(.data[[var]])) %in% yes_vals,
                                   na.rm = TRUE), 2),
            .groups = "drop"
          ) |>
          dplyr::filter(.data[["N"]] >= min_n_school) |>
          dplyr::mutate(demo_var = var, demo_val = "Yes") |>
          dplyr::select(-"N")
        rows_list[[var]] <- tmp
      }
    }

    # Categorical: one row per school × year × variable × category
    if (length(cat_avail) > 0) {
      for (var in cat_avail) {
        tmp <- data |>
          dplyr::filter(!is.na(.data[[school_id]]),
                        !is.na(.data[[var]])) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, var)))) |>
          dplyr::summarize(N_cat = dplyr::n(), .groups = "drop") |>
          dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
          dplyr::mutate(
            N_total = sum(.data[["N_cat"]]),
            pct     = round(100 * .data[["N_cat"]] / .data[["N_total"]], 2)
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(.data[["N_total"]] >= min_n_school) |>
          dplyr::rename(demo_val = dplyr::all_of(var)) |>
          dplyr::mutate(demo_val = as.character(.data[["demo_val"]]),
                        demo_var = var) |>
          dplyr::select(-"N_cat", -"N_total")
        rows_list[[var]] <- tmp
      }
    }

    do.call(rbind, rows_list)
  } else {
    data.frame()
  }

  # ------------------------------------------------------------------
  # 2. District demographic composition
  # ------------------------------------------------------------------
  district_demo_long <- if (nrow(school_demo_long) > 0 && !is.na(district_id)) {
    dist_id_cols <- intersect(c(district_id, "DISTRICT_NAME", "YEAR", "demo_var", "demo_val"),
                              names(school_demo_long))
    school_demo_long |>
      dplyr::group_by(dplyr::across(dplyr::all_of(dist_id_cols))) |>
      dplyr::summarize(pct = round(mean(.data[["pct"]], na.rm = TRUE), 2),
                       .groups = "drop")
  } else {
    data.frame()
  }

  # ------------------------------------------------------------------
  # 3. Demographic trend slopes (per school × demo_var × demo_val)
  # ------------------------------------------------------------------
  demographic_trend_slopes <- if (nrow(school_demo_long) > 0) {
    slope_id_cols <- intersect(
      c(school_id, "SCHOOL_NAME", district_id, "DISTRICT_NAME", "demo_var", "demo_val"),
      names(school_demo_long)
    )
    school_demo_long |>
      dplyr::group_by(dplyr::across(dplyr::all_of(slope_id_cols))) |>
      dplyr::group_modify(function(d, k) {
        d_clean <- d[!is.na(d[["pct"]]), ]
        if (nrow(d_clean) < 3) {
          return(data.frame(slope = NA_real_, n_years = nrow(d_clean),
                            stringsAsFactors = FALSE))
        }
        tryCatch({
          fit <- stats::lm(pct ~ YEAR, data = d_clean)
          data.frame(slope = stats::coef(fit)[["YEAR"]],
                     n_years = nrow(d_clean), stringsAsFactors = FALSE)
        }, error = function(e) {
          data.frame(slope = NA_real_, n_years = nrow(d_clean),
                     stringsAsFactors = FALSE)
        })
      }) |>
      dplyr::ungroup()
  } else {
    data.frame()
  }

  # ------------------------------------------------------------------
  # 4. Composition-proficiency join
  # ------------------------------------------------------------------
  composition_proficiency_join <- if (
    !is.null(performance_data$school_level_summary) &&
    nrow(performance_data$school_level_summary) > 0 &&
    nrow(school_demo_long) > 0
  ) {
    binary_avail <- intersect(
      names(Filter(function(d) d$type == "binary", spec$demographics)),
      names(data)
    )

    # Wide-format binary demo proportions.
    # Collapse duplicates (e.g. inconsistent district assignments in source
    # data) before pivoting so pivot_wider receives one row per school × year
    # × demo_var, avoiding list-column output.
    binary_wide <- school_demo_long |>
      dplyr::filter(.data[["demo_var"]] %in% binary_avail,
                    .data[["demo_val"]] == "Yes") |>
      dplyr::select(dplyr::all_of(c(school_id, "YEAR", "demo_var", "pct"))) |>
      dplyr::group_by(.data[[school_id]], .data[["YEAR"]], .data[["demo_var"]]) |>
      dplyr::summarize(pct = mean(.data[["pct"]], na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = "demo_var", values_from = "pct",
                         names_glue = "{demo_var}_pct") |>
      dplyr::mutate(dplyr::across(dplyr::ends_with("_pct"), as.numeric))

    # Year-over-year changes in composition
    binary_wide <- binary_wide |>
      dplyr::arrange(.data[[school_id]], .data[["YEAR"]]) |>
      dplyr::group_by(.data[[school_id]]) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with("_pct"),
          ~ . - dplyr::lag(.),
          .names = "delta_{.col}"
        )
      ) |>
      dplyr::ungroup()

    # Join to school-level proficiency changes
    prof_changes <- performance_data$school_level_summary |>
      dplyr::filter(!is.na(.data[["yoy_change"]])) |>
      dplyr::select(dplyr::all_of(intersect(
        c(school_id, "SCHOOL_NAME", district_id, "DISTRICT_NAME",
          subj_col, "GRADE", "YEAR", "yoy_change", "yoy_change_score"),
        names(performance_data$school_level_summary)
      )))

    join_key <- c(school_id, "YEAR")
    prof_changes |>
      dplyr::left_join(binary_wide, by = join_key)
  } else {
    data.frame()
  }

  # [REQUIRES school_results_long — NOT RUN]
  # by_coop and by_locale require CO_OP and LOCALE from school_results_long.
  list(
    school_demographic_composition   = school_demo_long,
    district_demographic_composition = district_demo_long,
    demographic_trend_slopes         = demographic_trend_slopes,
    composition_proficiency_join     = composition_proficiency_join,
    by_coop                          = NULL,  # [REQUIRES school_results_long]
    by_locale                        = NULL   # [REQUIRES school_results_long]
  )
}


# ==============================================================================
# compute_outlier_schools()
# ==============================================================================

#' Identify schools performing above or below demographic expectation
#'
#' Fits a regression of school-level proficiency on demographic composition
#' variables (FRL_pct, EL_pct, IEP_pct, minority_pct where available) within
#' each year × subject, computes residuals, and flags schools exceeding
#' `outlier_sd_threshold` standard deviations. Also produces funnel plot
#' control limits and tracks outlier stability across years.
#'
#' @param performance_data Named list returned by [compute_performance_trends()].
#' @param spec Named list describing the assessment program.
#' @param school_data `school_results_long`, or `NULL`. When `NULL`,
#'   `$outlier_characteristics` is set to `NULL`.
#' @param outlier_sd_threshold Numeric. SD threshold for flagging. Default `2`.
#' @param outlier_n_funnel Integer. Minimum n for funnel plot inclusion.
#'   Default `20`.
#'
#' @return A named list:
#'   * `$residual_model_results` — school × year × subject: observed,
#'     predicted, residual, sd_residual, outlier_flag, n_tested
#'   * `$positive_outlier_schools` — subset with outlier_flag == "high"
#'   * `$negative_outlier_schools` — subset with outlier_flag == "low"
#'   * `$outlier_stability` — school × subject: n_years_flagged, direction
#'   * `$funnel_plot_data` — school × year × subject: proficiency,
#'     n_tested, state_pct, upper_95, lower_95, upper_99, lower_99
#'   * `$outlier_characteristics` — `NULL` (requires school_results_long)
#' @export
compute_outlier_schools <- function(performance_data,
                                     spec,
                                     school_data           = NULL,
                                     outlier_sd_threshold  = 2,
                                     outlier_n_funnel      = 20) {
  stopifnot(
    is.list(performance_data),
    is.list(spec),
    !is.null(performance_data$school_level_summary)
  )

  school_tbl <- performance_data$school_level_summary
  if (nrow(school_tbl) == 0) {
    empty <- data.frame()
    return(list(
      residual_model_results = empty, positive_outlier_schools = empty,
      negative_outlier_schools = empty, outlier_stability = empty,
      funnel_plot_data = empty, outlier_characteristics = NULL
    ))
  }

  # Detect column names from performance_data
  cols        <- .resolve_agg_cols(school_tbl)
  school_id   <- cols$school_id
  subj_col    <- cols$subject

  # Available demographic predictor columns
  demo_pct_cols <- c(
    paste0(names(Filter(function(d) d$type == "binary", spec$demographics)), "_pct"),
    "minority_pct"
  )
  demo_pct_avail <- intersect(demo_pct_cols, names(school_tbl))

  # ------------------------------------------------------------------
  # 1. Residual model: proficiency ~ demographics (per year × subject,
  #    collapsing across grades by computing school-level proficiency mean)
  # ------------------------------------------------------------------
  school_yr_subj <- school_tbl |>
    dplyr::filter(!is.na(.data[["pct_proficient"]]),
                  .data[["N"]] >= 10) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(school_id, "SCHOOL_NAME", "DISTRICT_NAME", subj_col, "YEAR")
    ))) |>
    dplyr::summarize(
      n_tested       = sum(.data[["N"]], na.rm = TRUE),
      pct_proficient = stats::weighted.mean(.data[["pct_proficient"]],
                                             .data[["N"]], na.rm = TRUE),
      dplyr::across(dplyr::all_of(demo_pct_avail),
                    ~ mean(., na.rm = TRUE)),
      .groups = "drop"
    )

  group_vars <- c(subj_col, "YEAR")
  group_vars <- intersect(group_vars, names(school_yr_subj))

  residual_results <- school_yr_subj |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::group_modify(function(d, k) {
      d_model <- d[stats::complete.cases(d[, c("pct_proficient", demo_pct_avail)]), ]
      if (nrow(d_model) < 10 || length(demo_pct_avail) == 0) {
        d[["predicted_proficiency"]] <- mean(d[["pct_proficient"]], na.rm = TRUE)
        d[["residual"]]              <- d[["pct_proficient"]] - d[["predicted_proficiency"]]
      } else {
        formula_str <- paste("pct_proficient ~", paste(demo_pct_avail, collapse = " + "))
        fit         <- tryCatch(stats::lm(as.formula(formula_str), data = d_model),
                                error = function(e) NULL)
        if (!is.null(fit)) {
          d[["predicted_proficiency"]] <- stats::predict(fit, newdata = d)
          d[["residual"]]              <- d[["pct_proficient"]] - d[["predicted_proficiency"]]
        } else {
          d[["predicted_proficiency"]] <- mean(d[["pct_proficient"]], na.rm = TRUE)
          d[["residual"]]              <- d[["pct_proficient"]] - d[["predicted_proficiency"]]
        }
      }
      sd_resid <- stats::sd(d[["residual"]], na.rm = TRUE)
      d[["sd_residual"]] <- sd_resid
      d[["outlier_flag"]] <- dplyr::case_when(
        is.na(d[["residual"]])                               ~ "typical",
        d[["residual"]] >  outlier_sd_threshold * sd_resid  ~ "high",
        d[["residual"]] < -outlier_sd_threshold * sd_resid  ~ "low",
        TRUE ~ "typical"
      )
      d
    }) |>
    dplyr::ungroup()

  positive_outlier_schools <- residual_results |>
    dplyr::filter(.data[["outlier_flag"]] == "high") |>
    dplyr::arrange(dplyr::desc(.data[["residual"]]))

  negative_outlier_schools <- residual_results |>
    dplyr::filter(.data[["outlier_flag"]] == "low") |>
    dplyr::arrange(.data[["residual"]])

  # ------------------------------------------------------------------
  # 2. Outlier stability: how many years is each school flagged?
  # ------------------------------------------------------------------
  stability_grp <- c(school_id, "SCHOOL_NAME", "DISTRICT_NAME", subj_col)
  stability_grp <- intersect(stability_grp, names(residual_results))

  outlier_stability <- residual_results |>
    dplyr::filter(.data[["outlier_flag"]] != "typical") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(stability_grp))) |>
    dplyr::summarize(
      n_years_flagged = dplyr::n(),
      direction       = dplyr::if_else(
        mean(.data[["residual"]], na.rm = TRUE) > 0, "high", "low"
      ),
      mean_residual   = round(mean(.data[["residual"]], na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data[["n_years_flagged"]]),
                   dplyr::desc(abs(.data[["mean_residual"]])))

  # ------------------------------------------------------------------
  # 3. Funnel plot data
  # ------------------------------------------------------------------
  funnel_grp <- c(subj_col, "YEAR")
  funnel_grp <- intersect(funnel_grp, names(school_yr_subj))

  funnel_plot_data <- school_yr_subj |>
    dplyr::filter(.data[["n_tested"]] >= outlier_n_funnel,
                  !is.na(.data[["pct_proficient"]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(funnel_grp))) |>
    dplyr::mutate(
      state_pct  = stats::weighted.mean(.data[["pct_proficient"]],
                                         .data[["n_tested"]], na.rm = TRUE) / 100,
      upper_95   = 100 * (.data[["state_pct"]] +
                            1.96 * sqrt(.data[["state_pct"]] * (1 - .data[["state_pct"]]) /
                                          .data[["n_tested"]])),
      lower_95   = 100 * (.data[["state_pct"]] -
                            1.96 * sqrt(.data[["state_pct"]] * (1 - .data[["state_pct"]]) /
                                          .data[["n_tested"]])),
      upper_99   = 100 * (.data[["state_pct"]] +
                            3    * sqrt(.data[["state_pct"]] * (1 - .data[["state_pct"]]) /
                                          .data[["n_tested"]])),
      lower_99   = 100 * (.data[["state_pct"]] -
                            3    * sqrt(.data[["state_pct"]] * (1 - .data[["state_pct"]]) /
                                          .data[["n_tested"]])),
      state_pct  = round(.data[["state_pct"]] * 100, 1)
    ) |>
    dplyr::ungroup()

  # [REQUIRES school_results_long — NOT RUN]
  # outlier_characteristics joins CO_OP, LOCALE, SCHOOL_LEVEL, SCHOOL_TYPE,
  # HQIM_FLAG, LITERACY_COACH_FLAG, and ACCOUNTABILITY_GRADE to flagged schools.
  list(
    residual_model_results   = residual_results,
    positive_outlier_schools = positive_outlier_schools,
    negative_outlier_schools = negative_outlier_schools,
    outlier_stability        = outlier_stability,
    funnel_plot_data         = funnel_plot_data,
    outlier_characteristics  = NULL  # [REQUIRES school_results_long]
  )
}


# ==============================================================================
# compute_school_disaggregated()
# ==============================================================================

#' Compute school-level disaggregated proficiency and within-school gaps
#'
#' Aggregates `student_results_long` to the school level by demographic group
#' and computes proficiency rates, within-school gap estimates, comparisons to
#' statewide subgroup averages, and gap trend classifications.
#'
#' @param data A long-format student results data frame.
#' @param spec Named list describing the assessment program.
#' @param min_n_subgroup Integer. Minimum students per subgroup cell. Default `10`.
#'
#' @return A named list:
#'   * `$school_subgroup_proficiency` — school × year × subject × grade × group
#'   * `$district_subgroup_proficiency` — district × year × subject × grade × group
#'   * `$within_school_gaps` — school × year × subject × gap_pair: gap_estimate
#'   * `$subgroup_vs_state_comparison` — school × year × subject × group: delta
#'   * `$gap_trend_classification` — school × subject × gap_pair: direction
#' @export
compute_school_disaggregated <- function(data,
                                          spec,
                                          min_n_subgroup = 10) {
  stopifnot(
    is.data.frame(data),
    is.list(spec),
    all(c("YEAR", "ACHIEVEMENT_LEVEL") %in% names(data))
  )

  cols        <- .resolve_agg_cols(data)
  school_id   <- cols$school_id
  district_id <- cols$district_id
  subj_col    <- cols$subject

  data <- .add_proficient(data, spec)

  binary_demos <- names(Filter(function(d) d$type == "binary", spec$demographics))
  binary_avail <- intersect(binary_demos, names(data))
  yes_vals     <- c("YES", "Y", "TRUE", "1")

  school_label_cols <- intersect("SCHOOL_NAME", names(data))
  dist_label_cols   <- intersect("DISTRICT_NAME", names(data))

  # ------------------------------------------------------------------
  # 1. School subgroup proficiency (one table across all binary demos)
  # ------------------------------------------------------------------
  subgroup_rows <- lapply(binary_avail, function(var) {
    data |>
      dplyr::filter(!is.na(.data[[school_id]]),
                    !is.na(.data[[var]])) |>
      dplyr::mutate(
        demo_var = var,
        demo_val = dplyr::if_else(
          toupper(as.character(.data[[var]])) %in% yes_vals, "Yes", "No"
        )
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(
        c(school_id, school_label_cols, district_id, dist_label_cols,
          "YEAR", subj_col, "GRADE", "demo_var", "demo_val")
      ))) |>
      dplyr::summarize(
        N            = dplyr::n(),
        n_proficient = sum(.data[["PROFICIENT"]], na.rm = TRUE),
        .groups      = "drop"
      ) |>
      dplyr::mutate(
        pct_proficient = dplyr::if_else(
          .data[["N"]] >= min_n_subgroup,
          100 * .data[["n_proficient"]] / .data[["N"]],
          NA_real_
        )
      )
  })

  school_subgroup_proficiency <- do.call(rbind, subgroup_rows)

  # ------------------------------------------------------------------
  # 2. District subgroup proficiency
  # ------------------------------------------------------------------
  if (!is.na(district_id) && nrow(school_subgroup_proficiency) > 0) {
    dist_sg_grp <- c(district_id, dist_label_cols, "YEAR", subj_col, "GRADE",
                     "demo_var", "demo_val")
    dist_sg_grp <- intersect(dist_sg_grp, names(school_subgroup_proficiency))

    district_subgroup_proficiency <- school_subgroup_proficiency |>
      dplyr::group_by(dplyr::across(dplyr::all_of(dist_sg_grp))) |>
      dplyr::summarize(
        N            = sum(.data[["N"]], na.rm = TRUE),
        n_proficient = sum(.data[["n_proficient"]], na.rm = TRUE),
        .groups      = "drop"
      ) |>
      dplyr::mutate(
        pct_proficient = dplyr::if_else(
          .data[["N"]] >= min_n_subgroup,
          100 * .data[["n_proficient"]] / .data[["N"]],
          NA_real_
        )
      )
  } else {
    district_subgroup_proficiency <- data.frame()
  }

  # ------------------------------------------------------------------
  # 3. Within-school gaps (binary: Yes group vs. No group)
  # ------------------------------------------------------------------
  within_school_gaps <- if (nrow(school_subgroup_proficiency) > 0) {
    gap_pairs <- lapply(binary_avail, function(var) {
      sg <- school_subgroup_proficiency |>
        dplyr::filter(.data[["demo_var"]] == var) |>
        dplyr::select(
          dplyr::all_of(c(school_id, school_label_cols, district_id,
                           dist_label_cols, "YEAR", subj_col, "GRADE",
                           "demo_val", "pct_proficient"))
        ) |>
        tidyr::pivot_wider(
          names_from  = "demo_val",
          values_from = "pct_proficient",
          names_prefix = "pct_"
        )

      # Gap = No group (advantaged) minus Yes group (disadvantaged)
      if (all(c("pct_No", "pct_Yes") %in% names(sg))) {
        sg |>
          dplyr::mutate(
            gap_pair     = paste0("No", var, "_vs_", var),
            gap_estimate = .data[["pct_No"]] - .data[["pct_Yes"]]
          ) |>
          dplyr::select(-"pct_No", -"pct_Yes")
      } else {
        NULL
      }
    })
    do.call(rbind, Filter(Negate(is.null), gap_pairs))
  } else {
    data.frame()
  }

  # ------------------------------------------------------------------
  # 4. Subgroup vs. state average
  # ------------------------------------------------------------------
  subgroup_vs_state_comparison <- if (nrow(school_subgroup_proficiency) > 0) {
    state_avgs <- school_subgroup_proficiency |>
      dplyr::group_by(dplyr::across(dplyr::all_of(
        c("YEAR", subj_col, "GRADE", "demo_var", "demo_val")
      ))) |>
      dplyr::summarize(
        state_pct_proficient = dplyr::if_else(
          sum(.data[["N"]], na.rm = TRUE) >= min_n_subgroup,
          100 * sum(.data[["n_proficient"]], na.rm = TRUE) /
            sum(.data[["N"]], na.rm = TRUE),
          NA_real_
        ),
        .groups = "drop"
      )

    join_key <- c("YEAR", subj_col, "GRADE", "demo_var", "demo_val")
    join_key <- intersect(join_key, names(school_subgroup_proficiency))

    school_subgroup_proficiency |>
      dplyr::left_join(state_avgs, by = join_key) |>
      dplyr::mutate(
        delta_vs_state = .data[["pct_proficient"]] - .data[["state_pct_proficient"]]
      )
  } else {
    data.frame()
  }

  # ------------------------------------------------------------------
  # 5. Gap trend classification
  # ------------------------------------------------------------------
  gap_trend_classification <- if (nrow(within_school_gaps) > 0) {
    trend_grp <- c(school_id, school_label_cols, district_id, dist_label_cols,
                   subj_col, "gap_pair")
    trend_grp <- intersect(trend_grp, names(within_school_gaps))

    within_school_gaps |>
      dplyr::filter(!is.na(.data[["gap_estimate"]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(trend_grp))) |>
      dplyr::group_modify(function(d, k) {
        d_clean <- d[!is.na(d[["gap_estimate"]]), ]
        if (nrow(d_clean) < 2) {
          return(data.frame(trend_direction = "insufficient data",
                            slope = NA_real_, stringsAsFactors = FALSE))
        }
        tryCatch({
          fit   <- stats::lm(gap_estimate ~ YEAR, data = d_clean)
          slope <- stats::coef(fit)[["YEAR"]]
          dir   <- dplyr::case_when(
            abs(slope) < 0.5         ~ "stable",
            slope < 0                ~ "narrowing",
            TRUE                     ~ "widening"
          )
          data.frame(trend_direction = dir, slope = round(slope, 3),
                     stringsAsFactors = FALSE)
        }, error = function(e) {
          data.frame(trend_direction = "insufficient data",
                     slope = NA_real_, stringsAsFactors = FALSE)
        })
      }) |>
      dplyr::ungroup()
  } else {
    data.frame()
  }

  list(
    school_subgroup_proficiency    = school_subgroup_proficiency,
    district_subgroup_proficiency  = district_subgroup_proficiency,
    within_school_gaps             = within_school_gaps,
    subgroup_vs_state_comparison   = subgroup_vs_state_comparison,
    gap_trend_classification       = gap_trend_classification
  )
}


# ==============================================================================
# compute_stability_analysis()
# ==============================================================================

#' Compute year-over-year rank-order and correlation stability
#'
#' Derives stability metrics from the already-computed performance trends.
#' Requires no additional raw student data.
#'
#' @param performance_data Named list returned by [compute_performance_trends()].
#' @param spec Named list describing the assessment program.
#' @param correlation_flag_threshold Numeric. Spearman r threshold for flagging
#'   unstable year pairs. Default `0.85`.
#'
#' @return A named list:
#'   * `$year_to_year_correlations` — from performance_data (passed through)
#'   * `$rank_order_data` — from performance_data (passed through)
#'   * `$correlation_flag_summary` — year pairs where flagged == TRUE
#'   * `$icc_trends` — year × subject: icc averaged across grades
#' @export
compute_stability_analysis <- function(performance_data,
                                        spec,
                                        correlation_flag_threshold = 0.85) {
  stopifnot(
    is.list(performance_data),
    is.list(spec)
  )

  corr_tbl <- performance_data$year_to_year_correlations

  # Flag any unflagged rows that fall below threshold
  if (!is.null(corr_tbl) && nrow(corr_tbl) > 0 &&
      "spearman_r" %in% names(corr_tbl)) {
    if (!"flagged" %in% names(corr_tbl)) {
      corr_tbl <- corr_tbl |>
        dplyr::mutate(flagged = !is.na(.data[["spearman_r"]]) &
                        .data[["spearman_r"]] < correlation_flag_threshold)
    }
  }

  correlation_flag_summary <- if (!is.null(corr_tbl) && nrow(corr_tbl) > 0 &&
                                   "flagged" %in% names(corr_tbl)) {
    corr_tbl |> dplyr::filter(.data[["flagged"]])
  } else {
    data.frame()
  }

  # ICC trends: collapse icc_tables across grades (take mean per year × subject)
  icc_trends <- if (!is.null(performance_data$icc_tables) &&
                     nrow(performance_data$icc_tables) > 0 &&
                     "icc" %in% names(performance_data$icc_tables)) {
    cols    <- .resolve_agg_cols(performance_data$icc_tables)
    subj_col <- cols$subject

    trend_grp <- c("YEAR", subj_col)
    trend_grp <- intersect(trend_grp, names(performance_data$icc_tables))

    performance_data$icc_tables |>
      dplyr::filter(!is.na(.data[["icc"]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(trend_grp))) |>
      dplyr::summarize(
        icc         = round(mean(.data[["icc"]],         na.rm = TRUE), 4),
        var_between = round(mean(.data[["var_between"]], na.rm = TRUE), 2),
        var_within  = round(mean(.data[["var_within"]],  na.rm = TRUE), 2),
        n_grades    = dplyr::n(),
        .groups     = "drop"
      )
  } else {
    data.frame()
  }

  list(
    year_to_year_correlations = corr_tbl,
    rank_order_data           = performance_data$rank_order_data,
    correlation_flag_summary  = correlation_flag_summary,
    icc_trends                = icc_trends
  )
}
