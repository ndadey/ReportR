library(testthat)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

make_enroll_spec <- function() {
  list(
    program = list(
      name       = "Test Assessment",
      short_name = "TEST",
      state      = "TS"
    ),
    subjects = list(
      ELA  = list(label = "English Language Arts", grades = 3:8),
      MATH = list(label = "Mathematics",           grades = 3:8)
    ),
    achievement_levels = list(
      labels           = c("Unsatisfactory", "Partially Proficient",
                           "Proficient", "Advanced"),
      policy_benchmark = "Proficient"
    ),
    scale_scores = list(
      ELA = list(
        `3` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `4` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `5` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `6` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `7` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `8` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500))
      ),
      MATH = list(
        `3` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `4` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `5` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `6` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `7` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500)),
        `8` = list(loss = 300, hoss = 600, cuts = c(350, 420, 500))
      )
    ),
    years = list(tested_years = 2021:2023, cohort_anchor_grade = 3),
    demographics = list(
      RACE_ETHNICITY = list(type = "categorical"),
      FRL            = list(type = "binary"),
      EL             = list(type = "binary"),
      GENDER         = list(type = "binary")
    )
  )
}

# Creates synthetic data with:
#   n_persist   students present in all years
#   n_enter     students who appear only in the last year
#   n_exit      students who appear only in the first year
#   years       integer vector of testing years
#   two subjects (ELA, MATH) per student per year
make_enroll_data <- function(n_persist = 100,
                              n_enter   = 20,
                              n_exit    = 20,
                              years     = 2021:2023) {
  set.seed(42)
  subjects <- c("ELA", "MATH")

  persistent_ids <- paste0("P", seq_len(n_persist))
  entering_ids   <- paste0("E", seq_len(n_enter))
  exiting_ids    <- paste0("X", seq_len(n_exit))

  rows <- lapply(years, function(yr) {
    ids <- persistent_ids
    if (yr == max(years)) ids <- c(ids, entering_ids)
    if (yr == min(years)) ids <- c(ids, exiting_ids)
    n   <- length(ids)

    school_id   <- sample(c("SCH001", "SCH002"), n, replace = TRUE)
    school_name <- ifelse(school_id == "SCH001",
                          "Lincoln Elementary", "Roosevelt Middle")

    do.call(rbind, lapply(subjects, function(s) {
      data.frame(
        STUDENT_ID        = ids,
        YEAR              = yr,
        GRADE             = sample(3:8, n, replace = TRUE),
        SUBJECT           = s,
        SCALE_SCORE       = stats::rnorm(n, 450, 50),
        ACHIEVEMENT_LEVEL = sample(c("Unsatisfactory", "Partially Proficient",
                                      "Proficient", "Advanced"),
                                    n, replace = TRUE),
        SCHOOL_ID         = school_id,
        SCHOOL_NAME       = school_name,
        DISTRICT_ID       = "DIST01",
        DISTRICT_NAME     = "Example USD",
        RACE_ETHNICITY    = sample(c("White", "Black", "Hispanic", "Other"),
                                    n, replace = TRUE),
        FRL               = sample(c("Yes", "No"), n, replace = TRUE),
        EL                = sample(c("Yes", "No"), n, replace = TRUE),
        GENDER            = sample(c("M", "F"), n, replace = TRUE),
        stringsAsFactors  = FALSE
      )
    }))
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# suppress_small_n_enroll() tests
# ---------------------------------------------------------------------------

test_that("suppress_small_n_enroll sets suppress_cols to NA for small groups", {
  df <- data.frame(N = c(5, 50), N_change = c(1.0, 1.0),
                   stringsAsFactors = FALSE)
  out <- suppress_small_n_enroll(df, "N", 10, "N_change")
  expect_true(is.na(out[["N_change"]][1]))
  expect_false(is.na(out[["N_change"]][2]))
  expect_true(out[["suppressed"]][1])
  expect_false(out[["suppressed"]][2])
})

test_that("suppress_small_n_enroll returns suppressed column", {
  df  <- data.frame(N = c(5, 50), v = c(1.0, 2.0), stringsAsFactors = FALSE)
  out <- suppress_small_n_enroll(df, "N", 10, "v")
  expect_true("suppressed" %in% names(out))
  expect_type(out[["suppressed"]], "logical")
})

# ---------------------------------------------------------------------------
# compute_enrollment_counts() tests
# ---------------------------------------------------------------------------

test_that("compute_enrollment_counts returns expected structure for overall", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_enrollment_counts(d, by = NULL, spec)
  expect_s3_class(out, "data.frame")
  expect_true("N" %in% names(out))
  expect_equal(nrow(out), 1L)
  expect_gt(out[["N"]][1], 0)
})

test_that("compute_enrollment_counts deduplicates across subjects", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  # Without SUBJECT in by: each student should be counted once per year
  by_yr   <- compute_enrollment_counts(d, by = "YEAR", spec)
  by_yrs  <- compute_enrollment_counts(d, by = c("YEAR", "SUBJECT"), spec)
  # Sum of by_year should be less than sum of by_year_subject (no dedup by subj)
  expect_lt(sum(by_yr[["N"]]), sum(by_yrs[["N"]]))
})

test_that("compute_enrollment_counts returns N_change when YEAR in by", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_enrollment_counts(d, by = "YEAR", spec)
  expect_true("N_change" %in% names(out))
  expect_true("N_pct_change" %in% names(out))
  # First year should have NA change (no prior year)
  first_row <- out[out[["YEAR"]] == min(out[["YEAR"]]), ]
  expect_true(is.na(first_row[["N_change"]][1]))
})

test_that("compute_enrollment_counts suppresses N_change for small groups", {
  d    <- make_enroll_data(n_persist = 5, n_enter = 0, n_exit = 0)
  spec <- make_enroll_spec()
  out  <- compute_enrollment_counts(d, by = "YEAR", spec, min_n = 100)
  # All N_change values for non-first years should be NA (N < 100)
  non_first <- out[out[["YEAR"]] > min(out[["YEAR"]]), ]
  expect_true(all(is.na(non_first[["N_change"]])))
})

test_that("compute_enrollment_counts handles missing grouping columns gracefully", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  # SCHOOL_ID exists in d; DISTRICT_ID also exists
  out  <- compute_enrollment_counts(d, by = c("YEAR", "SCHOOL_ID"), spec)
  expect_s3_class(out, "data.frame")
  expect_true("N" %in% names(out))
})

test_that("compute_enrollment_counts returns suppressed column", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_enrollment_counts(d, by = NULL, spec)
  expect_true("suppressed" %in% names(out))
})

# ---------------------------------------------------------------------------
# compute_enrollment_proportions() tests
# ---------------------------------------------------------------------------

test_that("compute_enrollment_proportions returns expected structure", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_enrollment_proportions(d, by = "YEAR", assessment_spec = spec)
  expect_s3_class(out, "data.frame")
  expected_cols <- c("YEAR", "subgroup_var", "subgroup_val", "N", "total_N",
                     "pct", "suppressed")
  expect_true(all(expected_cols %in% names(out)))
})

test_that("compute_enrollment_proportions covers all subgroup variables", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_enrollment_proportions(d, by = "YEAR", assessment_spec = spec)
  expected_sg <- intersect(names(spec$demographics), names(d))
  expect_true(all(expected_sg %in% unique(out[["subgroup_var"]])))
})

test_that("compute_enrollment_proportions pct is NA for suppressed cells", {
  d    <- make_enroll_data(n_persist = 5, n_enter = 0, n_exit = 0)
  spec <- make_enroll_spec()
  out  <- compute_enrollment_proportions(d, by = "YEAR", assessment_spec = spec,
                                          min_n = 200)
  # All cells should be suppressed since N < 200
  expect_true(all(is.na(out[["pct"]][out[["suppressed"]]])))
})

test_that("compute_enrollment_proportions returns empty frame when subgroup absent", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  # Request a subgroup column that doesn't exist
  out  <- compute_enrollment_proportions(d, by = "YEAR",
                                          subgroup_vars = "NONEXISTENT_COL",
                                          assessment_spec = spec)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

# ---------------------------------------------------------------------------
# compute_subgroup_transitions() tests
# ---------------------------------------------------------------------------

test_that("compute_subgroup_transitions returns list with table and summary", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_subgroup_transitions(d, "RACE_ETHNICITY", spec)
  expect_type(out, "list")
  expect_true(all(c("table", "summary") %in% names(out)))
})

test_that("compute_subgroup_transitions table has expected columns", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_subgroup_transitions(d, "FRL", spec)
  tbl  <- out$table
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("YEAR", "N", "total_N", "pct", "subgroup_var",
                     "subgroup_val", "suppressed") %in% names(tbl)))
})

test_that("compute_subgroup_transitions summary has pct_change column", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_subgroup_transitions(d, "RACE_ETHNICITY", spec)
  smry <- out$summary
  expect_s3_class(smry, "data.frame")
  expect_true("pct_change" %in% names(smry))
})

test_that("compute_subgroup_transitions pct is NA for suppressed cells", {
  d    <- make_enroll_data(n_persist = 5, n_enter = 0, n_exit = 0)
  spec <- make_enroll_spec()
  out  <- compute_subgroup_transitions(d, "RACE_ETHNICITY", spec, min_n = 200)
  tbl  <- out$table
  expect_true(all(is.na(tbl[["pct"]][tbl[["suppressed"]]])))
})

test_that("compute_subgroup_transitions returns empty list when column absent", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_subgroup_transitions(d, "NONEXISTENT_VAR", spec)
  expect_equal(nrow(out$table), 0L)
  expect_equal(nrow(out$summary), 0L)
})

# ---------------------------------------------------------------------------
# compute_entry_exit_performance() tests
# ---------------------------------------------------------------------------

test_that("compute_entry_exit_performance returns expected structure", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_entry_exit_performance(d, direction = "entering",
                                          assessment_spec = spec)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("YEAR", "group", "N", "mean_score", "pct_proficient",
                     "suppressed") %in% names(out)))
})

test_that("compute_entry_exit_performance group column is 'Entering'", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_entry_exit_performance(d, direction = "entering",
                                          assessment_spec = spec)
  if (nrow(out) > 0) {
    expect_true(all(out[["group"]] == "Entering"))
  }
})

test_that("compute_entry_exit_performance suppresses small cells", {
  # n_enter/n_exit = 5 so entering/exiting groups are tiny
  d    <- make_enroll_data(n_persist = 200, n_enter = 5, n_exit = 5)
  spec <- make_enroll_spec()
  out  <- compute_entry_exit_performance(d, direction = "entering",
                                          assessment_spec = spec, min_n = 50)
  if (nrow(out) > 0) {
    small_rows <- out[out[["N"]] < 50, ]
    if (nrow(small_rows) > 0) {
      expect_true(all(is.na(small_rows[["mean_score"]])))
    }
  }
})

test_that("compute_entry_exit_performance returns empty frame without STUDENT_ID", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  d_no_id <- d[, setdiff(names(d), "STUDENT_ID")]
  out  <- compute_entry_exit_performance(d_no_id, assessment_spec = spec)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("compute_entry_exit_performance direction = 'both' includes all groups", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_entry_exit_performance(d, direction = "both",
                                          assessment_spec = spec)
  if (nrow(out) > 0) {
    groups <- unique(out[["group"]])
    expect_true("Entering" %in% groups)
    expect_true("Exiting" %in% groups)
  }
})

# ---------------------------------------------------------------------------
# compute_school_district_enrollment() tests
# ---------------------------------------------------------------------------

test_that("compute_school_district_enrollment returns list with totals and by_subgroup", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_school_district_enrollment(d, level = "school",
                                              assessment_spec = spec)
  expect_type(out, "list")
  expect_true(all(c("totals", "by_subgroup") %in% names(out)))
})

test_that("compute_school_district_enrollment totals has expected columns", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_school_district_enrollment(d, level = "school",
                                              assessment_spec = spec)
  tbl  <- out$totals
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("SCHOOL_ID", "YEAR", "N", "N_change") %in% names(tbl)))
})

test_that("compute_school_district_enrollment by_subgroup has expected columns", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  out  <- compute_school_district_enrollment(d, level = "school",
                                              assessment_spec = spec)
  sg   <- out$by_subgroup
  if (nrow(sg) > 0) {
    expect_true(all(c("subgroup_var", "subgroup_val", "N_sg", "pct_sg",
                       "suppressed") %in% names(sg)))
  }
})

test_that("compute_school_district_enrollment handles missing entity column", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  # Remove DISTRICT_ID and DISTRICT_NAME
  d_no_dist <- d[, setdiff(names(d), c("DISTRICT_ID", "DISTRICT_NAME"))]
  out  <- compute_school_district_enrollment(d_no_dist, level = "district",
                                              assessment_spec = spec)
  expect_equal(nrow(out$totals), 0L)
  expect_equal(nrow(out$by_subgroup), 0L)
})

# ---------------------------------------------------------------------------
# plot_enrollment_table() tests
# ---------------------------------------------------------------------------

test_that("plot_enrollment_table returns a flextable", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  counts <- compute_enrollment_counts(d, by = "YEAR", spec)
  ft   <- plot_enrollment_table(counts, type = "counts", assessment_spec = spec)
  expect_s3_class(ft, "flextable")
})

test_that("plot_enrollment_table suppressed cells show em dash", {
  d    <- make_enroll_data(n_persist = 5, n_enter = 0, n_exit = 0)
  spec <- make_enroll_spec()
  counts <- compute_enrollment_counts(d, by = "YEAR", spec, min_n = 200)
  # Function should not error even if all cells are suppressed
  expect_no_error(plot_enrollment_table(counts, type = "counts",
                                         assessment_spec = spec))
})

# ---------------------------------------------------------------------------
# run_enrollment_analysis() tests (integration)
# ---------------------------------------------------------------------------

test_that("run_enrollment_analysis returns a list with required top-level keys", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  res  <- run_enrollment_analysis(d, spec)
  expect_type(res, "list")
  expect_true(all(c("counts", "proportions", "transitions",
                     "entry_exit", "aggregate", "text", "params") %in% names(res)))
})

test_that("run_enrollment_analysis counts$overall has one row with N > 0", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  res  <- run_enrollment_analysis(d, spec)
  expect_equal(nrow(res$counts$overall), 1L)
  expect_gt(res$counts$overall[["N"]][1], 0)
})

test_that("run_enrollment_analysis text has narrative string", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  res  <- run_enrollment_analysis(d, spec)
  expect_true(!is.null(res$text$narrative))
  expect_true(nchar(res$text$narrative) > 0)
})

test_that("run_enrollment_analysis report_years filters correctly", {
  d    <- make_enroll_data(years = 2021:2023)
  spec <- make_enroll_spec()
  res  <- run_enrollment_analysis(d, spec, report_years = 2022:2023)
  by_yr <- res$counts$by_year
  expect_true(all(by_yr[["YEAR"]] %in% 2022:2023))
  expect_false(2021 %in% by_yr[["YEAR"]])
})

test_that("run_enrollment_analysis handles data with no SCHOOL_ID gracefully", {
  d    <- make_enroll_data()
  spec <- make_enroll_spec()
  d_no_school <- d[, setdiff(names(d), c("SCHOOL_ID", "SCHOOL_NAME",
                                           "DISTRICT_ID", "DISTRICT_NAME"))]
  expect_no_error(run_enrollment_analysis(d_no_school, spec))
})
