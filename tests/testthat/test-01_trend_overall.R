library(testthat)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------
make_trend_data <- function(n = 50, years = 2022:2024) {
  nr <- n * length(years) * 2  # 2 grades
  set.seed(7)
  data.frame(
    STUDENT_ID        = paste0("S", seq_len(nr)),
    YEAR              = rep(years, each = n * 2),
    GRADE             = rep(rep(c(3L, 4L), each = n), times = length(years)),
    SUBJECT           = "ELA",
    SCALE_SCORE       = stats::rnorm(nr, mean = 355, sd = 20),
    ACHIEVEMENT_LEVEL = "Passing",
    stringsAsFactors  = FALSE
  )
}

make_trend_spec <- function() {
  list(
    subjects = list(
      ELA = list(label = "English Language Arts", grades = 3:4)
    ),
    achievement_levels = list(
      labels           = c("Basic", "Proficient", "Advanced"),
      policy_benchmark = "Proficient"
    ),
    scale_scores = list(
      ELA_3 = list(loss = 300, hoss = 399, cuts = c(340, 370)),
      ELA_4 = list(loss = 400, hoss = 499, cuts = c(440, 470))
    ),
    years = list(tested_years = 2022:2024, cohort_anchor_grade = 3),
    demographics = list()
  )
}

# ---------------------------------------------------------------------------
# compute_score_summary() tests
# ---------------------------------------------------------------------------

test_that("compute_score_summary returns a list with expected elements", {
  d    <- make_trend_data()
  spec <- make_trend_spec()
  out  <- compute_score_summary(d, spec)
  expect_type(out, "list")
  expect_true(all(c("ScaleScore_Stats_Long", "ScaleScore_Stats_Wide",
                    "ScaleScore_Stats_Report", "ScaleScore_Counts") %in% names(out)))
})

test_that("ScaleScore_Stats_Wide has mean, sd, and percentile columns", {
  d    <- make_trend_data()
  spec <- make_trend_spec()
  out  <- compute_score_summary(d, spec)
  wide <- out$ScaleScore_Stats_Wide
  expect_s3_class(wide, "data.frame")
  expect_true(all(c("mean", "sd", "p05", "p50", "p95") %in% names(wide)))
})

test_that("ScaleScore_Stats_Report has year-grouped column blocks", {
  d    <- make_trend_data()
  spec <- make_trend_spec()
  out  <- compute_score_summary(d, spec)
  rpt  <- out$ScaleScore_Stats_Report
  expect_s3_class(rpt, "data.frame")
  # Should have columns like 2022_Mean, 2022_SD, 2022_N
  expect_true(any(grepl("_Mean$", names(rpt))))
  expect_true(any(grepl("_SD$", names(rpt))))
  expect_true(any(grepl("_N$", names(rpt))))
})

test_that("small-n suppression produces NA in stats, not dropped rows", {
  d    <- make_trend_data(n = 5)  # 5 per group — below default min_n
  spec <- make_trend_spec()
  out  <- compute_score_summary(d, spec, min_n = 50)
  wide <- out$ScaleScore_Stats_Wide
  expect_true(all(is.na(wide$mean)))
  expect_gt(nrow(wide), 0)
})

# ---------------------------------------------------------------------------
# mean_sd_table_wide() tests
# ---------------------------------------------------------------------------

test_that("mean_sd_table_wide returns a data frame", {
  d    <- make_trend_data()
  spec <- make_trend_spec()
  tbl  <- mean_sd_table_wide(d, spec, subject = "ELA")
  expect_s3_class(tbl, "data.frame")
})

test_that("mean_sd_table_wide filters to the requested subject", {
  d    <- make_trend_data()
  spec <- make_trend_spec()
  tbl  <- mean_sd_table_wide(d, spec, subject = "ELA")
  expect_true("SUBJECT" %in% names(tbl))
  expect_true(all(tbl$SUBJECT == "ELA"))
})
