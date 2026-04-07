library(testthat)

# ---------------------------------------------------------------------------
# Minimal synthetic data fixture
# ---------------------------------------------------------------------------
make_test_data <- function(n = 200, years = 2023:2024, subject = "ELA",
                           grades = 3:4) {
  nr <- n * length(years) * length(grades)
  set.seed(42)
  data.frame(
    STUDENT_ID        = paste0("S", seq_len(nr)),
    YEAR              = rep(years,  each = n * length(grades)),
    GRADE             = rep(rep(grades, each = n), times = length(years)),
    SUBJECT           = subject,
    SCALE_SCORE       = stats::rnorm(nr, mean = 355, sd = 20),
    ACHIEVEMENT_LEVEL = "Passing",
    SCHOOL_ID         = "SCH001",
    DISTRICT_ID       = "DIST01",
    stringsAsFactors  = FALSE
  )
}

# Assessment spec fixture (2 subjects, 2 grades, 3 levels)
make_test_spec <- function() {
  list(
    subjects = list(
      ELA  = list(label = "English Language Arts", grades = 3:4),
      MATH = list(label = "Mathematics",           grades = 3:4)
    ),
    achievement_levels = list(
      labels           = c("Basic", "Proficient", "Advanced"),
      policy_benchmark = "Proficient"
    ),
    scale_scores = list(
      ELA_3  = list(loss = 300, hoss = 399, cuts = c(340, 370)),
      ELA_4  = list(loss = 400, hoss = 499, cuts = c(440, 470)),
      MATH_3 = list(loss = 300, hoss = 399, cuts = c(338, 368)),
      MATH_4 = list(loss = 400, hoss = 499, cuts = c(435, 465))
    ),
    years = list(tested_years = 2023:2024, cohort_anchor_grade = 3),
    demographics = list()
  )
}

# ---------------------------------------------------------------------------
# compute_loss_hoss_table() tests
# ---------------------------------------------------------------------------

test_that("compute_loss_hoss_table returns a data frame", {
  d    <- make_test_data()
  spec <- make_test_spec()
  result <- compute_loss_hoss_table(d, spec)
  expect_s3_class(result, "data.frame")
})

test_that("compute_loss_hoss_table has expected columns", {
  d      <- make_test_data()
  spec   <- make_test_spec()
  result <- compute_loss_hoss_table(d, spec)
  expect_true(all(c("YEAR", "SUBJECT", "GRADE", "n",
                    "pct_near_loss", "pct_near_hoss",
                    "loss_flag", "hoss_flag") %in% names(result)))
})

test_that("small-n suppression sets pct values to NA, not drops rows", {
  d      <- make_test_data(n = 5)  # 5 students per year/grade — below min_n = 10
  spec   <- make_test_spec()
  result <- compute_loss_hoss_table(d, spec, min_n = 10)
  expect_true(all(is.na(result$pct_near_loss)))
  expect_true(all(is.na(result$pct_near_hoss)))
  expect_gt(nrow(result), 0)  # rows not dropped
})

test_that("large-n groups are not suppressed", {
  d      <- make_test_data(n = 100)
  spec   <- make_test_spec()
  result <- compute_loss_hoss_table(d, spec, min_n = 10)
  expect_false(all(is.na(result$pct_near_loss)))
})

test_that("hoss_flag and loss_flag take only expected values", {
  d      <- make_test_data()
  spec   <- make_test_spec()
  result <- compute_loss_hoss_table(d, spec)
  valid  <- c("None", "Mild", "Moderate", "Severe", NA_character_)
  expect_true(all(result$hoss_flag %in% valid))
  expect_true(all(result$loss_flag %in% valid))
})

test_that("compute_loss_hoss_table works with a 3-level spec (variable level count)", {
  d      <- make_test_data()
  spec   <- make_test_spec()  # 3 levels
  result <- compute_loss_hoss_table(d, spec)
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

# ---------------------------------------------------------------------------
# summarize_loss_hoss_findings() tests
# ---------------------------------------------------------------------------

test_that("summarize_loss_hoss_findings returns list with expected names", {
  d    <- make_test_data()
  spec <- make_test_spec()
  tbl  <- compute_loss_hoss_table(d, spec)
  txt  <- summarize_loss_hoss_findings(tbl)
  expect_type(txt, "list")
  expect_true(all(c("table", "by_subject", "narrative") %in% names(txt)))
})

test_that("summarize_loss_hoss_findings$narrative is a character vector", {
  d    <- make_test_data()
  spec <- make_test_spec()
  tbl  <- compute_loss_hoss_table(d, spec)
  txt  <- summarize_loss_hoss_findings(tbl)
  expect_type(txt$narrative, "character")
  expect_gte(length(txt$narrative), 1L)
})

# ---------------------------------------------------------------------------
# run_loss_hoss() integration test
# ---------------------------------------------------------------------------

test_that("run_loss_hoss returns sub-list with all expected elements", {
  d    <- make_test_data()
  spec <- make_test_spec()
  sub  <- run_loss_hoss(d, spec)
  expected <- c("table", "plot_hoss_cleveland", "plot_loss_cleveland",
                "plot_hoss_heat", "plot_loss_heat",
                "plot_hoss_line", "plot_loss_line", "text")
  expect_true(all(expected %in% names(sub)))
})

test_that("run_loss_hoss plot elements are ggplot objects", {
  d    <- make_test_data()
  spec <- make_test_spec()
  sub  <- run_loss_hoss(d, spec)
  plot_names <- c("plot_hoss_cleveland", "plot_loss_cleveland",
                  "plot_hoss_heat",      "plot_loss_heat",
                  "plot_hoss_line",      "plot_loss_line")
  for (nm in plot_names) {
    expect_true(inherits(sub[[nm]], "ggplot"),
                label = paste("element", nm, "should be ggplot"))
  }
})
