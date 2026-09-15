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
    # Nested structure: scale_scores$SUBJECT$GRADE (see CLAUDE.md section 5).
    scale_scores = list(
      ELA = list(
        `3` = list(loss = 300, hoss = 399, cuts = c(340, 370)),
        `4` = list(loss = 400, hoss = 499, cuts = c(440, 470))
      ),
      MATH = list(
        `3` = list(loss = 300, hoss = 399, cuts = c(338, 368)),
        `4` = list(loss = 400, hoss = 499, cuts = c(435, 465))
      )
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

# ---------------------------------------------------------------------------
# EOC handling — subjects declared grades = NULL whose records carry a real
# tested grade (Mississippi reports Algebra I in grades 7 through 12).
# ---------------------------------------------------------------------------

make_eoc_spec <- function() {
  spec <- make_test_spec()
  spec$subjects$ALG_I     <- list(label = "Algebra I", grades = NULL)
  spec$scale_scores$ALG_I <- list(loss = 1001, hoss = 1099, cuts = c(1038, 1064))
  spec
}

make_eoc_data <- function(n = 60, years = 2023:2024, grades = 9:10) {
  nr <- n * length(years) * length(grades)
  set.seed(7)
  data.frame(
    STUDENT_ID        = paste0("E", seq_len(nr)),
    YEAR              = rep(years, each = n * length(grades)),
    GRADE             = rep(rep(grades, each = n), times = length(years)),
    SUBJECT           = "ALG_I",
    SCALE_SCORE       = stats::rnorm(nr, mean = 1050, sd = 15),
    ACHIEVEMENT_LEVEL = "Proficient",
    SCHOOL_ID         = "SCH001",
    DISTRICT_ID       = "DIST01",
    stringsAsFactors  = FALSE
  )
}

test_that("compute_loss_hoss_table resolves bounds for EOC rows with real grades", {
  tbl <- compute_loss_hoss_table(make_eoc_data(), make_eoc_spec())
  eoc <- tbl[tbl[["SUBJECT"]] == "ALG_I", ]
  expect_gt(nrow(eoc), 0L)
  expect_false(any(is.na(eoc[["LOSS"]])))
  expect_true(all(eoc[["LOSS"]] == 1001))
  expect_true(all(eoc[["HOSS"]] == 1099))
})

test_that("compute_loss_hoss_table warns about cells the spec does not cover", {
  d <- rbind(
    make_test_data(n = 20),
    transform(make_test_data(n = 20, grades = 9L), SUBJECT = "MATH")
  )
  expect_warning(compute_loss_hoss_table(d, make_test_spec()), "MATH/9")
})

test_that("plot_loss_hoss_distributions builds a plot per EOC grade", {
  d     <- make_eoc_data()
  spec  <- make_eoc_spec()
  tbl   <- compute_loss_hoss_table(d, spec)
  plots <- plot_loss_hoss_distributions(d, spec, tbl)
  expect_true(all(c("ALG_I_Grade9", "ALG_I_Grade10") %in% names(plots)))
  expect_true(inherits(plots[["ALG_I_Grade9"]], "ggplot"))
})

test_that("plot_loss_hoss_distributions survives a cell with no spec entry", {
  # Regression for: density.default(..., from = x_lo) -> non-finite 'from'.
  # An unmapped cell must fall back to its observed range, never error.
  d    <- transform(make_test_data(n = 30), SUBJECT = "SCIENCE")
  spec <- make_test_spec()
  tbl  <- suppressWarnings(compute_loss_hoss_table(d, spec))
  expect_true(all(is.na(tbl[["LOSS"]])))
  expect_no_error(plots <- plot_loss_hoss_distributions(d, spec, tbl))
  expect_true(all(vapply(plots, function(p) is.null(p) || inherits(p, "ggplot"),
                         logical(1))))
})
