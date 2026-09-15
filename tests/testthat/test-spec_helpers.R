library(testthat)

# ---------------------------------------------------------------------------
# Fixtures — one span-tested subject (grades 3-4) and one EOC subject.
# EOC records deliberately carry real tested grades, mirroring Mississippi,
# where Algebra I is taken in grades 7 through 12.
# ---------------------------------------------------------------------------
make_spec_helpers_spec <- function() {
  list(
    subjects = list(
      ELA   = list(label = "English Language Arts", grades = 3:4),
      ALG_I = list(label = "Algebra I",             grades = NULL)
    ),
    achievement_levels = list(
      labels           = c("Basic", "Proficient", "Advanced"),
      policy_benchmark = "Proficient"
    ),
    scale_scores = list(
      ELA = list(
        `3` = list(loss = 300, hoss = 399, cuts = c(340, 370)),
        `4` = list(loss = 400, hoss = 499, cuts = c(440, 470))
      ),
      ALG_I = list(loss = 1001, hoss = 1099, cuts = c(1038, 1064))
    ),
    years = list(tested_years = 2023:2024, cohort_anchor_grade = 3),
    demographics = list()
  )
}

make_spec_helpers_data <- function() {
  data.frame(
    SUBJECT     = c("ELA", "ELA", "ALG_I", "ALG_I", "ALG_I"),
    GRADE       = c(3L,    4L,    9L,      10L,     7L),
    SCALE_SCORE = c(350,   450,   1050,    1060,    1020),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# get_score_spec()
# ---------------------------------------------------------------------------

test_that("get_score_spec resolves span-tested subjects by grade", {
  spec <- make_spec_helpers_spec()
  expect_equal(get_score_spec(spec, "ELA", 3)$loss, 300)
  expect_equal(get_score_spec(spec, "ELA", 4)$hoss, 499)
})

test_that("get_score_spec ignores grade for EOC subjects", {
  spec <- make_spec_helpers_spec()
  # Regression: previously returned NULL because it looked for a `9` key.
  expect_equal(get_score_spec(spec, "ALG_I", 9)$loss,  1001)
  expect_equal(get_score_spec(spec, "ALG_I", 12)$hoss, 1099)
  expect_equal(get_score_spec(spec, "ALG_I")$loss,     1001)
})

test_that("get_score_spec returns NULL for unknown subjects and grades", {
  spec <- make_spec_helpers_spec()
  expect_null(get_score_spec(spec, "SCIENCE", 5))
  expect_null(get_score_spec(spec, "ELA", 9))
})

# ---------------------------------------------------------------------------
# build_loss_hoss_lookup()
# ---------------------------------------------------------------------------

test_that("build_loss_hoss_lookup returns one row per spec cell", {
  lk <- build_loss_hoss_lookup(make_spec_helpers_spec())
  expect_s3_class(lk, "data.frame")
  expect_equal(nrow(lk), 3L)                       # ELA 3, ELA 4, ALG_I
  expect_true(all(c("SUBJECT", "GRADE", "LOSS", "HOSS") %in% names(lk)))
  expect_true(is.na(lk[["GRADE"]][lk[["SUBJECT"]] == "ALG_I"]))
})

test_that("build_loss_hoss_lookup adds a CUTS list-column on request", {
  lk <- build_loss_hoss_lookup(make_spec_helpers_spec(), cuts = TRUE)
  expect_true("CUTS" %in% names(lk))
  expect_true(is.list(lk[["CUTS"]]))
  expect_equal(lk[["CUTS"]][lk[["SUBJECT"]] == "ALG_I"][[1L]], c(1038, 1064))
})

# ---------------------------------------------------------------------------
# join_score_bounds()
# ---------------------------------------------------------------------------

test_that("join_score_bounds attaches bounds to EOC rows carrying real grades", {
  # The bug this whole change exists for: EOC records with grades 7-12 missed
  # a SUBJECT + GRADE join and silently received NA bounds.
  out <- join_score_bounds(make_spec_helpers_data(), make_spec_helpers_spec())
  eoc <- out[out[["SUBJECT"]] == "ALG_I", ]
  expect_equal(nrow(eoc), 3L)
  expect_false(any(is.na(eoc[["LOSS"]])))
  expect_true(all(eoc[["LOSS"]] == 1001))
  expect_true(all(eoc[["HOSS"]] == 1099))
})

test_that("join_score_bounds still matches span-tested subjects on SUBJECT + GRADE", {
  out <- join_score_bounds(make_spec_helpers_data(), make_spec_helpers_spec())
  expect_equal(out[["LOSS"]][out[["SUBJECT"]] == "ELA" & out[["GRADE"]] == 3L], 300)
  expect_equal(out[["HOSS"]][out[["SUBJECT"]] == "ELA" & out[["GRADE"]] == 4L], 499)
})

test_that("join_score_bounds preserves row count and does not reorder", {
  d   <- make_spec_helpers_data()
  out <- join_score_bounds(d, make_spec_helpers_spec())
  expect_equal(nrow(out), nrow(d))
  expect_equal(out[["SUBJECT"]], d[["SUBJECT"]])
  expect_equal(out[["SCALE_SCORE"]], d[["SCALE_SCORE"]])
})

test_that("join_score_bounds attaches cuts for both subject types", {
  out <- join_score_bounds(make_spec_helpers_data(), make_spec_helpers_spec(),
                           cuts = TRUE)
  expect_true(is.list(out[["CUTS"]]))
  expect_equal(out[["CUTS"]][out[["SUBJECT"]] == "ELA" &
                             out[["GRADE"]] == 3L][[1L]], c(340, 370))
  # EOC cuts spliced in via the SUBJECT-only stage
  expect_equal(out[["CUTS"]][out[["SUBJECT"]] == "ALG_I" &
                             out[["GRADE"]] == 9L][[1L]], c(1038, 1064))
})

test_that("join_score_bounds warns naming each unmatched cell", {
  d <- rbind(make_spec_helpers_data(),
             data.frame(SUBJECT = "MATH", GRADE = 9L, SCALE_SCORE = 700,
                        stringsAsFactors = FALSE))
  expect_warning(out <- join_score_bounds(d, make_spec_helpers_spec()),
                 "MATH/9")
  expect_true(is.na(out[["LOSS"]][out[["SUBJECT"]] == "MATH"]))
})

test_that("join_score_bounds stays silent when warn_unmatched is FALSE", {
  d <- rbind(make_spec_helpers_data(),
             data.frame(SUBJECT = "MATH", GRADE = 9L, SCALE_SCORE = 700,
                        stringsAsFactors = FALSE))
  expect_no_warning(join_score_bounds(d, make_spec_helpers_spec(),
                                      warn_unmatched = FALSE))
})

test_that("join_score_bounds does not warn when every cell matches", {
  expect_no_warning(
    join_score_bounds(make_spec_helpers_data(), make_spec_helpers_spec())
  )
})

test_that("join_score_bounds handles a spec with no EOC subjects", {
  spec <- make_spec_helpers_spec()
  spec$subjects$ALG_I     <- NULL
  spec$scale_scores$ALG_I <- NULL
  d    <- make_spec_helpers_data()[1:2, ]
  out  <- join_score_bounds(d, spec)
  expect_true(all(c("LOSS", "HOSS") %in% names(out)))
  expect_false(any(is.na(out[["LOSS"]])))
})

test_that("join_score_bounds handles a spec with only EOC subjects", {
  spec <- make_spec_helpers_spec()
  spec$subjects$ELA     <- NULL
  spec$scale_scores$ELA <- NULL
  d    <- make_spec_helpers_data()[3:5, ]
  out  <- join_score_bounds(d, spec)
  expect_false(any(is.na(out[["LOSS"]])))
})

test_that("join_score_bounds returns NA bound columns when the spec matches nothing", {
  spec <- make_spec_helpers_spec()
  spec$scale_scores <- list()   # no entries at all -> lookup is NULL
  out <- suppressWarnings(
    join_score_bounds(make_spec_helpers_data(), spec, cuts = TRUE)
  )
  expect_true(all(c("LOSS", "HOSS", "CUTS") %in% names(out)))
  expect_true(all(is.na(out[["LOSS"]])))
  expect_equal(nrow(out), 5L)
})
