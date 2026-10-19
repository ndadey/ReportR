make_distributions_data <- function(n = 4000) {
  set.seed(11)
  cell <- function(grade, scores) {
    data.frame(YEAR = 2024L, SUBJECT = "MATH", GRADE = grade,
               SCALE_SCORE = scores, stringsAsFactors = FALSE)
  }
  rbind(
    cell(3L, rnorm(n, 500, 40)),
    # trapezoid: flat top, excess kurtosis about -1.1
    cell(4L, runif(n, 300, 700) + runif(n, 0, 80)),
    cell(5L, 400 + stats::rgamma(n, shape = 3, scale = 30)),
    cell(6L, c(rnorm(n / 2, 420, 15), rnorm(n / 2, 580, 15))),
    cell(7L, rnorm(5, 500, 40))
  )
}

make_distributions_spec <- function() {
  list(subjects = list(MATH = list(label = "Mathematics", grades = 3:7)))
}

test_that("each shape gets the expected flag", {
  out  <- compute_distributions(make_distributions_data(),
                                make_distributions_spec())
  flag <- stats::setNames(out$distribution_flag, out$GRADE)
  expect_equal(unname(flag[c("3", "4", "5", "6")]),
               c("None", "Flat", "Skewed", "Bimodal"))
})

test_that("a normal distribution is not flagged as flat", {
  out <- compute_distributions(make_distributions_data(),
                               make_distributions_spec())
  normal <- out[out$GRADE == 3L, ]
  expect_gt(normal$kurtosis, -1)
  expect_equal(normal$distribution_flag, "None")
})

test_that("the flat flag is based on excess kurtosis", {
  out  <- compute_distributions(make_distributions_data(),
                                make_distributions_spec())
  flat <- out[out$GRADE == 4L, ]
  expect_lt(flat$kurtosis, -1)
  expect_match(flat$flag_reason, "kurtosis")
})

test_that("cells below min_n are suppressed and unflagged", {
  out   <- compute_distributions(make_distributions_data(),
                                 make_distributions_spec(), min_n = 10)
  small <- out[out$GRADE == 7L, ]
  expect_true(is.na(small$sd))
  expect_true(is.na(small$kurtosis))
  expect_equal(small$distribution_flag, "None")
})

test_that("flags do not require score ranges in the spec", {
  expect_no_error(
    compute_distributions(make_distributions_data(), make_distributions_spec())
  )
})
