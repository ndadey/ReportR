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

make_flagged_distribution_table <- function() {
  data.frame(
    YEAR    = rep(2020:2024, times = 2),
    SUBJECT = rep(c("READ", "MATH"), each = 5),
    GRADE   = 4L,
    n       = 500L,
    sd      = 40,
    skewness = c(-1.4, -1.3, -1.2, -1.5, 0.1, 0.0, 0.1, -1.2, 0.2, 0.1),
    kurtosis = 1,
    bimodality_coefficient = 0.3,
    distribution_flag = c(rep("Skewed", 4), "None", "None", "None", "Skewed",
                          "None", "None"),
    stringsAsFactors = FALSE
  )
}

test_that("the distribution narrative counts flags and names lasting patterns", {
  spec <- list(subjects = list(
    READ = list(label = "Reading",     grades = 4L),
    MATH = list(label = "Mathematics", grades = 4L)
  ))
  txt <- summarize_distributions(make_flagged_distribution_table(), spec)$narrative
  expect_match(txt, paste0("5 of 10 year-subject-grade combinations had unusual ",
                           "score distributions, all skewed."), fixed = TRUE)
  expect_match(txt, "By subject: Mathematics 1 and Reading 4.", fixed = TRUE)
  expect_match(txt, "Flagged in at least 4 of 5 years: Reading grade 4.",
               fixed = TRUE)
})

test_that("the distribution narrative falls back to subject codes", {
  txt <- summarize_distributions(make_flagged_distribution_table())$narrative
  expect_match(txt, "By subject: MATH 1 and READ 4.", fixed = TRUE)
})
