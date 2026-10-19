make_transitions_spec <- function() {
  list(subjects = list(MATH = list(label = "Mathematics", grades = 3:4)))
}

make_transitions_data <- function() {
  summary_table <- data.frame(
    SUBJECT = "MATH",
    GRADE   = rep(3:4, each = 4),
    YEAR_1  = rep(2020:2023, 2),
    YEAR_2  = rep(2021:2024, 2),
    n_total = 1000L,
    # grade 3 jumps up in 2022-2023; grade 4 moves up less every year
    up      = c(0.15, 0.16, 0.30, 0.14, 0.05, 0.06, 0.05, 0.04),
    down    = c(0.15, 0.14, 0.05, 0.16, 0.35, 0.34, 0.36, 0.35),
    stringsAsFactors = FALSE
  )
  summary_table$stable <- 1 - summary_table$up - summary_table$down

  long_table <- data.frame(
    SUBJECT    = "MATH",
    GRADE      = rep(3:4, each = 4),
    YEAR_1     = rep(2020:2023, 2),
    YEAR_2     = rep(2021:2024, 2),
    from_level = "Basic",
    to_level   = "Basic",
    # grade 3 rises in 2023-2024; grade 4 is higher every year
    prop       = c(0.10, 0.11, 0.10, 0.20, 0.30, 0.31, 0.30, 0.29),
    stringsAsFactors = FALSE
  )
  list(summary_table = summary_table, long_table = long_table)
}

test_that("flags compare each year pair with the same grade's other years", {
  fl <- flag_transitions(make_transitions_data(), make_transitions_spec())
  expect_equal(nrow(fl$table), 1L)
  expect_equal(fl$table$GRADE, 3L)
  expect_equal(fl$table$YEAR_1, 2022L)
  expect_equal(fl$table$flag_type, "unusually high upward rate")
})

test_that("a grade that differs the same way every year is not flagged", {
  fl <- flag_transitions(make_transitions_data(), make_transitions_spec())
  expect_false(4L %in% fl$table$GRADE)
  expect_false(4L %in% fl$table_cells$GRADE)
})

test_that("cell flags report the other years' share as typical", {
  fl <- flag_transitions(make_transitions_data(), make_transitions_spec())
  expect_equal(nrow(fl$table_cells), 1L)
  expect_equal(fl$table_cells$YEAR_1, 2023L)
  expect_equal(fl$table_cells$mean_prop, mean(c(0.10, 0.11, 0.10)))
})

test_that("the flag narrative is short and names the flagged year pair", {
  fl <- flag_transitions(make_transitions_data(), make_transitions_spec())
  expect_match(fl$narrative, "Mathematics grade 3, 2022\u20132023: 30% moved up")
  expect_lt(nchar(fl$narrative), 600)
})

test_that("the transition summary gives one line per subject", {
  data <- make_transitions_data()
  spec <- make_transitions_spec()
  fl   <- flag_transitions(data, spec)
  txt  <- summarize_transition_findings(data, spec, flags = fl)$narrative
  expect_length(gregexpr("\n- ", txt, fixed = TRUE)[[1]], 1L)
  expect_match(txt, "on average")
  expect_match(txt, "Most upward: grade 3, 2022\u20132023 (30% moved up)",
               fixed = TRUE)
  expect_match(txt, "1 grade-year combination was flagged as unusual.",
               fixed = TRUE)
})
