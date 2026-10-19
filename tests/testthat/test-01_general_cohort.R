test_that("enrollment diagnostics narrate flagged cells briefly", {
  counts <- c(100, 100, 100, 100, 100, 160)
  data   <- data.frame(YEAR = rep(2019:2024, times = counts), GRADE = 3L,
                       SUBJECT = "MATH", stringsAsFactors = FALSE)
  spec   <- list(subjects = list(MATH = list(label = "Mathematics", grades = 3L)))

  out <- compute_enrollment_diagnostics(data, spec)
  expect_equal(sum(out$table$FLAGGED), 1L)
  expect_match(out$narrative, "Mathematics grade 3, 2024 (N = 160, z = +2.04)",
               fixed = TRUE)
})
