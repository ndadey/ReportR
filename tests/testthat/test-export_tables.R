test_that("sheet names abbreviate known slot names", {
  expect_equal(.shorten_sheet_name("achievement_transitions.matrices.summary_table"),
               "ach_trans.matrices.summ_tbl")
  expect_equal(.shorten_sheet_name("performance_trends.improvement_classification"),
               "perf_trends.impr_class")
  expect_equal(.shorten_sheet_name("score_trends.table"), "score_trends.table")
})

test_that("long sheet names keep every level of the path", {
  nm <- .shorten_sheet_name("transitions.FREE_REDUCED_LUNCH_STATUS_FLAG.summary")
  expect_lte(nchar(nm), 31L)
  expect_match(nm, "^trans\\.FREE_[A-Z_]+\\.summary$")
})

test_that("characters Excel does not allow are replaced", {
  expect_equal(.shorten_sheet_name("by_group.Grade 3/4"), "by_group.Grade_3_4")
})

test_that("duplicate sheet names are made unique ignoring case", {
  expect_equal(.dedupe_sheet_names(c("a.table", "A.TABLE", "a.table")),
               c("a.table", "A.TABLE_2", "a.table_3"))
  long <- .dedupe_sheet_names(rep(strrep("x", 31), 2))
  expect_equal(nchar(long), c(31L, 31L))
  expect_false(identical(long[1], long[2]))
})

test_that("export_tables_to_excel writes non-empty tables and skips plots", {
  skip_if_not_installed("openxlsx")
  results <- list(
    score_trends = list(
      table = data.frame(a = 1:2),
      plot  = list(mean_trend = ggplot2::ggplot()),
      text  = list(narrative = "text", table = data.frame(b = 3))
    ),
    empty = data.frame(c = integer(0)),
    achievement_transitions = list(
      matrices = list(summary_table = data.frame(d = 1))
    )
  )
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f))

  expect_message(export_tables_to_excel(results, f), "Wrote 3 tables")
  expect_equal(openxlsx::getSheetNames(f),
               c("score_trends.table", "score_trends.text.table",
                 "ach_trans.matrices.summ_tbl"))
})
