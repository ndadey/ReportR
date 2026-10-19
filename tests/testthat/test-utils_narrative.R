test_that(".oxford joins items with commas and 'and'", {
  expect_equal(.oxford("a"), "a")
  expect_equal(.oxford(c("a", "b")), "a and b")
  expect_equal(.oxford(c("a", "b", "c")), "a, b, and c")
  expect_equal(.oxford(c("a", "b", "c"), sep = "; "), "a; b; and c")
})

test_that("grade phrases list a few grades and count many", {
  expect_equal(.grades_phrase(4), "grade 4")
  expect_equal(.grades_phrase(c(3, 5)), "grades 3 and 5")
  expect_equal(.grade_count_phrase(c(3, 5), 8), "grades 3 and 5")
  expect_equal(.grade_count_phrase(c(4, 6, 7, 8, 10), 8), "5 of 8 grades")
  expect_equal(.grade_count_phrase(3:10, 8), "all 8 grades")
})

test_that(".fmt_signed uses a plus or a true minus sign", {
  expect_equal(.fmt_signed(11.06), "+11.1")
  expect_equal(.fmt_signed(-8.6), "\u22128.6")
  expect_equal(.fmt_signed(-1.634, digits = 2), "\u22121.63")
})

test_that(".change_summary describes all grades in one line", {
  out <- .change_summary(3:10, c(-2.6, 11.1, -8.6, 1.7, 9.9, 8.3, 0.9, 4.7),
                         flat_threshold = 1, unit = "points")
  expect_equal(out, paste0(
    "up in 5 of 8 grades, most in grade 4 (+11.1 points); ",
    "down in grades 3 and 5, most in grade 5 (\u22128.6 points); ",
    "relatively flat in grade 9"
  ))
})

test_that(".change_summary handles a single end-of-course change", {
  expect_equal(.change_summary(NA, 3.24, 1, "points"), "up 3.2 points")
  expect_equal(.change_summary(NA, -0.4, 1, "points"),
               "relatively flat (\u22120.4 points)")
})

test_that(".top_items_sentence names at most three items", {
  expect_equal(.top_items_sentence(c("A", "B")), "A and B.")
  expect_equal(.top_items_sentence(c("A", "B", "C", "D")),
               "The largest: A; B; and C.")
})
