# ==============================================================================
# Narrative helpers
#
# Narratives lead with the finding, name at most a few items (largest first),
# and leave full detail to the tables printed next to them.
# ==============================================================================

# "a", "a and b", "a, b, and c" (sep = "; " for items that contain commas)
.oxford <- function(x, sep = ", ") {
  x <- as.character(x)
  n <- length(x)
  if (n <= 1L) return(paste(x, collapse = ""))
  if (n == 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-n], collapse = sep), sep, "and ", x[n])
}

.grades_phrase <- function(grades) {
  paste0(if (length(grades) == 1L) "grade " else "grades ", .oxford(grades))
}

# "grade 4", "grades 3 and 5", "5 of 8 grades", "all 8 grades"
.grade_count_phrase <- function(grades, n_total) {
  k <- length(grades)
  if (k == n_total && n_total > 2L) return(paste0("all ", n_total, " grades"))
  if (k <= 3L) return(.grades_phrase(grades))
  paste0(k, " of ", n_total, " grades")
}

# "+11.1" / "−8.6" (true minus sign)
.fmt_signed <- function(x, digits = 1) {
  paste0(if (x < 0) "\u2212" else "+",
         formatC(abs(x), format = "f", digits = digits))
}

# Name every item when there are few; otherwise only the first `n_show` (callers
# sort largest first and state the total count themselves).
.top_items_sentence <- function(items, n_show = 3L) {
  if (length(items) <= n_show) return(paste0(.oxford(items, sep = "; "), "."))
  paste0("The largest: ", .oxford(items[seq_len(n_show)], sep = "; "), ".")
}

# Subject and grade as prose ("Mathematics grade 8"); the subject alone for EOC.
.place_label <- function(subject_label, grade) {
  ifelse(is.na(grade), subject_label, paste0(subject_label, " grade ", grade))
}

# One-line summary of first-to-last-year changes across a subject's grades, e.g.
# "up in 5 of 8 grades, most in grade 4 (+11.1 points); down in grade 5 (...)".
# Pass grade = NA for an end-of-course subject (a single change).
.change_summary <- function(grades, deltas, flat_threshold, unit) {
  if (length(deltas) == 1L && is.na(grades[1L])) {
    d <- deltas[1L]
    if (abs(d) < flat_threshold) {
      return(paste0("relatively flat (", .fmt_signed(d), " ", unit, ")"))
    }
    return(paste0(if (d > 0) "up " else "down ",
                  formatC(abs(d), format = "f", digits = 1), " ", unit))
  }

  n    <- length(deltas)
  up   <- deltas >=  flat_threshold
  down <- deltas <= -flat_threshold
  flat <- !up & !down

  describe <- function(mask, word, pick) {
    g <- grades[mask]
    d <- deltas[mask]
    i <- pick(d)
    paste0(word, " in ", .grade_count_phrase(g, n),
           if (length(g) > 1L) paste0(", most in grade ", g[i]) else "",
           " (", .fmt_signed(d[i]), " ", unit, ")")
  }

  parts <- character(0)
  if (any(up))   parts <- c(parts, describe(up,   "up",   which.max))
  if (any(down)) parts <- c(parts, describe(down, "down", which.min))
  if (any(flat)) {
    parts <- c(parts, paste0("relatively flat in ",
                             .grade_count_phrase(grades[flat], n)))
  }
  paste(parts, collapse = "; ")
}
