# Internal helper: names of the EOC subjects in a spec — those declared with
# `grades = NULL`. EOC records are matched on SUBJECT alone, because states
# routinely report a real tested grade on EOC rows (e.g. Algebra I taken in
# grades 7-12), which would never match a lookup keyed on GRADE = NA.
.eoc_subjects <- function(assessment_spec) {
  subs <- assessment_spec$subjects
  if (is.null(subs)) return(character(0L))
  names(subs)[vapply(subs, function(s) is.null(s$grades), logical(1))]
}


#' Retrieve scale score specification for a subject/grade combination
#'
#' Looks up the `loss`, `hoss`, and `cuts` entry from `assessment_spec$scale_scores`.
#' Span-tested subjects are keyed by grade (`scale_scores$SUBJECT$GRADE`); EOC
#' subjects have no grade nesting (`scale_scores$SUBJECT`) and are resolved on
#' `subject` alone, so a `grade` supplied for an EOC subject is ignored rather
#' than causing a lookup miss.
#'
#' @param assessment_spec Named list describing the assessment program.
#' @param subject Character scalar. Subject code matching a key in
#'   `assessment_spec$subjects`.
#' @param grade Integer scalar or `NULL`. Grade level for span-tested subjects;
#'   ignored for EOC subjects.
#'
#' @return A named list with elements `loss`, `hoss`, and `cuts`, or `NULL` if
#'   the combination is not found in the spec.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
#' get_score_spec(assessment_spec, "ELA", 3)
#' get_score_spec(assessment_spec, "ENG_II")
#' get_score_spec(assessment_spec, "ALG_I", 9)  # EOC — grade ignored
#' }
get_score_spec <- function(assessment_spec, subject, grade = NULL) {
  stopifnot(
    is.list(assessment_spec),
    !is.null(assessment_spec$scale_scores),
    is.character(subject),
    length(subject) == 1L
  )
  ss <- assessment_spec$scale_scores[[subject]]
  if (is.null(ss)) return(NULL)
  if (is.null(grade)) return(ss)
  # EOC subject — the spec entry is not grade-nested, so ignore `grade`.
  if (subject %in% .eoc_subjects(assessment_spec)) return(ss)
  ss[[as.character(grade)]]
}


#' Build a LOSS/HOSS lookup table from an assessment spec
#'
#' Iterates over every subject (and grade, for span-tested subjects) defined in
#' `assessment_spec$subjects` and returns a data frame of LOSS and HOSS values
#' suitable for joining to student-level data.
#'
#' @param assessment_spec Named list describing the assessment program.
#' @param cuts Logical. When `TRUE`, append a `CUTS` list-column holding each
#'   row's cut-score vector. Default `FALSE`.
#'
#' @return A data frame with columns `SUBJECT` (character), `GRADE` (integer,
#'   `NA` for EOC subjects), `LOSS` (numeric), `HOSS` (numeric), and — when
#'   `cuts = TRUE` — `CUTS` (list). One row per subject/grade combination that
#'   has a matching entry in `assessment_spec$scale_scores`.
#'
#'   Note that EOC rows carry `GRADE = NA`. Do not join this table to student
#'   data on `SUBJECT` + `GRADE` directly — states commonly report a real tested
#'   grade on EOC records, which would miss. Use [join_score_bounds()] instead.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
#' build_loss_hoss_lookup(assessment_spec)
#' }
build_loss_hoss_lookup <- function(assessment_spec, cuts = FALSE) {
  stopifnot(
    is.list(assessment_spec),
    !is.null(assessment_spec$subjects),
    !is.null(assessment_spec$scale_scores),
    is.logical(cuts), length(cuts) == 1L
  )

  # Build one lookup row from a single scale_scores entry.
  make_row <- function(subj, grade, ss) {
    if (is.null(ss)) return(NULL)
    out <- data.frame(
      SUBJECT = subj,
      GRADE   = grade,
      LOSS    = ss$loss,
      HOSS    = ss$hoss,
      stringsAsFactors = FALSE
    )
    if (cuts) out[["CUTS"]] <- I(list(ss$cuts))
    out
  }

  rows <- lapply(names(assessment_spec$subjects), function(subj) {
    grades <- assessment_spec$subjects[[subj]]$grades

    if (is.null(grades)) {
      # EOC subject — no grade nesting
      make_row(subj, NA_integer_, assessment_spec$scale_scores[[subj]])
    } else {
      # Span-tested subject — nested by grade character key
      do.call(rbind, lapply(grades, function(g) {
        make_row(subj, as.integer(g),
                 assessment_spec$scale_scores[[subj]][[as.character(g)]])
      }))
    }
  })

  do.call(rbind, rows)
}


#' Attach LOSS/HOSS (and optionally cut scores) to a data frame
#'
#' The single supported way to bring scale-score bounds from an
#' `assessment_spec` onto data. Span-tested subjects are matched on `SUBJECT` +
#' `GRADE`; EOC subjects (those declared `grades = NULL`) are matched on
#' `SUBJECT` alone.
#'
#' The EOC rule matters: states routinely report a real tested grade on EOC
#' records — Mississippi's Algebra I records span grades 7 through 12 — while
#' the spec declares EOC subjects without grades. Joining on `SUBJECT` + `GRADE`
#' silently misses every one of those rows and leaves `LOSS`/`HOSS` as `NA`.
#'
#' @param data A data frame with `SUBJECT` and `GRADE` columns. Works on
#'   student-level data and on already-summarized tables alike.
#' @param assessment_spec Named list describing the assessment program.
#' @param cuts Logical. When `TRUE`, also attach a `CUTS` list-column.
#'   Default `FALSE`.
#' @param warn_unmatched Logical. When `TRUE` (default), raise a single warning
#'   naming each `SUBJECT`/`GRADE` cell present in `data` that has no entry in
#'   the spec, with its row count. Set `FALSE` in downstream callers so one run
#'   warns once.
#'
#' @return `data` with `GRADE` coerced to integer and `LOSS`, `HOSS` (and
#'   `CUTS`, when requested) attached. Cells with no spec entry get `NA`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
#' dat <- join_score_bounds(student_results_long, assessment_spec)
#' }
join_score_bounds <- function(data, assessment_spec, cuts = FALSE,
                              warn_unmatched = TRUE) {
  stopifnot(
    is.data.frame(data),
    is.list(assessment_spec),
    !is.null(assessment_spec$subjects),
    !is.null(assessment_spec$scale_scores),
    all(c("SUBJECT", "GRADE") %in% names(data))
  )

  lookup   <- build_loss_hoss_lookup(assessment_spec, cuts = cuts)
  eoc      <- .eoc_subjects(assessment_spec)
  add_cols <- c("LOSS", "HOSS", if (cuts) "CUTS")

  out <- data
  out[["GRADE"]] <- suppressWarnings(as.integer(out[["GRADE"]]))

  # build_loss_hoss_lookup() returns NULL when no subject/grade in the spec has
  # a scale_scores entry. Attach all-NA bounds so callers still find the columns.
  if (is.null(lookup) || nrow(lookup) == 0L) {
    for (nm in add_cols) {
      out[[nm]] <- if (nm == "CUTS") vector("list", nrow(out)) else NA_real_
    }
    if (warn_unmatched) .warn_unmatched_cells(out)
    return(out)
  }

  lk_grd <- lookup[!lookup[["SUBJECT"]] %in% eoc, , drop = FALSE]
  lk_eoc <- lookup[ lookup[["SUBJECT"]] %in% eoc, , drop = FALSE]
  lk_eoc[["GRADE"]] <- NULL   # EOC matches on SUBJECT alone

  # Stage 1 — span-tested subjects on SUBJECT + GRADE.
  if (nrow(lk_grd) > 0L) {
    out <- dplyr::left_join(out, lk_grd, by = c("SUBJECT", "GRADE"))
  } else {
    for (nm in add_cols) {
      out[[nm]] <- if (nm == "CUTS") vector("list", nrow(out)) else NA_real_
    }
  }

  # Stage 2 — EOC subjects on SUBJECT alone, coalesced over stage 1.
  if (nrow(lk_eoc) > 0L) {
    out <- dplyr::left_join(out, lk_eoc, by = "SUBJECT", suffix = c("", ".eoc"))
    out[["LOSS"]] <- dplyr::coalesce(out[["LOSS"]], out[["LOSS.eoc"]])
    out[["HOSS"]] <- dplyr::coalesce(out[["HOSS"]], out[["HOSS.eoc"]])
    if (cuts) {
      # CUTS is a list-column, so coalesce() does not apply — splice by hand.
      need <- lengths(out[["CUTS"]]) == 0L
      out[["CUTS"]][need] <- out[["CUTS.eoc"]][need]
    }
    out <- out[, !names(out) %in% paste0(add_cols, ".eoc"), drop = FALSE]
  }

  if (warn_unmatched) .warn_unmatched_cells(out)

  out
}


# Internal helper: warn once, naming every SUBJECT/GRADE cell that found no
# scale-score entry. Surfaces genuine data anomalies (a subject code the spec
# does not know, a grade outside the tested span) that would otherwise vanish
# silently into NA bounds.
.warn_unmatched_cells <- function(dat) {
  miss <- dat[is.na(dat[["LOSS"]]), c("SUBJECT", "GRADE"), drop = FALSE]
  if (nrow(miss) == 0L) return(invisible(NULL))

  tab <- as.data.frame(
    table(SUBJECT = miss[["SUBJECT"]], GRADE = miss[["GRADE"]], useNA = "ifany"),
    stringsAsFactors = FALSE
  )
  tab <- tab[tab[["Freq"]] > 0L, , drop = FALSE]

  warning(
    "No scale-score spec entry for ", nrow(tab), " SUBJECT x GRADE cell(s); ",
    "LOSS/HOSS set to NA: ",
    paste0(tab[["SUBJECT"]], "/", tab[["GRADE"]],
           " (n=", tab[["Freq"]], ")", collapse = ", "),
    call. = FALSE
  )
  invisible(NULL)
}


#' Return the index of the policy benchmark level in the achievement level labels
#'
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return An integer scalar: the position of `policy_benchmark` in
#'   `achievement_levels$labels`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
#' benchmark_index(assessment_spec)  # 4 for MS (Proficient is 4th of 5 levels)
#' }
benchmark_index <- function(assessment_spec) {
  stopifnot(
    is.list(assessment_spec),
    !is.null(assessment_spec$achievement_levels$labels),
    !is.null(assessment_spec$achievement_levels$policy_benchmark)
  )
  which(
    assessment_spec$achievement_levels$labels ==
      assessment_spec$achievement_levels$policy_benchmark
  )
}
