#' Retrieve scale score specification for a subject/grade combination
#'
#' Looks up the `loss`, `hoss`, and `cuts` entry from `assessment_spec$scale_scores`
#' using the correct key format: `"SUBJECT_GRADE"` for span-tested subjects and
#' `"SUBJECT"` for EOC subjects (where `grade` is `NULL`).
#'
#' @param assessment_spec Named list describing the assessment program.
#' @param subject Character scalar. Subject code matching a key in
#'   `assessment_spec$subjects`.
#' @param grade Integer scalar or `NULL`. Grade level for span-tested subjects;
#'   `NULL` for EOC subjects.
#'
#' @return A named list with elements `loss`, `hoss`, and `cuts`, or `NULL` if
#'   the combination is not found in the spec.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
#' get_score_spec(assessment_spec, "ELA", 3)
#' get_score_spec(assessment_spec, "ENG_II")
#' }
get_score_spec <- function(assessment_spec, subject, grade = NULL) {
  stopifnot(
    is.list(assessment_spec),
    !is.null(assessment_spec$scale_scores),
    is.character(subject),
    length(subject) == 1L
  )
  key <- if (is.null(grade)) subject else paste(subject, grade, sep = "_")
  assessment_spec$scale_scores[[key]]
}


#' Build a LOSS/HOSS lookup table from an assessment spec
#'
#' Iterates over every subject (and grade, for span-tested subjects) defined in
#' `assessment_spec$subjects` and returns a data frame of LOSS and HOSS values
#' suitable for joining to student-level data.
#'
#' @param assessment_spec Named list describing the assessment program.
#'
#' @return A data frame with columns `SUBJECT` (character), `GRADE` (integer,
#'   `NA` for EOC subjects), `LOSS` (numeric), and `HOSS` (numeric). One row
#'   per subject/grade combination that has a matching entry in
#'   `assessment_spec$scale_scores`.
#' @export
#' @examples
#' \dontrun{
#' source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
#' build_loss_hoss_lookup(assessment_spec)
#' }
build_loss_hoss_lookup <- function(assessment_spec) {
  stopifnot(
    is.list(assessment_spec),
    !is.null(assessment_spec$subjects),
    !is.null(assessment_spec$scale_scores)
  )

  rows <- lapply(names(assessment_spec$subjects), function(subj) {
    grades <- assessment_spec$subjects[[subj]]$grades

    if (is.null(grades)) {
      # EOC subject — key is the subject name alone
      ss <- assessment_spec$scale_scores[[subj]]
      if (is.null(ss)) return(NULL)
      data.frame(
        SUBJECT = subj,
        GRADE   = NA_integer_,
        LOSS    = ss$loss,
        HOSS    = ss$hoss,
        stringsAsFactors = FALSE
      )
    } else {
      # Span-tested subject — one row per grade
      grade_rows <- lapply(grades, function(g) {
        key <- paste(subj, g, sep = "_")
        ss  <- assessment_spec$scale_scores[[key]]
        if (is.null(ss)) return(NULL)
        data.frame(
          SUBJECT = subj,
          GRADE   = as.integer(g),
          LOSS    = ss$loss,
          HOSS    = ss$hoss,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, grade_rows)
    }
  })

  do.call(rbind, rows)
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
