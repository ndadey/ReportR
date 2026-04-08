#' student_results_long: Expected data format for ReportR analysis functions
#'
#' All ReportR analysis functions expect a long-format data frame produced by
#' [standardize_student_results()]. This page documents the required columns
#' and their expected types.
#'
#' @format A data frame with one row per student per year per subject, with
#'   the following columns:
#'   \describe{
#'     \item{STUDENT_ID}{character. Unique student identifier. Required for
#'       correlation analysis ([run_correlations()]).}
#'     \item{YEAR}{integer. Administration year. \code{"YYYY_YYYY"}
#'       academic-year strings are automatically converted to the end year by
#'       [standardize_student_results()].}
#'     \item{GRADE}{integer. Tested grade. \code{NA} for EOC subjects.}
#'     \item{SUBJECT}{character. Subject code matching keys in
#'       \code{assessment_spec$subjects} (e.g. \code{"ELA"},
#'       \code{"MATHEMATICS"}).}
#'     \item{SCALE_SCORE}{numeric. Student scale score.}
#'     \item{ACHIEVEMENT_LEVEL}{character. Achievement level label matching
#'       \code{assessment_spec$achievement_levels$labels}.}
#'     \item{SCHOOL_ID}{character. School identifier.}
#'     \item{SCHOOL_NAME}{character. School name.}
#'     \item{DISTRICT_ID}{character. District identifier.}
#'     \item{DISTRICT_NAME}{character. District name.}
#'     \item{RACE_ETHNICITY}{character. Race/ethnicity category.}
#'     \item{FRL}{character. Free/reduced lunch status (binary demographic).}
#'     \item{EL}{character. English Learner status (binary demographic).}
#'     \item{IEP}{character. Individualized Education Program status
#'       (binary demographic).}
#'     \item{GENDER}{character. Student gender.}
#'   }
#'
#' @details
#' Use [standardize_student_results()] to convert raw assessment data to this
#' format. The function handles common variations in column naming (e.g.
#' \code{CONTENT_AREA} -> \code{SUBJECT}, \code{SS} -> \code{SCALE_SCORE}) and
#' coerces \code{YEAR} and \code{GRADE} to integer.
#'
#' **Important:** Always call [standardize_student_results()] before filtering
#' by year. Raw data from SGPdata uses \code{"YYYY_YYYY"} year strings;
#' filtering by integer year before standardizing returns 0 rows.
#'
#' @examples
#' \dontrun{
#' library(SGPdata)
#' library(dplyr)
#'
#' student_results_long <- sgpData_LONG |>
#'   filter(VALID_CASE == "VALID_CASE") |>
#'   standardize_student_results()
#' }
#'
#' @name student_results_long
NULL
