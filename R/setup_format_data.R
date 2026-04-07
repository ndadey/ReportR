# Internal helper: rename a column if the old name exists in the data frame.
rename_if_exists <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    names(df)[names(df) == old] <- new
  }
  df
}


#' Standardize student results to the canonical column names
#'
#' Renames columns from various raw source formats to the canonical names used
#' throughout the ReportR pipeline (see the data contract in the package
#' documentation). Call this once after loading raw data, before passing the
#' data frame to any analysis function.
#'
#' @param df A data frame of student-level assessment results in long format.
#'
#' @return The same data frame with canonical column names. `YEAR` and `GRADE`
#'   are coerced to integer. Columns that are already using canonical names are
#'   left unchanged.
#' @export
#' @examples
#' \dontrun{
#' library(SGPdata)
#' library(dplyr)
#' demo_data <- sgpData_LONG |>
#'   filter(VALID_CASE == "VALID_CASE") |>
#'   standardize_student_results()
#' }
standardize_student_results <- function(df) {
  stopifnot(is.data.frame(df))

  # --- Identifiers ---
  df <- rename_if_exists(df, "ID",                        "STUDENT_ID")
  df <- rename_if_exists(df, "STUDENT_ID_LONG",           "STUDENT_ID")

  # --- Assessment context ---
  df <- rename_if_exists(df, "CONTENT_AREA",              "SUBJECT")
  df <- rename_if_exists(df, "CONTENT_AREA_ORIGINAL",     "SUBJECT")

  # --- Score ---
  df <- rename_if_exists(df, "SS",                        "SCALE_SCORE")
  df <- rename_if_exists(df, "SCALE_SCORE_ORIGINAL",      "SCALE_SCORE")

  # --- Achievement level ---
  df <- rename_if_exists(df, "PERFORMANCE_LEVEL",         "ACHIEVEMENT_LEVEL")
  df <- rename_if_exists(df, "PROF_LEVEL",                "ACHIEVEMENT_LEVEL")

  # --- School / district ---
  df <- rename_if_exists(df, "SCHOOL_NUMBER",             "SCHOOL_ID")
  df <- rename_if_exists(df, "SCHOOL_CODE",               "SCHOOL_ID")
  df <- rename_if_exists(df, "DISTRICT_NUMBER",           "DISTRICT_ID")
  df <- rename_if_exists(df, "DISTRICT_CODE",             "DISTRICT_ID")
  df <- rename_if_exists(df, "SCHOOL_NAME_ORIGINAL",      "SCHOOL_NAME")
  df <- rename_if_exists(df, "DISTRICT_NAME_ORIGINAL",    "DISTRICT_NAME")

  # --- Demographics ---
  df <- rename_if_exists(df, "ETHNICITY",                 "RACE_ETHNICITY")
  df <- rename_if_exists(df, "RACE",                      "RACE_ETHNICITY")
  df <- rename_if_exists(df, "FREE_REDUCED_LUNCH_STATUS", "FRL")
  df <- rename_if_exists(df, "FRL_STATUS",                "FRL")
  df <- rename_if_exists(df, "ELL_STATUS",                "EL")
  df <- rename_if_exists(df, "EL_STATUS",                 "EL")
  df <- rename_if_exists(df, "ELL",                       "EL")
  df <- rename_if_exists(df, "IEP_STATUS",                "IEP")
  df <- rename_if_exists(df, "GENDER",                    "GENDER")

  # --- Type coercions ---
  if ("YEAR" %in% names(df)) {
    yr_raw <- as.character(df[["YEAR"]])
    # Handle "YYYY_YYYY" academic-year format (e.g. "2015_2016") by taking the
    # end year (the year in which the assessment was administered).
    df[["YEAR"]] <- suppressWarnings(
      ifelse(
        grepl("^[0-9]{4}_[0-9]{4}$", yr_raw),
        as.integer(sub("^[0-9]{4}_", "", yr_raw)),
        as.integer(yr_raw)
      )
    )
  }
  if ("GRADE" %in% names(df)) {
    df[["GRADE"]] <- suppressWarnings(as.integer(df[["GRADE"]]))
  }

  df
}
