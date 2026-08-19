# =============================================================================
# Workshop: Running the Enrollment Analysis with ReportR — MS MAAP
#
# This script walks through the three steps to produce an Enrollment
# Analysis report using the ReportR package and real Mississippi MAAP data.
#
# Requirements:
#   devtools::load_all() or devtools::install_github("ndadey/ReportR")
#   install.packages("quarto")
#   install.packages("openxlsx")  # optional, for Excel export
#
# Run this script from the package root:
#   C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR
# =============================================================================


# =============================================================================
# STEP 1: Setup — Long Format Data & Assessment Spec
# =============================================================================

library(dplyr)
library(quarto)

# Load package functions from source (development mode).
# Switch to library(ReportR) once the package is installed.
PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"
devtools::load_all(PKG_ROOT)

# -- 1a. Load and standardize MS student data ---------------------------------
#
# _setup_light.R loads the MS MAAP data into `student_results_long` and
# sources the MS assessment spec (puts `assessment_spec` in scope).
# It also runs standardize_student_results() so column names match the
# package's data contract.

setwd(PKG_ROOT)
source("MS/_setup_light.R")

# -- 1b. Confirm the MS spec is loaded ----------------------------------------

SPEC_FILE <- file.path(PKG_ROOT, "inst/specs/00_MS_Assessment_Spec.R")
source(SPEC_FILE)   # puts assessment_spec in scope (or refreshes it)


# =============================================================================
# STEP 2: Run the Analysis
# =============================================================================

# run_general_enrollment_analysis() runs all enrollment sub-analyses:
#
#   $counts       — unique student counts by year, grade, subject, subgroup,
#                   school, and district
#   $proportions  — subgroup proportions within year, grade, and school
#   $transitions  — year-over-year composition shifts per subgroup variable
#   $entry_exit   — mean score and % proficient for entering vs. exiting students
#   $aggregate    — school- and district-level enrollment totals and subgroup splits
devtools::load_all(PKG_ROOT)

general_enrollment_results <- run_general_enrollment_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,                            # NULL = all years in the data
  subgroup_vars        = c("RACE_ETHNICITY", "LEP", "FRL", "GENDER"),
  min_n                = 10
)


# -- Save the results ---------------------------------------------------------

dir.create("output", showWarnings = FALSE)
results_file <- file.path(PKG_ROOT, "output/general_enrollment_results.rds")
saveRDS(general_enrollment_results, results_file)


# =============================================================================
# STEP 3: Render the Report
# =============================================================================

# Copy the template to the package root before rendering.
# This avoids Dropbox file-locking errors that occur when Quarto tries
# to clean up intermediate files inside a synced directory.

qmd_src  <- file.path(PKG_ROOT, "inst/templates/01_Enrollment_Trends_Report.qmd")
qmd_dest <- file.path(PKG_ROOT, "01_Enrollment_Trends_Report.qmd")
file.copy(qmd_src, qmd_dest, overwrite = TRUE)

quarto::quarto_render(
  input          = qmd_dest,
  output_format  = "docx", #docx
  output_file    = "01_Enrollment_Trends_Report.docx",
  execute_params = list(
    pkg_path     = PKG_ROOT,
    spec_file    = SPEC_FILE,
    results_file = results_file,
    min_n        = 10L
  )
)

#message("Done! Open 01_Enrollment_Trends_Report.docx to view the report.")
