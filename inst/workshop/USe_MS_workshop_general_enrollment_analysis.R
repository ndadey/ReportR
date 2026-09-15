# =============================================================================
# Workshop: Running the General Trend Analysis with ReportR — MS MAAP
#
# This script walks through the three steps to produce a General Trend
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

# run_general_trend_analysis() runs all three Line 1 sub-analyses:
#
#   $score_trends    — mean, SD, and percentile trends by year, subject, grade
#   $proficiency     — percent at or above the policy benchmark over time
#   $distributions   — kernel density plots showing distributional shifts
#
# MS-specific threshold adjustments:
#   score_flat_threshold = 0.2  — scale scores change slowly year-to-year for MAAP
#   prof_flat_threshold  = 0.5  — similarly small pp changes expected

general_trend_results <- run_general_trend_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,    # NULL = all years in the data
  min_n                = 10,
  score_flat_threshold = 0.2,
  prof_flat_threshold  = 0.5
)

# -- Explore the results -------------------------------------------------------


dir.create("output", showWarnings = FALSE)
results_file <- file.path(PKG_ROOT, "output/general_trend_results.rds")
saveRDS(general_trend_results, results_file)

# Optionally export all summary tables to Excel (one tab per table)
export_tables_to_excel(
  general_trend_results,
  file.path(PKG_ROOT, "output/general_trend_tables.xlsx")
)


# =============================================================================
# STEP 3: Render the Report
# =============================================================================

# Copy the template to the working directory before rendering.
# This avoids Dropbox file-locking errors that occur when Quarto tries
# to clean up intermediate files inside a synced directory.

qmd_src  <- file.path(PKG_ROOT, "inst/templates/01_General_Trend_Report.qmd")
qmd_dest <- file.path(PKG_ROOT, "01_General_Trend_Report.qmd")
file.copy(qmd_src, qmd_dest, overwrite = TRUE)

quarto::quarto_render(
  input          = qmd_dest,
  output_format  = "html",   # switch to "docx" for Word output
  output_file    = "01_General_Trend_Report.html",
  execute_params = list(
    pkg_path     = PKG_ROOT,
    spec_file    = SPEC_FILE,
    results_file = results_file,
    min_n        = 10L
  )
)

message("Done! Open 01_General_Trend_Report.html to view the report.")
