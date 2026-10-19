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
PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"
devtools::load_all(PKG_ROOT)

general_trend_results <- run_general_trend_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,    # NULL = all years in the data
  min_n                = 10,
  score_flat_threshold = 0.2,
  prof_flat_threshold  = 0.5
)


dir.create(file.path(PKG_ROOT, "output"), recursive = TRUE, showWarnings = FALSE)
RENDER_DIR   <- file.path(tempdir(), "ReportR_render")
dir.create(RENDER_DIR, recursive = TRUE, showWarnings = FALSE)
# Save to tempdir first to avoid Dropbox sync locking, then copy to output/
results_file_tmp <- file.path(RENDER_DIR, "general_trend_results.rds")
results_file     <- file.path(PKG_ROOT,   "output/general_trend_results.rds")
saveRDS(general_trend_results, results_file_tmp)
file.copy(results_file_tmp, results_file, overwrite = TRUE)

# Optionally export all summary tables to Excel (one tab per table)
#need to write out 
  list(general_trend_results$score_trends$table,
  general_trend_results$proficiency$table,
  general_trend_results$achievement_transitions$matrices$long_table,
  general_trend_results$achievement_transitions$matrices$summary_table)

#export_tables_to_excel(
#  general_trend_results,
#  file.path(PKG_ROOT, "output/general_trend_tables.xlsx")
#)


# =============================================================================
# STEP 3A: Gernal
# =============================================================================

OUTPUT_FILE <- "01_General_Trend_Report.docx"
OUTPUT_DIR  <- file.path(PKG_ROOT, "output")

qmd_src   <- file.path(PKG_ROOT, "inst/templates/01_General_Trend_Report.qmd")
qmd_local <- file.path(RENDER_DIR, "01_General_Trend_Report.qmd")

copied <- file.copy(qmd_src, qmd_local, overwrite = TRUE)
if (!copied) stop("Could not copy QMD template. Check that the file exists at:\n  ", qmd_src)

quarto::quarto_render(
  input          = qmd_src,
  output_format  = "docx",
  output_file    = OUTPUT_FILE,
  execute_params = list(
    pkg_path     = PKG_ROOT,
    spec_file    = SPEC_FILE,
    results_file = results_file_tmp,
    min_n        = 10L
  )
)

final_docx <- file.path(OUTPUT_DIR, OUTPUT_FILE)
file.copy(file.path(RENDER_DIR, OUTPUT_FILE), final_docx, overwrite = TRUE)
message("Done! Report written to: ", final_docx)


# =============================================================================
# STEP 3B: Anomaly Report
# =============================================================================

ANOMALY_OUTPUT_FILE      <- "04_Anomaly_Report.docx"
# Save RDS to tempdir to avoid Dropbox sync locking during write
ANOMALY_RESULTS_FILE     <- file.path(RENDER_DIR, "anomaly_results.rds")
ANOMALY_RESULTS_FILE_OUT <- file.path(OUTPUT_DIR,  "anomaly_results.rds")

message("Running anomaly analysis...")
anomaly_results <- run_anomaly_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,
  min_n                = 10L
)
saveRDS(anomaly_results, ANOMALY_RESULTS_FILE)
file.copy(ANOMALY_RESULTS_FILE, ANOMALY_RESULTS_FILE_OUT, overwrite = TRUE)

qmd_src_anomaly   <- file.path(PKG_ROOT, "inst/templates/04_Anomaly_Report.qmd")
qmd_local_anomaly <- file.path(RENDER_DIR, "04_Anomaly_Report.qmd")

copied <- file.copy(qmd_src_anomaly, qmd_local_anomaly, overwrite = TRUE)
if (!copied) stop("Could not copy QMD template. Check that the file exists at:\n  ", qmd_src_anomaly)

quarto::quarto_render(
  input          = qmd_local_anomaly,
  output_format  = "docx",
  output_file    = ANOMALY_OUTPUT_FILE,
  execute_params = list(
    pkg_path     = PKG_ROOT,
    spec_file    = SPEC_FILE,
    results_file = ANOMALY_RESULTS_FILE,
    min_n        = 10L
  )
)

final_anomaly_docx <- file.path(OUTPUT_DIR, ANOMALY_OUTPUT_FILE)
file.copy(file.path(RENDER_DIR, ANOMALY_OUTPUT_FILE), final_anomaly_docx, overwrite = TRUE)
message("Done! Anomaly report written to: ", final_anomaly_docx)
