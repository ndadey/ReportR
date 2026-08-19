# =============================================================================
# Workshop: Running the Score Quality Analysis with ReportR
#
# This script walks through the three steps to produce a Score Quality
# Analysis report using the ReportR package and the built-in SGPdata demo.
#
# Requirements:
#   install.packages("devtools")
#   devtools::install_github("ndadey/ReportR")
#   install.packages("SGPdata")
#   install.packages("quarto")
# =============================================================================


# =============================================================================
# STEP 1: Setup — Long Format Data & Assessment Spec
# =============================================================================
PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"
devtools::load_all(PKG_ROOT)
#library(ReportR)
#library(SGPdata)
library(dplyr)
library(quarto)

# -- 1a. Load and standardize student data ------------------------------------
setwd(PKG_ROOT)
source("MS/_setup_light.R")

# -- 1b. Confirm the MS spec is loaded ----------------------------------------

SPEC_FILE <- file.path(PKG_ROOT, "inst/specs/00_MS_Assessment_Spec.R")
source(SPEC_FILE)   # puts assessment_spec in scope (or refreshes it)

# =============================================================================
# STEP 2: Create the Results List Object
# =============================================================================

# run_anomaly_analysis() runs all five Score Quality sub-analyses:
#
#   $loss_hoss        — pileups near the score floor (LOSS) or ceiling (HOSS)
#   $score_integrity  — scores outside valid range; level-cut mismatches
#   $outliers         — unusually large year-to-year changes (±2/3 SD)
#   $correlations     — cohort Spearman correlations across adjacent grades
#   $distributions    — flat, bimodal, or skewed score distributions
#
# report_years filters to a subset of years (NULL = use all years in the data)

anomaly_results <- run_anomaly_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,
  min_n                = 10
)

# Save the results — the report reads from this file so analysis does not
# need to re-run every time you adjust the report layout.
dir.create("output", showWarnings = FALSE)
saveRDS(anomaly_results, "output/anomaly_results.rds")

# Optionally export all summary tables to Excel (one tab per table).
# Requires: install.packages("openxlsx")
export_tables_to_excel(anomaly_results, "output/anomaly_tables.xlsx")


# =============================================================================
# STEP 3: Output via the Quarto Anomaly Analysis Report
# =============================================================================

# Copy the report template to your working directory so you can edit it.
# Alternatively, you could render directly from the inst/templates path inside
# the package directory (found via system.file("templates", package = "ReportR")).
file.copy(
  system.file("templates/04_Anomaly_Report.qmd", package = "ReportR"),
  "04_Anomaly_Report.qmd",
  overwrite = FALSE   # set TRUE to overwrite if you already have a copy
)

# Render the report to HTML or Word (.docx)
# pkg_path tells the report where the package lives so it can load functions.
# spec_file points to the assessment spec.
# results_file points to the saved RDS from Step 2.

quarto::quarto_render(
  input         = "inst/templates/04_Anomaly_Report.qmd",
                  #"inst/templates/04_Anomaly_Report.qmd",
  output_format = "docx", #"docx",
  output_file   = "04_Anomaly_Report.docx",
  execute_params = list(
    pkg_path     = find.package("ReportR"),
    spec_file    = SPEC_FILE,
    results_file = file.path(getwd(), "output/anomaly_results.rds"),
    min_n        = 10L
  )
)

# The report will be saved as 04_Anomaly_Report.html or 04_Anomaly_Report.docx 
#in your working directory.
message("Done! Open 04_Anomaly_Report.html or docx to view the report.")




#------- 
sage("Done! Report written to: ", final_docx)


# =============================================================================
# STEP 3B: Anomaly Report
# =============================================================================

ANOMALY_RESULTS_FILE <- file.path(PKG_ROOT, "output/anomaly_results_4.rds")
ANOMALY_OUTPUT_FILE  <- "04_Anomaly_Report.docx"

message("Running anomaly analysis...")
anomaly_results <- run_anomaly_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,
  min_n                = 10L
)
saveRDS(anomaly_results, ANOMALY_RESULTS_FILE)

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




