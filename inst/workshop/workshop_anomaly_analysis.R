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

library(ReportR)
library(SGPdata)
library(dplyr)
library(quarto)

# -- 1a. Load and standardize student data ------------------------------------
#
# standardize_student_results() renames columns to the canonical names used
# throughout the package (e.g. CONTENT_AREA -> SUBJECT, SS -> SCALE_SCORE)
# and converts "YYYY_YYYY" year strings to integers. The 
# standardize_student_results() function is not comprehensive, but is mean to help
# with cleaning, at least a little. 

student_results_long <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results()

# Take a look at the data structure
glimpse(student_results_long)

# -- 1b. Load the assessment spec ---------------------------------------------
#
# The spec defines everything assessment-specific: subjects, grades, scale
# score ranges (LOSS/HOSS), achievement level labels, and cut scores.
# A template for building your own spec is at:
#   system.file("specs/00_TEMPLATE_Spec.R", package = "ReportR")

source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))

# Take a look at what the spec contains
str(assessment_spec, max.level = 2)


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

# Explore the results before rendering the report
anomaly_results$loss_hoss$table
anomaly_results$loss_hoss$plot_hoss_cleveland
anomaly_results$outliers$text$narrative
anomaly_results$correlations$plot$line

# Save the results — the report reads from this file so analysis does not
# need to re-run every time you adjust the report layout.
dir.create("output", showWarnings = FALSE)
saveRDS(anomaly_results, "output/anomaly_results.rds")


# =============================================================================
# STEP 3: Output via the Quarto Anomaly Analysis Report
# =============================================================================

# Copy the report template to your working directory so you can edit it.
# Alternatively, you could run directly from the ist/templates within the
# package directory (which can be found in 
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
  input         = "04_Anomaly_Report.qmd",
                  #"inst/templates/04_Anomaly_Report.qmd",
  output_format = "html", #"docx",
  output_file   = "04_Anomaly_Report.html",
  execute_params = list(
    pkg_path     = find.package("ReportR"),
    spec_file    = system.file("specs/00_DEMO_Assessment_Spec.R",
                              package = "ReportR"),
    results_file = file.path(getwd(), "output/anomaly_results.rds"),
    min_n        = 10L
  )
)

# The report will be saved as 04_Anomaly_Report.html or 04_Anomaly_Report.docx 
#in your working directory.
message("Done! Open 04_Anomaly_Report.docx to view the report.")
