# =============================================================================
# Workshop: Running the General Cohort Analysis with ReportR — MS MAAP
#
# This script walks through the steps to produce a General Cohort Analysis
# (Line 1.3) report using the ReportR package and real Mississippi MAAP data.
#
# Requirements:
#   devtools::load_all() or devtools::install_github("ndadey/ReportR")
#   install.packages("quarto")
#   install.packages("ggalluvial")  # optional, for Sankey transition plots
#   install.packages("openxlsx")   # optional, for Excel export
#
# Run this script from the package root:
#   C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR
# =============================================================================


# =============================================================================
# STEP 1: Setup — Load Package, Spec, and Data
# =============================================================================

library(dplyr)
library(quarto)

# Load package functions from source (development mode).
# Switch to library(ReportR) once the package is installed.
PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"
devtools::load_all(PKG_ROOT)

# -- 1a. Load the MS assessment spec ------------------------------------------
#
# The spec defines everything assessment-specific: subjects, grades, scale score
# ranges (LOSS/HOSS), achievement level labels, cut scores, and the policy
# benchmark. The MS MAAP spec lives at inst/specs/00_MS_Assessment_Spec.R.

SPEC_FILE <- file.path(PKG_ROOT, "inst/specs/00_MS_Assessment_Spec.R")
source(SPEC_FILE)   # puts assessment_spec in scope

# Attach the spec file path as an attribute so run_general_cohort() can pass
# it to the QMD for sourcing inside the report.
attr(assessment_spec, "spec_file") <- SPEC_FILE

# Take a look at the spec
str(assessment_spec, max.level = 2)
assessment_spec$years$cohort_anchor_grade  # grade that defines cohort entry

# -- 1b. Load and standardize MS student data ---------------------------------
#
# _setup_light.R loads the MS MAAP student data into `student_results_long`
# and runs standardize_student_results() so column names match the package's
# data contract (YEAR, GRADE, SUBJECT, SCALE_SCORE, ACHIEVEMENT_LEVEL, etc.).
# Always standardize BEFORE filtering by year.

setwd(PKG_ROOT)
source("MS/_setup_light.R")   # loads student_results_long

# Inspect the data
glimpse(student_results_long)
sort(unique(student_results_long$YEAR))   # confirm year range


# =============================================================================
# STEP 2: Run the Cohort Analysis
# =============================================================================
#
# run_cohort_analysis() runs all ten sub-analyses:
#
#   $summary            — mean scores, SD, proficiency, level counts by cohort × grade
#   $demographics       — demographic composition by cohort × grade
#   $transitions        — achievement level transitions across adjacent grades
#   $rate_of_change     — improving / stable / declining trajectories
#   $correlations       — Pearson r of adjacent-grade scale scores
#   $crosssectional     — cohort vs. cross-sectional comparison data
#   $mobility           — entering / continuing / exiting classification
#   $mobility_by_achiev — mobility crossed with achievement level
#   $persistence        — original cohort retention from anchor grade
#   $enrollment_diag    — z-score flagging of unusual enrollment changes
#
# Note: analyses requiring persistent STUDENT_ID (transitions, correlations,
# mobility, persistence) return 0 rows when IDs are absent or non-persistent.

# Define output directory
OUTPUT_DIR <- file.path(PKG_ROOT, "MS/output/cohort")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Save RDS to tempdir first (Dropbox-safe), then copy to OUTPUT_DIR.
# run_general_cohort() handles this automatically.

cohort_results <- run_cohort_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  min_n                = 10
)

tapply(student_results_long$STUDENT_ID, 
       student_results_long$YEAR,
       function(x) length(unique(x)))

# =============================================================================
# STEP 3: Explore the Results
# =============================================================================

# -- Cohort summary table -----------------------------------------------------
# One row per SUBJECT × COHORT_YEAR × GRADE × YEAR
# Columns: N, MEAN_SCORE, SD_SCORE, MEDIAN_SCORE, Q1_SCORE, Q3_SCORE,
#          PCT_PROFICIENT, N_<Level> for each achievement level
head(cohort_results$summary)

# -- Enrollment diagnostics ---------------------------------------------------
# Flagged cells where enrollment deviates by >1.5 SD from the grade mean
cohort_results$enrollment_diag$narrative
cohort_results$enrollment_diag$table |> filter(FLAGGED)

# -- Cohort persistence -------------------------------------------------------
# Track how many anchor-grade students appear in each subsequent grade
cohort_results$persistence

# -- Mobility (entering / continuing / exiting) -------------------------------
cohort_results$mobility
cohort_results$mobility_by_achiev

# -- Score trends plots -------------------------------------------------------
# One plot per subject, grade on x-axis, cohort year as color
cohort_results$plot$mean_scores$ELA
cohort_results$plot$mean_scores$MATH

# -- Proficiency rate plots ---------------------------------------------------
cohort_results$plot$proficiency_rates$ELA

# -- Distribution plots -------------------------------------------------------
cohort_results$plot$distributions$ELA

# -- Mobility distribution plots ----------------------------------------------
# NULL if no persistent student IDs
cohort_results$plot$mobility_distribution$ELA
cohort_results$plot$mobility_by_achievement$MATH

# -- Achievement transitions --------------------------------------------------
cohort_results$transitions
cohort_results$plot$sankey$ELA   # NULL if ggalluvial not installed

# -- Score correlations across adjacent grades --------------------------------
cohort_results$correlations
cohort_results$plot$correlations$ELA


# =============================================================================
# STEP 4: Save Results and Render the Report
# =============================================================================
#
# run_general_cohort() combines STEP 2 + STEP 4 in one call.
# It saves the RDS using the Dropbox-safe tempdir pattern and renders the QMD.

cohort_results <- run_general_cohort(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  output_dir           = OUTPUT_DIR,
  report_years         = NULL,   # NULL = all years in the data
  min_n                = 10,
  render_report        = TRUE,
  open_report          = TRUE
)

message("Done! Check ", OUTPUT_DIR, " for the report and RDS.")
