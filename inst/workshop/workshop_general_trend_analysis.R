# =============================================================================
# Workshop: Running the General Trend Analysis with ReportR
#
# This script walks through the three steps to produce a General Trend
# Analysis report using the ReportR package and the built-in SGPdata demo.
#
# Requirements:
#   install.packages("devtools")
#   devtools::install_github("ndadey/ReportR", ref = "integrate-general-trend")
#   install.packages("SGPdata")
#   install.packages("quarto")
#
# Note: once the integrate-general-trend branch is merged into main, the
# ref argument can be dropped:
#   devtools::install_github("ndadey/ReportR")
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
# and converts "YYYY_YYYY" year strings to integers.
#
# Important: always standardize BEFORE filtering by year. Raw SGPdata uses
# "YYYY_YYYY" strings — filtering by integer year first returns 0 rows.

student_results_long <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results()

# Take a look at the data structure
glimpse(student_results_long)

# -- 1b. Load the assessment spec ---------------------------------------------
#
# The spec defines everything assessment-specific: subjects, grades, scale
# score ranges (LOSS/HOSS), achievement level labels, cut scores, and the
# policy benchmark (the "proficient" cut used in proficiency calculations).
# A template for building your own spec is at:
#   system.file("specs/00_TEMPLATE_Spec.R", package = "ReportR")

source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))

# Take a look at what the spec contains
str(assessment_spec, max.level = 2)

# The policy benchmark determines what counts as "proficient":
assessment_spec$achievement_levels$policy_benchmark


# =============================================================================
# STEP 2: Create the Results List Object
# =============================================================================

# run_general_trend_analysis() runs all three Line 1 sub-analyses:
#
#   $score_trends    — mean, SD, and percentile trends by year, subject, grade
#   $proficiency     — percent at or above the policy benchmark over time
#   $distributions   — kernel density plots showing distributional shifts
#
# report_years filters to a subset of years (NULL = use all years in the data)

general_trend_results <- run_general_trend_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,
  min_n                = 10
)

# -- Explore the results -------------------------------------------------------

# Score trends: report-ready wide table (grades as rows, year-block columns)
general_trend_results$score_trends$table

# Score trend plots
general_trend_results$score_trends$plot$mean_trend      # mean over time by grade
general_trend_results$score_trends$plot$mean_sd         # mean ± 1 SD bands
general_trend_results$score_trends$plot$grade_profile   # grade on x, year as colour
general_trend_results$score_trends$plot$median_iqr      # median with IQR bands

# Score trend narrative (plain-language summary of first-to-last year changes)
general_trend_results$score_trends$text$narrative

# Proficiency: percent proficient table
general_trend_results$proficiency$table

# Proficiency plots
general_trend_results$proficiency$plot$trend    # trend lines by grade
general_trend_results$proficiency$plot$heatmap  # grade × year heatmap

# Proficiency narrative
general_trend_results$proficiency$text$narrative

# Score distributions: one density plot per subject (years overlaid, faceted by grade)
general_trend_results$distributions$plot$density[["ELA"]]
general_trend_results$distributions$plot$density[["MATHEMATICS"]]

# -- Save the results ---------------------------------------------------------
#
# The report reads from this file so analysis does not need to re-run every
# time you adjust the report layout.

dir.create("output", showWarnings = FALSE)
saveRDS(general_trend_results, "output/general_trend_results.rds")

# Optionally export all summary tables to Excel (one tab per table).
# Requires: install.packages("openxlsx")
export_tables_to_excel(general_trend_results, "output/general_trend_tables.xlsx")


# =============================================================================
# STEP 3: Output via the Quarto General Trend Report
# =============================================================================

# Copy the report template to your working directory so you can edit it.
# Alternatively, you could render directly from the inst/templates path inside
# the package directory (found via system.file("templates", package = "ReportR")).
file.copy(
  system.file("templates/01_General_Trend_Report.qmd", package = "ReportR"),
  "01_General_Trend_Report.qmd",
  overwrite = FALSE   # set TRUE to overwrite if you already have a copy
)

# Render the report to HTML or Word (.docx)
# pkg_path tells the report where the package lives so it can load functions.
# spec_file points to the assessment spec.
# results_file points to the saved RDS from Step 2.

quarto::quarto_render(
  input          = "01_General_Trend_Report.qmd",
  output_format  = "html", #"docx",
  output_file    = "01_General_Trend_Report.html",
  execute_params = list(
    pkg_path     = find.package("ReportR"),
    spec_file    = system.file("specs/00_DEMO_Assessment_Spec.R",
                               package = "ReportR"),
    results_file = file.path(getwd(), "output/general_trend_results.rds"),
    min_n        = 10L
  )
)

# The report will be saved as 01_General_Trend_Report.html or .docx
# in your working directory.
message("Done! Open 01_General_Trend_Report.html or docx to view the report.")
