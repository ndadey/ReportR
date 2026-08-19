# =============================================================================
# Workshop: Running the Aggregate (School and District) Analysis — MS MAAP
#
# This script runs the Section 5 aggregate analysis using Mississippi MAAP
# data. It produces school- and district-level performance summaries,
# demographic composition trends, residual-based outlier identification,
# within-school achievement gap estimates, and rank-order stability metrics.
#
# Sub-analyses marked [REQUIRES school_results_long] are skipped until that
# data source is confirmed available. Their result slots will be NULL.
#
# Requirements:
#   devtools::load_all() or devtools::install_github("ndadey/ReportR")
#   install.packages("quarto")
#   install.packages("lme4")      # for ICC computation (optional)
#   install.packages("ggrepel")   # for labeled scatter plots (optional)
#
# Run this script from the package root:
#   C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR
# =============================================================================


# =============================================================================
# STEP 1: Setup
# =============================================================================

library(dplyr)
library(quarto)
library(lme4)

PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"
devtools::load_all(PKG_ROOT)

setwd(PKG_ROOT)
source("MS/_setup_light.R")

SPEC_FILE <- file.path(PKG_ROOT, "inst/specs/00_MS_Assessment_Spec.R")
source(SPEC_FILE)

cat("Program:", assessment_spec$program$name, "\n")
cat("Years in data:", sort(unique(student_results_long$YEAR)), "\n")
cat("School ID column:",
    intersect(c("SCHOOL_NUMBER", "SCHOOL_ID"), names(student_results_long))[1], "\n")


# =============================================================================
# STEP 2: Run the Analysis
# =============================================================================

# run_aggregate_analysis() runs all sub-analyses available from student data:
#
#   $performance_trends  — 5.1: school/district means, SDs, proficiency,
#                          improvement classification, regression slopes,
#                          year-to-year correlations, rank orders, ICCs
#   $population_shifts   — 5.3: demographic composition trends and
#                          composition-vs-proficiency correlation
#   $outliers            — 5.4: residual-based school outlier identification,
#                          funnel plot data, stability across years
#   $disaggregated       — 5.5: subgroup proficiency by school, within-school
#                          gaps, gap trend classifications
#   $stability           — 5.6: year-to-year rank-order correlations,
#                          ICC trends
#
#   [SKIPPED] $accountability — requires school_results_long
#   [SKIPPED] $intervention   — requires school_results_long

PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"
devtools::load_all(PKG_ROOT)


aggregate_results <- run_aggregate_analysis(
  student_results_long       = student_results_long,
  assessment_spec            = assessment_spec,
  school_results_long        = NULL,   # set to school_results_long when available
  report_years               = NULL,   # NULL = all years in the data
  min_n_school               = 10,
  min_n_subgroup             = 10,
  improvement_threshold      = 3,
  outlier_sd_threshold       = 2,
  outlier_n_funnel           = 20,
  correlation_flag_threshold = 0.85
)

dir.create("output", showWarnings = FALSE)
results_file <- file.path(PKG_ROOT, "output/aggregate_results.rds")
saveRDS(aggregate_results, results_file)



# -- Explore results -----------------------------------------------------------

# 5.1: How many schools are improving, stable, or declining?
aggregate_results$performance_trends$improvement_classification |>
  count(YEAR, SUBJECT, classification) |>
  tidyr::pivot_wider(names_from = classification, values_from = n, values_fill = 0)

# 5.1: Distribution of school-level regression slopes
aggregate_results$performance_trends$regression_slopes |>
  filter(!is.na(slope)) |>
  group_by(SUBJECT, GRADE) |>
  summarize(median_slope = median(slope), pct_positive = mean(slope > 0), .groups = "drop")

# 5.1: Year-to-year correlation stability
aggregate_results$performance_trends$year_to_year_correlations

# 5.3: Schools with largest composition-proficiency shifts (ELA example)
if (!is.null(aggregate_results$population_shifts$composition_proficiency_join) &&
    nrow(aggregate_results$population_shifts$composition_proficiency_join) > 0) {
  aggregate_results$population_shifts$composition_proficiency_join |>
    filter(SUBJECT == "ELA", !is.na(yoy_change)) |>
    arrange(desc(abs(yoy_change))) |>
    select(1:6) |>
    head(20)
}

# 5.4: Top positive and negative outlier schools
aggregate_results$outliers$positive_outlier_schools |>
  select(1:2, SUBJECT, YEAR, n_tested, pct_proficient, predicted_proficiency, residual) |>
  head(20)

aggregate_results$outliers$negative_outlier_schools |>
  select(1:2, SUBJECT, YEAR, n_tested, pct_proficient, predicted_proficiency, residual) |>
  head(20)

# 5.4: Schools flagged as outliers in 2+ consecutive years
aggregate_results$outliers$outlier_stability |>
  filter(n_years_flagged >= 2) |>
  arrange(desc(n_years_flagged))

# 5.5: Distribution of within-school FRL gap (first binary demo in spec)
if (nrow(aggregate_results$disaggregated$within_school_gaps) > 0) {
  aggregate_results$disaggregated$within_school_gaps |>
    filter(!is.na(gap_estimate)) |>
    group_by(YEAR, SUBJECT, gap_pair) |>
    summarize(median_gap = median(gap_estimate), .groups = "drop")
}

# 5.6: Year pairs with below-threshold stability
aggregate_results$stability$correlation_flag_summary


# -- Save results --------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
results_file <- file.path(PKG_ROOT, "output/aggregate_results.rds")
saveRDS(aggregate_results, results_file)


# =============================================================================
# STEP 3: Render the Report
# =============================================================================

qmd_src  <- file.path(PKG_ROOT, "inst/templates/05_Aggregate_Analysis_Report.qmd")
qmd_dest <- file.path(PKG_ROOT, "05_Aggregate_Analysis_Report.qmd")
file.copy(qmd_src, qmd_dest, overwrite = TRUE)

quarto::quarto_render(
  input          = qmd_dest,
  output_format  = "docx",
  output_file    = "05_Aggregate_Analysis_Report.docx",
  execute_params = list(
    pkg_path     = PKG_ROOT,
    spec_file    = SPEC_FILE,
    results_file = results_file,
    min_n        = 10L
  )
)

message("Done! Open 05_Aggregate_Analysis_Report.docx to view the report.")


# =============================================================================
# [REQUIRES school_results_long — Uncomment when available]
# =============================================================================

# When school_results_long is ready, re-run with:
# aggregate_results <- run_aggregate_analysis(
#   student_results_long = student_results_long,
#   assessment_spec      = assessment_spec,
#   school_results_long  = school_results_long,   # <- provide here
#   ...
# )
#
# This will enable:
#   $accountability    — grade distributions, grade changes, indicator attribution
#   $intervention      — HQIM vs. matched comparison schools
#   $outliers$outlier_characteristics — by co-op, locale, HQIM, accountability grade
#   $population_shifts$by_coop   — demographic composition by co-op
#   $population_shifts$by_locale — demographic composition by locale
