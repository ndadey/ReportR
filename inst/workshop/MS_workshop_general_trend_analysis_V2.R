# =============================================================================
# Workshop: Running the General Trend Analysis with ReportR — MS MAAP
# =============================================================================


# =============================================================================
# STEP 1: Setup — Load Package, Spec, and Data
# =============================================================================

library(dplyr)
library(quarto)

PKG_ROOT  <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"
SPEC_FILE <- file.path(PKG_ROOT, "inst/specs/00_MS_Assessment_Spec.R")

devtools::load_all(PKG_ROOT)

setwd(PKG_ROOT)
source("MS/_setup_light.R")
source(SPEC_FILE)


# =============================================================================
# STEP 2: Run the Analysis
# =============================================================================

dir.create(file.path(PKG_ROOT, "output"), recursive = TRUE, showWarnings = FALSE)

general_trend_results <- run_general_trend_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = NULL,
  min_n                = 10,
  score_flat_threshold = 0.2,
  prof_flat_threshold  = 0.5
)

results_file <- file.path(PKG_ROOT, "output/general_trend_results.rds")
saveRDS(general_trend_results, results_file)


# =============================================================================
# STEP 3: Render the General Trend Report
# =============================================================================

OUTPUT_FILE <- "01_General_Trend_Report_12PM.docx"
OUTPUT_DIR  <- file.path(PKG_ROOT, "output")

qmd_src   <- file.path(PKG_ROOT, "inst/templates/01_General_Trend_Report.qmd")
qmd_local <- file.path(OUTPUT_DIR, "01_General_Trend_Report.qmd")

file.copy(qmd_src, qmd_local, overwrite = TRUE)

quarto::quarto_render(
  input          = qmd_local,
  output_format  = "docx",
  output_file    = OUTPUT_FILE,
  execute_params = list(
    pkg_path     = PKG_ROOT,
    spec_file    = SPEC_FILE,
    results_file = results_file,
    min_n        = 10L
  )
)

file.remove(qmd_local)
message("Done! Report written to: ", file.path(OUTPUT_DIR, OUTPUT_FILE))


# =============================================================================
# STEP 4: Render the General Trend Report
# =============================================================================
openxlsx::write.xlsx(list("scale_scores"         = general_trend_results$score_trends$table,
                          "proficiency_percents" = general_trend_results$proficiency$table,
                          "level_percents"       = general_trend_results$achievement_transitions$matrices$long_table,
                          "level_summary"        = general_trend_results$achievement_transitions$matrices$summary_table), 
                     file = 'Output/01_General_Trend_043026_V2.xlsx')


