# =============================================================================
# run_state.R — Generic single-state runner for ReportR
#
# USAGE:
#   1. Make sure states/_config.R exists (copy from states/_config_template.R
#      and fill in PKG_ROOT + output paths).
#   2. Edit the user settings below.
#   3. Source this file or run it section by section in RStudio.
#
# TO SWITCH STATES: change STATE <- "MS" to STATE <- "AR", "DEMO", etc.
# =============================================================================

PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"

# ---- User settings ----------------------------------------------------------
STATE        <- "MS"   # change to "AR", "DEMO", etc.
ANALYSES     <- "aggregate" #c("trend", "enrollment", "cohort", "anomaly", "aggregate")
RENDER       <- TRUE
MIN_N        <- 10L
REPORT_YEARS <- 2022:2026  # years shown in report main body; NULL = show all years
RE_EXPORT_XLSX  <- FALSE  # TRUE = skip analysis, regenerate xlsx from existing RDS only
RE_EXPORT_LINES <- ANALYSES #"trend" # which line(s): "trend","enrollment","cohort","anomaly","aggregate"
# -----------------------------------------------------------------------------

devtools::load_all(PKG_ROOT)
source(file.path(PKG_ROOT, "states/_config.R"))

# -----------------------------------------------------------------------------
# Private helper: copy QMD to output_dir, render there, remove QMD after.
# -----------------------------------------------------------------------------
.render_report <- function(qmd_name, rds_path, output_dir,
                            pkg_path, spec_file, min_n,
                            report_years = NULL) {
  # Prefer source-tree template (ensures edits are picked up); fall back to installed package
  qmd_src <- file.path(pkg_path, "inst", "templates", qmd_name)
  if (!file.exists(qmd_src)) {
    qmd_src <- system.file("templates", qmd_name, package = "ReportR")
  }
  if (!file.exists(qmd_src)) {
    warning("Template not found, skipping render: ", qmd_name)
    return(invisible(NULL))
  }

  out_file  <- sub("\\.qmd$", ".docx", qmd_name)
  qmd_local <- file.path(output_dir, qmd_name)
  file.copy(qmd_src, qmd_local, overwrite = TRUE)

  quarto::quarto_render(
    input          = qmd_local,
    output_format  = "docx",
    output_file    = out_file,
    execute_params = list(
      pkg_path     = pkg_path,
      spec_file    = spec_file,
      results_file = rds_path,
      min_n        = min_n,
      report_years = report_years
    )
  )

  file.remove(qmd_local)
  message("  Report: ", file.path(output_dir, out_file))
  invisible(NULL)
}

# Helper: write every table in a results list to xlsx (skipped if openxlsx absent)
.write_xlsx <- function(results, path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) return(invisible(NULL))
  export_tables_to_excel(results, path)
}

# -----------------------------------------------------------------------------
# run_state() — main entry point
# -----------------------------------------------------------------------------
run_state <- function(state_code,
                      analyses     = c("trend", "enrollment", "cohort", "anomaly", "aggregate"),
                      render       = TRUE,
                      min_n        = 10L,
                      report_years = NULL,
                      config       = state_configs) {

  cfg <- config[[state_code]]
  if (is.null(cfg)) stop("No config entry found for state: ", state_code)

  pkg_path <- PKG_ROOT  # always use source tree so QMD templates and R/ reflect edits

  # 1. Load spec
  source(cfg$spec, local = TRUE)
  attr(assessment_spec, "spec_file") <- cfg$spec

  # 2. Load and munge data (produces student_results_long)
  source(cfg$setup, local = TRUE)

  # 3. Prepare output directory
  output_dir <- cfg$output
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  results <- list()

  # ---- Line 1: General Trend ------------------------------------------------
  if ("trend" %in% analyses) {
    message("[", state_code, "] Line 1 — General Trend...")
    results$trend <- run_general_trend_analysis(
      student_results_long, assessment_spec, min_n = min_n
    )
    rds_path <- file.path(output_dir, "general_trend_results.rds")
    saveRDS(results$trend, rds_path)
    .write_xlsx(
      results$trend,
      file.path(output_dir, "general_trend_full.xlsx")
    )
    if (render) .render_report("01_General_Trend_Report.qmd", rds_path, output_dir,
                                pkg_path, cfg$spec, min_n, report_years)
  }

  # ---- Line 1.2: Enrollment -------------------------------------------------
  if ("enrollment" %in% analyses) {
    message("[", state_code, "] Line 1.2 — Enrollment...")
    results$enrollment <- run_general_enrollment_analysis(
      student_results_long, assessment_spec, min_n = min_n
    )
    rds_path <- file.path(output_dir, "enrollment_results.rds")
    saveRDS(results$enrollment, rds_path)
    .write_xlsx(
      results$enrollment,
      file.path(output_dir, "enrollment_full.xlsx")
    )
    if (render) .render_report("01_Enrollment_Trends_Report.qmd", rds_path, output_dir,
                                pkg_path, cfg$spec, min_n, report_years)
  }

  # ---- Line 1.3: Cohort -----------------------------------------------------
  if ("cohort" %in% analyses) {
    message("[", state_code, "] Line 1.3 — Cohort...")
    results$cohort <- run_general_cohort(
      student_results_long = student_results_long,
      assessment_spec      = assessment_spec,
      output_dir           = file.path(output_dir, "cohort"),
      render_report        = render,
      report_years         = report_years,
      min_n                = min_n
    )
    if (!is.null(results$cohort)) {
      .write_xlsx(
        results$cohort,
        file.path(output_dir, "cohort_full.xlsx")
      )
    }
  }

  # ---- Line 4: Anomaly / Score Quality --------------------------------------
  if ("anomaly" %in% analyses) {
    message("[", state_code, "] Line 4 — Anomaly...")
    results$anomaly <- run_anomaly_analysis(
      student_results_long, assessment_spec, min_n = min_n
    )
    rds_path <- file.path(output_dir, "anomaly_results.rds")
    saveRDS(results$anomaly, rds_path)
    .write_xlsx(
      results$anomaly,
      file.path(output_dir, "anomaly_full.xlsx")
    )
    if (render) .render_report("04_Anomaly_Report.qmd", rds_path, output_dir,
                                pkg_path, cfg$spec, min_n, report_years)
  }

  # ---- Line 5: Aggregate ----------------------------------------------------
  if ("aggregate" %in% analyses) {
    message("[", state_code, "] Line 5 — Aggregate...")
    results$aggregate <- run_aggregate_analysis(
      student_results_long, assessment_spec, min_n_school = min_n
    )
    rds_path <- file.path(output_dir, "aggregate_results.rds")
    saveRDS(results$aggregate, rds_path)
    .write_xlsx(
      results$aggregate,
      file.path(output_dir, "aggregate_full.xlsx")
    )
    if (render) .render_report("05_Aggregate_Analysis_Report.qmd", rds_path, output_dir,
                                pkg_path, cfg$spec, min_n, report_years)
  }

  message("[", state_code, "] Complete. Outputs: ", output_dir)
  invisible(results)
}

# =============================================================================
# Run
# =============================================================================
if (!RE_EXPORT_XLSX) {
  run_state(STATE, analyses = ANALYSES, render = RENDER, min_n = MIN_N,
            report_years = REPORT_YEARS)
}

# =============================================================================
# Re-export xlsx from existing RDS (skips analysis — set RE_EXPORT_XLSX <- TRUE)
# =============================================================================
if (RE_EXPORT_XLSX) {
  cfg <- state_configs[[STATE]]
  rds_map <- list(
    trend      = "general_trend_results.rds",
    enrollment = "enrollment_results.rds",
    anomaly    = "anomaly_results.rds",
    aggregate  = "aggregate_results.rds",
    cohort     = file.path("cohort", "cohort_results.rds")
  )
  xlsx_map <- list(
    trend      = "general_trend_full.xlsx",
    enrollment = "enrollment_full.xlsx",
    anomaly    = "anomaly_full.xlsx",
    aggregate  = "aggregate_full.xlsx",
    cohort     = "cohort_full.xlsx"
  )
  for (line in RE_EXPORT_LINES) {
    rds_path  <- file.path(cfg$output, rds_map[[line]])
    xlsx_path <- file.path(cfg$output, xlsx_map[[line]])
    if (!file.exists(rds_path)) {
      message("  [skip] RDS not found: ", rds_path)
      next
    }
    message("Re-exporting xlsx for: ", line)
    .write_xlsx(readRDS(rds_path), xlsx_path)
  }
}
