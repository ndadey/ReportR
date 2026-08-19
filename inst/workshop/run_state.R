# =============================================================================
# run_state.R — Generic single-state runner for ReportR
#
# USAGE:
#   1. Make sure states/_config.R exists (copy from states/_config_template.R
#      and fill in PKG_ROOT + output paths).
#   2. Edit STATE and ANALYSES below.
#   3. Source this file or run it section by section in RStudio.
#
# TO SWITCH STATES: change STATE <- "MS" to STATE <- "AR", "DEMO", etc.
# =============================================================================

PKG_ROOT <- "C:/Users/NathanDadey/CenterforAssessment Dropbox/Nathan Dadey/Git/ReportR"

# ---- User settings ----------------------------------------------------------
STATE    <- "MS"   # change to "AR", "DEMO", etc.
ANALYSES <- c("trend", "enrollment", "cohort", "anomaly", "aggregate")
RENDER   <- TRUE
MIN_N    <- 10L
# -----------------------------------------------------------------------------

devtools::load_all(PKG_ROOT)
source(file.path(PKG_ROOT, "states/_config.R"))

# -----------------------------------------------------------------------------
# Private helper: copy QMD to tempdir, render, copy output back.
# Follows the Dropbox-safe pattern required by this project.
# -----------------------------------------------------------------------------
.render_report <- function(qmd_name, rds_tmp, output_dir, render_dir,
                            pkg_path, spec_file, min_n) {
  qmd_src <- system.file("templates", qmd_name, package = "ReportR")
  if (!nzchar(qmd_src)) {
    qmd_src <- file.path(pkg_path, "inst", "templates", qmd_name)
  }
  if (!file.exists(qmd_src)) {
    warning("Template not found, skipping render: ", qmd_name)
    return(invisible(NULL))
  }

  out_file  <- sub("\\.qmd$", ".docx", qmd_name)
  qmd_local <- file.path(render_dir, qmd_name)
  file.copy(qmd_src, qmd_local, overwrite = TRUE)

  quarto::quarto_render(
    input          = qmd_local,
    output_format  = "docx",
    output_file    = out_file,
    execute_params = list(
      pkg_path     = pkg_path,
      spec_file    = spec_file,
      results_file = rds_tmp,
      min_n        = min_n
    )
  )

  file.copy(file.path(render_dir, out_file),
            file.path(output_dir, out_file), overwrite = TRUE)
  message("  Report: ", file.path(output_dir, out_file))
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# run_state() — main entry point
# -----------------------------------------------------------------------------
run_state <- function(state_code,
                      analyses = c("trend", "enrollment", "cohort", "anomaly", "aggregate"),
                      render   = TRUE,
                      min_n    = 10L,
                      config   = state_configs) {

  cfg <- config[[state_code]]
  if (is.null(cfg)) stop("No config entry found for state: ", state_code)

  pkg_path <- tryCatch(find.package("ReportR"), error = function(e) PKG_ROOT)

  # 1. Load spec
  source(cfg$spec, local = TRUE)
  attr(assessment_spec, "spec_file") <- cfg$spec

  # 2. Load and munge data (produces student_results_long)
  source(cfg$setup, local = TRUE)

  # 3. Prepare output directories
  output_dir <- cfg$output
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  render_dir <- file.path(tempdir(), paste0("ReportR_", state_code))
  dir.create(render_dir, showWarnings = FALSE)

  results <- list()

  # ---- Line 1: General Trend ------------------------------------------------
  if ("trend" %in% analyses) {
    message("[", state_code, "] Line 1 — General Trend...")
    results$trend <- run_general_trend_analysis(
      student_results_long, assessment_spec, min_n = min_n
    )
    rds_tmp <- file.path(render_dir, "general_trend_results.rds")
    saveRDS(results$trend, rds_tmp)
    file.copy(rds_tmp, file.path(output_dir, "general_trend_results.rds"), overwrite = TRUE)
    if (render) .render_report("01_General_Trend_Report.qmd", rds_tmp, output_dir,
                                render_dir, pkg_path, cfg$spec, min_n)
  }

  # ---- Line 1.2: Enrollment -------------------------------------------------
  if ("enrollment" %in% analyses) {
    message("[", state_code, "] Line 1.2 — Enrollment...")
    results$enrollment <- run_general_enrollment_analysis(
      student_results_long, assessment_spec, min_n = min_n
    )
    rds_tmp <- file.path(render_dir, "enrollment_results.rds")
    saveRDS(results$enrollment, rds_tmp)
    file.copy(rds_tmp, file.path(output_dir, "enrollment_results.rds"), overwrite = TRUE)
    if (render) .render_report("01_Enrollment_Trends_Report.qmd", rds_tmp, output_dir,
                                render_dir, pkg_path, cfg$spec, min_n)
  }

  # ---- Line 1.3: Cohort -----------------------------------------------------
  if ("cohort" %in% analyses) {
    message("[", state_code, "] Line 1.3 — Cohort...")
    results$cohort <- run_general_cohort(
      student_results_long = student_results_long,
      assessment_spec      = assessment_spec,
      output_dir           = file.path(output_dir, "cohort"),
      render_report        = render,
      min_n                = min_n
    )
  }

  # ---- Line 4: Anomaly / Score Quality --------------------------------------
  if ("anomaly" %in% analyses) {
    message("[", state_code, "] Line 4 — Anomaly...")
    results$anomaly <- run_anomaly_analysis(
      student_results_long, assessment_spec, min_n = min_n
    )
    rds_tmp <- file.path(render_dir, "anomaly_results.rds")
    saveRDS(results$anomaly, rds_tmp)
    file.copy(rds_tmp, file.path(output_dir, "anomaly_results.rds"), overwrite = TRUE)
    if (render) .render_report("04_Anomaly_Report.qmd", rds_tmp, output_dir,
                                render_dir, pkg_path, cfg$spec, min_n)
  }

  # ---- Line 5: Aggregate ----------------------------------------------------
  if ("aggregate" %in% analyses) {
    message("[", state_code, "] Line 5 — Aggregate...")
    results$aggregate <- run_aggregate_analysis(
      student_results_long, assessment_spec, min_n_school = min_n
    )
    rds_tmp <- file.path(render_dir, "aggregate_results.rds")
    saveRDS(results$aggregate, rds_tmp)
    file.copy(rds_tmp, file.path(output_dir, "aggregate_results.rds"), overwrite = TRUE)
    if (render) .render_report("05_Aggregate_Analysis_Report.qmd", rds_tmp, output_dir,
                                render_dir, pkg_path, cfg$spec, min_n)
  }

  message("[", state_code, "] Complete. Outputs: ", output_dir)
  invisible(results)
}

# =============================================================================
# Run
# =============================================================================
run_state(STATE, analyses = ANALYSES, render = RENDER, min_n = MIN_N)
