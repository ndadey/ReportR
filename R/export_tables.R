# ==============================================================================
# export_tables_to_excel()
#
# Recursively extracts all data frames from a ReportR results list and writes
# each one as a separate tab in a single Excel workbook.
# ==============================================================================


# Short forms of long slot names, so sheet names fit Excel's 31-character limit.
.seg_abbr <- c(
  # general trend
  achievement_transitions          = "ach_trans",
  summary_table                    = "summ_tbl",
  long_table                       = "long_tbl",
  comparisons                      = "comp",
  # anomaly
  score_integrity                  = "score_integ",
  distributions                    = "distrib",
  invalid_records                  = "invalid_recs",
  mismatch_records                 = "mismatch_recs",
  # enrollment and cohort
  transitions                      = "trans",
  score_enrollment_trends          = "score_enrl_trends",
  by_school_subgroup               = "by_school_sg",
  crosssectional                   = "cross_sect",
  mobility_by_achiev               = "mob_by_achiev",
  enrollment_diag                  = "enroll_diag",
  # aggregate
  performance_trends               = "perf_trends",
  school_level_summary             = "school_lvl_summ",
  district_level_summary           = "dist_lvl_summ",
  improvement_classification       = "impr_class",
  year_to_year_correlations        = "yr_yr_corr",
  rank_order_data                  = "rank_order",
  regression_slopes                = "reg_slopes",
  population_shifts                = "pop_shifts",
  school_demographic_composition   = "school_demo_comp",
  district_demographic_composition = "dist_demo_comp",
  demographic_trend_slopes         = "demo_trend_slopes",
  composition_proficiency_join     = "comp_prof_join",
  disaggregated                    = "disagg",
  residual_model_results           = "resid_model",
  positive_outlier_schools         = "pos_outlier_schl",
  negative_outlier_schools         = "neg_outlier_schl",
  outlier_stability                = "outlier_stab",
  funnel_plot_data                 = "funnel_data",
  school_subgroup_proficiency      = "school_sg_prof",
  district_subgroup_proficiency    = "dist_sg_prof",
  within_school_gaps               = "within_schl_gaps",
  subgroup_vs_state_comparison     = "sg_vs_state",
  gap_trend_classification         = "gap_trend_class",
  correlation_flag_summary         = "corr_flag_summ"
)

# Non-empty data frames in a results list, named by path ("loss_hoss.table").
.collect_tables <- function(x, prefix = "") {
  if (inherits(x, c("gg", "ggplot", "patchwork"))) return(list())
  if (is.data.frame(x)) {
    if (!nzchar(prefix) || nrow(x) == 0L) return(list())
    return(stats::setNames(list(x), prefix))
  }
  if (!is.list(x) || is.null(names(x))) return(list())
  out <- list()
  for (nm in names(x)) {
    if (!nzchar(nm) || is.null(x[[nm]])) next
    child_prefix <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
    out <- c(out, .collect_tables(x[[nm]], child_prefix))
  }
  out
}

# Trim the longest segment, not the start, so every path level stays visible.
.shorten_sheet_name <- function(path, max_len = 31L) {
  segs <- strsplit(path, ".", fixed = TRUE)[[1]]
  hit  <- segs %in% names(.seg_abbr)
  segs[hit] <- .seg_abbr[segs[hit]]
  segs <- gsub("[^A-Za-z0-9_]", "_", segs)

  name_len <- function(s) sum(nchar(s)) + length(s) - 1L
  while (name_len(segs) > max_len && any(nchar(segs) > 1L)) {
    i <- which.max(nchar(segs))
    segs[i] <- substr(segs[i], 1L,
                      max(1L, nchar(segs[i]) - (name_len(segs) - max_len)))
  }
  substr(paste(segs, collapse = "."), 1L, max_len)
}

# Excel sheet names must be unique ignoring case.
.dedupe_sheet_names <- function(sheet_names, max_len = 31L) {
  seen <- character(0)
  for (i in seq_along(sheet_names)) {
    base <- sheet_names[i]
    k    <- 1L
    while (tolower(sheet_names[i]) %in% seen) {
      k      <- k + 1L
      suffix <- paste0("_", k)
      sheet_names[i] <- paste0(substr(base, 1L, max_len - nchar(suffix)), suffix)
    }
    seen <- c(seen, tolower(sheet_names[i]))
  }
  sheet_names
}


#' Export all tables from a ReportR results list to Excel
#'
#' Recursively walks a results list (e.g. `anomaly_results` from
#' [run_anomaly_analysis()]) and writes every non-empty data frame to its own
#' worksheet in a single Excel workbook. Plots are skipped.
#'
#' Sheet names follow each table's place in the list, with `.` between levels
#' (e.g. `loss_hoss.table`, `outliers.table`). Long slot names are abbreviated
#' (`achievement_transitions` becomes `ach_trans`, `performance_trends` becomes
#' `perf_trends`) so names fit Excel's 31-character limit; any name that is
#' still too long has its longest part trimmed, keeping every level of the path.
#'
#' @param results A named list returned by a ReportR `run_*_analysis()`
#'   function (e.g. [run_anomaly_analysis()]).
#' @param file Character. Path to the output `.xlsx` file.
#' @param overwrite Logical. Whether to overwrite an existing file. Default
#'   `TRUE`.
#'
#' @return Invisibly returns the path to the written file.
#' @export
#' @examples
#' \dontrun{
#' anomaly_results <- run_anomaly_analysis(student_results_long, assessment_spec)
#' export_tables_to_excel(anomaly_results, "output/anomaly_tables.xlsx")
#' }
export_tables_to_excel <- function(results, file, overwrite = TRUE) {
  stopifnot(is.list(results), is.character(file), length(file) == 1L)

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "The 'openxlsx' package is required. Install it with:\n",
      "  install.packages('openxlsx')"
    )
  }

  tables <- .collect_tables(results)

  if (length(tables) == 0) {
    message("No data frames found in results list.")
    return(invisible(file))
  }

  sheet_names <- .dedupe_sheet_names(
    vapply(names(tables), .shorten_sheet_name, character(1), USE.NAMES = FALSE)
  )

  wb <- openxlsx::createWorkbook()

  for (i in seq_along(tables)) {
    openxlsx::addWorksheet(wb, sheetName = sheet_names[[i]])
    openxlsx::writeDataTable(
      wb,
      sheet      = sheet_names[[i]],
      x          = tables[[i]],
      tableStyle = "TableStyleMedium2"
    )
    openxlsx::setColWidths(wb, sheet = sheet_names[[i]], cols = seq_len(ncol(tables[[i]])),
                           widths = "auto")
  }

  openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)
  message("Wrote ", length(tables), " tables to: ", file)
  invisible(file)
}
