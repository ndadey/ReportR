# ==============================================================================
# export_tables_to_excel()
#
# Recursively extracts all data frames from a ReportR results list and writes
# each one as a separate tab in a single Excel workbook.
# ==============================================================================


#' Export all tables from a ReportR results list to Excel
#'
#' Recursively walks a results list (e.g. `anomaly_results` from
#' [run_anomaly_analysis()]) and writes every data frame found to a separate
#' worksheet in a single Excel workbook. Sheet names are derived from the
#' nested list path (e.g. `loss_hoss__table`, `outliers__table`).
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

  # Recursively collect all data frames from a nested list.
  # Returns a named list of data frames, names reflect the list path.
  collect_tables <- function(x, prefix = "") {
    out <- list()
    if (is.data.frame(x)) {
      out[[prefix]] <- x
    } else if (is.list(x) && !is.data.frame(x)) {
      for (nm in names(x)) {
        child_prefix <- if (nchar(prefix) == 0) nm else paste0(prefix, "__", nm)
        out <- c(out, collect_tables(x[[nm]], child_prefix))
      }
    }
    out
  }

  tables <- collect_tables(results)

  if (length(tables) == 0) {
    message("No data frames found in results list.")
    return(invisible(file))
  }

  # Excel sheet names: max 31 characters, no special characters
  clean_sheet_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_]", "_", x)
    if (nchar(x) > 31) x <- substr(x, nchar(x) - 30, nchar(x))
    x
  }

  sheet_names <- vapply(names(tables), clean_sheet_name, character(1))

  # Deduplicate sheet names if truncation caused collisions
  sheet_names <- make.unique(sheet_names, sep = "_")

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
