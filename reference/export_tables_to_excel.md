# Export all tables from a ReportR results list to Excel

Recursively walks a results list (e.g. `anomaly_results` from
[`run_anomaly_analysis()`](https://ndadey.github.io/ReportR/reference/run_anomaly_analysis.md))
and writes every non-empty data frame to its own worksheet in a single
Excel workbook. Plots are skipped.

## Usage

``` r
export_tables_to_excel(results, file, overwrite = TRUE)
```

## Arguments

- results:

  A named list returned by a ReportR `run_*_analysis()` function (e.g.
  [`run_anomaly_analysis()`](https://ndadey.github.io/ReportR/reference/run_anomaly_analysis.md)).

- file:

  Character. Path to the output `.xlsx` file.

- overwrite:

  Logical. Whether to overwrite an existing file. Default `TRUE`.

## Value

Invisibly returns the path to the written file.

## Details

Sheet names follow each table's place in the list, with `.` between
levels (e.g. `loss_hoss.table`, `outliers.table`). Long slot names are
abbreviated (`achievement_transitions` becomes `ach_trans`,
`performance_trends` becomes `perf_trends`) so names fit Excel's
31-character limit; any name that is still too long has its longest part
trimmed, keeping every level of the path.

## Examples

``` r
if (FALSE) { # \dontrun{
anomaly_results <- run_anomaly_analysis(student_results_long, assessment_spec)
export_tables_to_excel(anomaly_results, "output/anomaly_tables.xlsx")
} # }
```
