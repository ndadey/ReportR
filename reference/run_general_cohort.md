# Run General Cohort Analysis

Standalone orchestrator for Line 1.3 cohort composition and performance
analysis. Parallel to
[`run_general_trend_analysis()`](https://ndadey.github.io/ReportR/reference/run_general_trend_analysis.md)
(Line 1) and
[`run_anomaly_analysis()`](https://ndadey.github.io/ReportR/reference/run_anomaly_analysis.md)
(Line 4). Runs
[`run_cohort_analysis()`](https://ndadey.github.io/ReportR/reference/run_cohort_analysis.md),
saves results to RDS, and optionally renders the Quarto report.

## Usage

``` r
run_general_cohort(
  student_results_long,
  assessment_spec,
  output_dir,
  report_years = NULL,
  min_n = 10,
  render_report = TRUE,
  open_report = FALSE
)
```

## Arguments

- student_results_long:

  A standardized long-format student results data frame (see data
  contract in `CLAUDE.md §6`). Must have already been processed by
  [`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md).

- assessment_spec:

  Named list describing the assessment program. Set
  `attr(assessment_spec, "spec_file")` to the absolute path of the spec
  file so it can be sourced inside the QMD.

- output_dir:

  Character. Directory where the RDS and rendered report will be copied
  after rendering.

- report_years:

  Integer vector. Years to include in the analysis. If `NULL` (default),
  all years present in `student_results_long` are used. Always filter
  *after*
  [`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md).

- min_n:

  Integer. Minimum cell size for small-n suppression. Default `10`.

- render_report:

  Logical. If `TRUE` (default), renders the Quarto report to both docx
  and html.

- open_report:

  Logical. If `TRUE`, opens the rendered html in a browser after
  rendering. Default `FALSE`.

## Value

The cohort results list, returned invisibly.

## Details

Run the General Cohort Analysis (Line 1.3)

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
attr(assessment_spec, "spec_file") <-
  system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR")
cohort_results <- run_general_cohort(
  student_results_long = demo_data,
  assessment_spec      = assessment_spec,
  output_dir           = tempdir(),
  render_report        = FALSE
)
} # }
```
