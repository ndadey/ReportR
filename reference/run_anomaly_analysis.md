# Run all Line 4 score quality analyses

Orchestrates the five score quality sub-analyses (HOSS/LOSS pileups,
score integrity, year-to-year outliers, year-to-year correlations, and
distribution diagnostics) and returns a single named results list. All
subsequent report chunks should read from the returned object rather
than re-running analysis.

## Usage

``` r
run_anomaly_analysis(
  student_results_long,
  assessment_spec,
  report_years = NULL,
  min_n = 10,
  point_adjust = 5,
  mild_thresh = 5,
  moderate_thresh = 10,
  severe_thresh = 15
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Should
  already be processed by
  [`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md).

- assessment_spec:

  Named list describing the assessment program (see assessment_spec).

- report_years:

  Integer vector. Years to include. If `NULL` (default) all years
  present in the data are used.

- min_n:

  Integer. Minimum group size for small-n suppression. Default `10`.

- point_adjust:

  Integer. Scale-score points from LOSS/HOSS boundary that defines
  "near". Default `5`.

- mild_thresh:

  Numeric. Percent threshold for Mild HOSS/LOSS flag. Default `5`.

- moderate_thresh:

  Numeric. Percent threshold for Moderate flag. Default `10`.

- severe_thresh:

  Numeric. Percent threshold for Severe flag. Default `15`.

## Value

A named list `anomaly_results` with sub-lists:

- `$loss_hoss` — output of
  [`run_loss_hoss()`](https://ndadey.github.io/ReportR/reference/run_loss_hoss.md)

- `$score_integrity` — output of
  [`run_score_integrity()`](https://ndadey.github.io/ReportR/reference/run_score_integrity.md)

- `$outliers` — output of
  [`run_outliers()`](https://ndadey.github.io/ReportR/reference/run_outliers.md)
  (stub)

- `$correlations` — output of
  [`run_correlations()`](https://ndadey.github.io/ReportR/reference/run_correlations.md)
  (stub)

- `$distributions` — output of
  [`run_distributions()`](https://ndadey.github.io/ReportR/reference/run_distributions.md)
  (stub)

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
results <- run_anomaly_analysis(
  student_results_long = demo_data,
  assessment_spec      = assessment_spec,
  report_years         = 2016:2019,
  min_n                = 10
)
} # }
```
