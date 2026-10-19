# Run the score integrity sub-analysis and return the results sub-list

Orchestrates
[`compute_score_integrity()`](https://ndadey.github.io/ReportR/reference/compute_score_integrity.md),
[`plot_score_integrity()`](https://ndadey.github.io/ReportR/reference/plot_score_integrity.md),
and
[`summarize_score_integrity()`](https://ndadey.github.io/ReportR/reference/summarize_score_integrity.md)
into the standardized sub-list consumed by
[`run_anomaly_analysis()`](https://ndadey.github.io/ReportR/reference/run_anomaly_analysis.md).

## Usage

``` r
run_score_integrity(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum group size for suppression. Default `10`.

## Value

A named list with: `$table`, `$invalid_records`, `$mismatch_records`,
`$plot`, `$text`.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
sub <- run_score_integrity(demo_data, assessment_spec)
sub$text$narrative
} # }
```
