# Compute year-to-year outliers in assessment results

Identifies cells (YEAR x SUBJECT x GRADE) where the year-to-year change
in mean scale score, percent proficient, or binary demographic
composition falls more than 2 standard deviations from the mean change
across all cells.

## Usage

``` r
compute_outliers(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum group size for suppression. Default `10`.

## Value

A data frame with one row per YEAR x SUBJECT x GRADE containing base
metrics (`mean_score`, `pct_proficient`, `pct_<demo>`), year-to-year
delta columns (`delta_*`), and outlier flag columns (`outlier_flag`,
`outlier_metric`, `outlier_direction`).

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
tbl <- compute_outliers(demo_data, assessment_spec)
} # }
```
