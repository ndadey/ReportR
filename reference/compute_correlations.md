# Compute cohort-style adjacent-grade score correlations

For each SUBJECT and adjacent year-pair (T, T+1), matches students whose
scale score at grade G in year T is correlated with their scale score at
grade G+1 in year T+1. Each row represents one SUBJECT x grade-pair x
year-pair combination.

## Usage

``` r
compute_correlations(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum number of matched pairs required to report a
  correlation. Default `10`.

## Value

A data frame with one row per SUBJECT x grade-pair x year-pair
containing `YEAR_1`, `YEAR_2`, `SUBJECT`, `GRADE_1`, `GRADE_2`, `n`,
`correlation`, `low_flag`, and `drop_flag`. Returns an empty data frame
(with a message) if `STUDENT_ID` is absent.

## Details

Flags are added for unusually low correlations (\< 0.5) and for drops of
more than 0.15 from the prior year-pair within the same SUBJECT x
grade-pair.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
tbl <- compute_correlations(demo_data, assessment_spec)
} # }
```
