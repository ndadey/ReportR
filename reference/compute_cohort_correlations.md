# Compute Pearson correlations of scale scores across adjacent grades within cohort

Compute Pearson correlations of scale scores across adjacent grades
within cohort

## Usage

``` r
compute_cohort_correlations(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Default `10`.

## Value

A data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE_X`, `GRADE_Y`, `YEAR_X`,
`YEAR_Y`, `R`, `N`, `P_VALUE`. Returns 0 rows when no persistent IDs
exist.
