# Track retention of the original anchor-grade cohort across subsequent grades

Track retention of the original anchor-grade cohort across subsequent
grades

## Usage

``` r
compute_cohort_persistence(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Default `10`.

## Value

A data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE`, `YEAR`, `N_ORIGINAL`,
`N_ENTERING`, `N_TOTAL`, `PCT_RETAINED`. Returns 0 rows when no
persistent student IDs are present.
