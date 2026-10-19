# Classify students as Entering, Continuing, or Exiting between adjacent grades

Classify students as Entering, Continuing, or Exiting between adjacent
grades

## Usage

``` r
compute_cohort_mobility(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Default `10`.

## Value

A data frame: `SUBJECT`, `COHORT_YEAR`, `FROM_GRADE`, `TO_GRADE`,
`YEAR`, `MOBILITY_STATUS`, `N`, `PCT`. Returns 0 rows when no persistent
student IDs are present.
