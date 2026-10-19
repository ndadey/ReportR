# Compute achievement level transition rates across adjacent grades within cohort

Compute achievement level transition rates across adjacent grades within
cohort

## Usage

``` r
compute_cohort_transitions(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame. Must contain `STUDENT_ID`.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Default `10`.

## Value

A data frame: `SUBJECT`, `COHORT_YEAR`, `FROM_GRADE`, `TO_GRADE`,
`FROM_LEVEL`, `TO_LEVEL`, `N`, `PCT_OF_FROM`. Returns 0 rows when no
persistent student IDs are available.
