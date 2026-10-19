# Compute demographic composition of cohorts by grade

Compute demographic composition of cohorts by grade

## Usage

``` r
compute_cohort_demographics(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Cells below this size have `PCT` set to `NA`. Default `10`.

## Value

A long data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE`, `YEAR`,
`DEMOGRAPHIC`, `GROUP`, `N`, `PCT`.
