# Compute rates of improving, stable, and declining trajectories across grades

Compute rates of improving, stable, and declining trajectories across
grades

## Usage

``` r
compute_cohort_rate_of_change(
  student_results_long,
  assessment_spec,
  min_n = 10
)
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
`TRAJECTORY`, `N`, `PCT`. Returns 0 rows when no persistent IDs exist.
