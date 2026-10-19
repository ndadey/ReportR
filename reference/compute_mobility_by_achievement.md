# Cross mobility status with achievement level to identify selection patterns

Cross mobility status with achievement level to identify selection
patterns

## Usage

``` r
compute_mobility_by_achievement(
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
`MOBILITY_STATUS`, `ACHIEVEMENT_LEVEL`, `N`, `PCT_WITHIN_MOBILITY`.
Returns 0 rows when no persistent student IDs are present.
