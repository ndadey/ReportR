# Compare cohort trajectories to cross-sectional grade snapshots

Compare cohort trajectories to cross-sectional grade snapshots

## Usage

``` r
compute_cohort_vs_crosssectional(
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

A named list:

- `$cohort` — data frame from
  [`compute_cohort_summary()`](https://ndadey.github.io/ReportR/reference/compute_cohort_summary.md)

- `$crosssectional` — data frame: `SUBJECT`, `GRADE`, `YEAR`, `N`,
  `MEAN_SCORE`, `PCT_PROFICIENT`
