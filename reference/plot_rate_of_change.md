# Plot rates of improving, stable, and declining trajectories

Plot rates of improving, stable, and declining trajectories

## Usage

``` r
plot_rate_of_change(roc_df, assessment_spec)
```

## Arguments

- roc_df:

  Data frame from
  [`compute_cohort_rate_of_change()`](https://ndadey.github.io/ReportR/reference/compute_cohort_rate_of_change.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT, or `NULL`.
