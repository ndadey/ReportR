# Plot mobility status distribution across grades

Plot mobility status distribution across grades

## Usage

``` r
plot_mobility_distribution(mobility_df, assessment_spec)
```

## Arguments

- mobility_df:

  Data frame from
  [`compute_cohort_mobility()`](https://ndadey.github.io/ReportR/reference/compute_cohort_mobility.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT, or `NULL`.
