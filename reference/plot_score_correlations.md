# Plot Pearson correlations of scale scores across adjacent grades

Plot Pearson correlations of scale scores across adjacent grades

## Usage

``` r
plot_score_correlations(corr_df, assessment_spec)
```

## Arguments

- corr_df:

  Data frame from
  [`compute_cohort_correlations()`](https://ndadey.github.io/ReportR/reference/compute_cohort_correlations.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT, or `NULL`.
