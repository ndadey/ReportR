# Plot mean scale scores by cohort across grades

Plot mean scale scores by cohort across grades

## Usage

``` r
plot_cohort_mean_scores(cohort_summary, assessment_spec, crosssectional = NULL)
```

## Arguments

- cohort_summary:

  Data frame from
  [`compute_cohort_summary()`](https://ndadey.github.io/ReportR/reference/compute_cohort_summary.md).

- assessment_spec:

  Named list describing the assessment program.

- crosssectional:

  Optional cross-sectional data frame from
  [`compute_cohort_vs_crosssectional()`](https://ndadey.github.io/ReportR/reference/compute_cohort_vs_crosssectional.md)
  to overlay as a dashed reference line.

## Value

Named list of ggplot objects keyed by SUBJECT.
