# Plot score distributions by cohort using pre-computed quantiles

Plot score distributions by cohort using pre-computed quantiles

## Usage

``` r
plot_cohort_score_distributions(cohort_summary, assessment_spec)
```

## Arguments

- cohort_summary:

  Data frame from
  [`compute_cohort_summary()`](https://ndadey.github.io/ReportR/reference/compute_cohort_summary.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT.
