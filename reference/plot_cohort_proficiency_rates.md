# Plot proficiency rates by cohort across grades

Plot proficiency rates by cohort across grades

## Usage

``` r
plot_cohort_proficiency_rates(cohort_summary, assessment_spec)
```

## Arguments

- cohort_summary:

  Data frame from
  [`compute_cohort_summary()`](https://ndadey.github.io/ReportR/reference/compute_cohort_summary.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT.
