# Plot demographic composition of cohorts

Plot demographic composition of cohorts

## Usage

``` r
plot_cohort_demographic_composition(demo_df, assessment_spec)
```

## Arguments

- demo_df:

  Data frame from
  [`compute_cohort_demographics()`](https://ndadey.github.io/ReportR/reference/compute_cohort_demographics.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT.
