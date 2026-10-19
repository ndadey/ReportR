# Generate plain-language narrative summary of cohort analysis findings

Generate plain-language narrative summary of cohort analysis findings

## Usage

``` r
summarize_cohort_analysis(
  cohort_summary,
  transition_df,
  roc_df,
  corr_df,
  assessment_spec
)
```

## Arguments

- cohort_summary:

  Data frame from
  [`compute_cohort_summary()`](https://ndadey.github.io/ReportR/reference/compute_cohort_summary.md).

- transition_df:

  Data frame from
  [`compute_cohort_transitions()`](https://ndadey.github.io/ReportR/reference/compute_cohort_transitions.md).

- roc_df:

  Data frame from
  [`compute_cohort_rate_of_change()`](https://ndadey.github.io/ReportR/reference/compute_cohort_rate_of_change.md).

- corr_df:

  Data frame from
  [`compute_cohort_correlations()`](https://ndadey.github.io/ReportR/reference/compute_cohort_correlations.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

A named list: `$narrative`, `$by_subject`, `$table`.
