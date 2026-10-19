# Plot Sankey / alluvial transition diagrams

Requires the `ggalluvial` package (in Suggests). Returns `NULL` if not
installed or if `transition_df` has 0 rows.

## Usage

``` r
plot_sankey_transitions(transition_df, assessment_spec)
```

## Arguments

- transition_df:

  Data frame from
  [`compute_cohort_transitions()`](https://ndadey.github.io/ReportR/reference/compute_cohort_transitions.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT, or `NULL`.
