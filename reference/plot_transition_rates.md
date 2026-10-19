# Plot total and net transition rates across all grade/subject/year cells

Plot total and net transition rates across all grade/subject/year cells

## Usage

``` r
plot_transition_rates(transition_data, assessment_spec, flag_table = NULL)
```

## Arguments

- transition_data:

  Named list returned by
  [`compute_transitions()`](https://ndadey.github.io/ReportR/reference/compute_transitions.md).

- assessment_spec:

  Named list describing the assessment program.

- flag_table:

  Data frame of flagged cells (from
  [`flag_transitions()`](https://ndadey.github.io/ReportR/reference/flag_transitions.md)),
  or `NULL` to omit highlighting.

## Value

A ggplot object.
