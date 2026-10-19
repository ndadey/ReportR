# Plot alluvial (Sankey) flow diagrams for achievement level transitions

Requires the `ggalluvial` package. If not installed, returns `NULL` with
a message.

## Usage

``` r
plot_transition_alluvial(transition_data, assessment_spec)
```

## Arguments

- transition_data:

  Named list returned by
  [`compute_transitions()`](https://ndadey.github.io/ReportR/reference/compute_transitions.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by `"SUBJECT_Grade_G"`, or `NULL` if
`ggalluvial` is unavailable.
