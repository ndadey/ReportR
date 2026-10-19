# Plot average transition heatmaps by subject

Plot average transition heatmaps by subject

## Usage

``` r
plot_transition_heatmaps(transition_data, assessment_spec)
```

## Arguments

- transition_data:

  Named list returned by
  [`compute_transitions()`](https://ndadey.github.io/ReportR/reference/compute_transitions.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects (one per subject), each faceted by grade.
