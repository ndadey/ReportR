# Plot scatterplots comparing transition rates to score/proficiency changes

Plot scatterplots comparing transition rates to score/proficiency
changes

## Usage

``` r
plot_transition_comparisons(comparisons, assessment_spec)
```

## Arguments

- comparisons:

  Named list returned by
  [`compute_transition_comparisons()`](https://ndadey.github.io/ReportR/reference/compute_transition_comparisons.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list with `$scatter_mean` and `$scatter_prof` ggplot objects.
