# Plot cohort-style score correlations

Returns a named list of two plots visualising year-to-year cohort
Spearman correlations by grade-pair within each subject.

## Usage

``` r
plot_correlations(correlation_table, assessment_spec)
```

## Arguments

- correlation_table:

  Data frame returned by
  [`compute_correlations()`](https://ndadey.github.io/ReportR/reference/compute_correlations.md).

- assessment_spec:

  Named list describing the assessment program (used for subject
  labels).

## Value

A named list with elements `$line` (line plot over time, one line per
grade-pair) and `$heatmap` (correlation heatmap by grade-pair and
year-pair), each a `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_correlations(demo_data, assessment_spec)
plots <- plot_correlations(tbl, assessment_spec)
plots$line
} # }
```
