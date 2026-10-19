# Plot distribution shape diagnostics

Returns a named list of three plots summarising the distribution shape
screening results.

## Usage

``` r
plot_distributions(distribution_table, student_results_long, assessment_spec)
```

## Arguments

- distribution_table:

  Data frame returned by
  [`compute_distributions()`](https://ndadey.github.io/ReportR/reference/compute_distributions.md).

- student_results_long:

  A long-format student results data frame (used to draw kernel density
  plots for flagged cells).

- assessment_spec:

  Named list describing the assessment program (used for subject labels
  and LOSS/HOSS boundaries).

## Value

A named list with:

- `$flag_summary` — bar chart of flagged cell counts by subject and flag
  type; `NULL` if no cells are flagged

- `$shape_heatmap` — heatmap of `bimodality_coefficient` by grade ×
  year, faceted by subject

- `$density_flagged` — named list of kernel density plots for flagged
  cells

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_distributions(demo_data, assessment_spec)
plots <- plot_distributions(tbl, demo_data, assessment_spec)
plots$shape_heatmap
} # }
```
