# Plot score integrity results

Produces two diagnostic plots from the output of
[`compute_score_integrity()`](https://ndadey.github.io/ReportR/reference/compute_score_integrity.md):
a bar chart of invalid score counts and a heatmap of mismatch rates.

## Usage

``` r
plot_score_integrity(score_integrity_result)
```

## Arguments

- score_integrity_result:

  Named list returned by
  [`compute_score_integrity()`](https://ndadey.github.io/ReportR/reference/compute_score_integrity.md).

## Value

A named list with:

- `$invalid` – bar chart of `n_invalid` by subject and grade, faceted by
  year. `NULL` (with message) if no invalid scores exist.

- `$mismatch` – heatmap of `pct_mismatch` with year on x-axis, grade on
  y-axis, faceted by subject. Grey cells = no mismatches. `NULL` (with
  message) if no mismatches exist.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- compute_score_integrity(demo_data, assessment_spec)
plots  <- plot_score_integrity(result)
plots$mismatch
} # }
```
