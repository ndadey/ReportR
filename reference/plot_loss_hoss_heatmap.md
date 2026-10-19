# Heatmap of near-LOSS or near-HOSS percentages

Displays a tile heatmap with year on the x-axis, grade on the y-axis,
and fill intensity proportional to the percentage of students near the
boundary. Faceted by subject.

## Usage

``` r
plot_loss_hoss_heatmap(loss_hoss_table, metric = c("hoss", "loss"))
```

## Arguments

- loss_hoss_table:

  Data frame returned by
  [`compute_loss_hoss_table()`](https://ndadey.github.io/ReportR/reference/compute_loss_hoss_table.md).

- metric:

  Character. `"hoss"` (default) or `"loss"`.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
plot_loss_hoss_heatmap(tbl, metric = "hoss")
} # }
```
