# Cleveland dot plot of near-LOSS or near-HOSS percentages

Displays the percentage of students near the LOSS or HOSS boundary for
each grade (y-axis), with years differentiated by color. A horizontal
line spans the min–max range across years for each grade to show
year-to-year movement. Faceted by subject with subjects arranged as
columns so the plot stays page-width and fits comfortably on a portrait
page.

## Usage

``` r
plot_loss_hoss_cleveland(
  loss_hoss_table,
  metric = c("hoss", "loss"),
  point_size = 3
)
```

## Arguments

- loss_hoss_table:

  Data frame returned by
  [`compute_loss_hoss_table()`](https://ndadey.github.io/ReportR/reference/compute_loss_hoss_table.md).

- metric:

  Character. `"hoss"` (default) or `"loss"`.

- point_size:

  Numeric. Size of plotted points. Default `3`.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
plot_loss_hoss_cleveland(tbl, metric = "hoss")
} # }
```
