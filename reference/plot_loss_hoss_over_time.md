# Line plot of near-LOSS or near-HOSS percentages over time

Plots the percentage of students near each boundary across years, with
one line per grade, faceted by subject.

## Usage

``` r
plot_loss_hoss_over_time(loss_hoss_table, metric = c("hoss", "loss"))
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
plot_loss_hoss_over_time(tbl, metric = "hoss")
} # }
```
