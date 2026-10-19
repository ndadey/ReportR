# Compute year-over-year rank-order and correlation stability

Derives stability metrics from the already-computed performance trends.
Requires no additional raw student data.

## Usage

``` r
compute_stability_analysis(
  performance_data,
  spec,
  correlation_flag_threshold = 0.85
)
```

## Arguments

- performance_data:

  Named list returned by
  [`compute_performance_trends()`](https://ndadey.github.io/ReportR/reference/compute_performance_trends.md).

- spec:

  Named list describing the assessment program.

- correlation_flag_threshold:

  Numeric. Spearman r threshold for flagging unstable year pairs.
  Default `0.85`.

## Value

A named list:

- `$year_to_year_correlations` — from performance_data (passed through)

- `$rank_order_data` — from performance_data (passed through)

- `$correlation_flag_summary` — year pairs where flagged == TRUE

- `$icc_trends` — year × subject: icc averaged across grades
