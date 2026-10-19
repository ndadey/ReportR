# Identify schools performing above or below demographic expectation

Fits a regression of school-level proficiency on demographic composition
variables (FRL_pct, EL_pct, IEP_pct, minority_pct where available)
within each year × subject, computes residuals, and flags schools
exceeding `outlier_sd_threshold` standard deviations. Also produces
funnel plot control limits and tracks outlier stability across years.

## Usage

``` r
compute_outlier_schools(
  performance_data,
  spec,
  school_data = NULL,
  outlier_sd_threshold = 2,
  outlier_n_funnel = 20
)
```

## Arguments

- performance_data:

  Named list returned by
  [`compute_performance_trends()`](https://ndadey.github.io/ReportR/reference/compute_performance_trends.md).

- spec:

  Named list describing the assessment program.

- school_data:

  `school_results_long`, or `NULL`. When `NULL`,
  `$outlier_characteristics` is set to `NULL`.

- outlier_sd_threshold:

  Numeric. SD threshold for flagging. Default `2`.

- outlier_n_funnel:

  Integer. Minimum n for funnel plot inclusion. Default `20`.

## Value

A named list:

- `$residual_model_results` — school × year × subject: observed,
  predicted, residual, sd_residual, outlier_flag, n_tested

- `$positive_outlier_schools` — subset with outlier_flag == "high"

- `$negative_outlier_schools` — subset with outlier_flag == "low"

- `$outlier_stability` — school × subject: n_years_flagged, direction

- `$funnel_plot_data` — school × year × subject: proficiency, n_tested,
  state_pct, upper_95, lower_95, upper_99, lower_99

- `$outlier_characteristics` — `NULL` (requires school_results_long)
