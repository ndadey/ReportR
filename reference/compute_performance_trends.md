# Compute school- and district-level performance trends

Aggregates `student_results_long` to the school and district level and
computes summary statistics, year-over-year changes, improvement
classifications, school-level regression slopes, rank orders,
year-to-year correlations, and intraclass correlations (ICCs). ICC
computation requires the `lme4` package (in Suggests); if not installed,
`$icc_tables` is an empty data frame.

## Usage

``` r
compute_performance_trends(
  data,
  spec,
  min_n_school = 10,
  improvement_threshold = 3,
  correlation_flag_threshold = 0.85
)
```

## Arguments

- data:

  A long-format student results data frame (see data contract).

- spec:

  Named list describing the assessment program (assessment_spec).

- min_n_school:

  Integer. Schools with fewer students than this in a year × subject ×
  grade cell are suppressed. Default `10`.

- improvement_threshold:

  Numeric. Percentage-point change threshold for classifying a school as
  improving or declining. Default `3`.

- correlation_flag_threshold:

  Numeric. Year-to-year Spearman r below which a year pair is flagged as
  unstable. Default `0.85`.

## Value

A named list:

- `$school_level_summary` — school × year × subject × grade with N,
  mean_score, sd_score, p05–p95, n_proficient, pct_proficient,
  yoy_change, demographic proportion columns, and yoy_change_score

- `$district_level_summary` — district × year × subject × grade

- `$improvement_classification` — school × year × subject:
  classification

- `$regression_slopes` — school × subject × grade: slope, se, r_squared

- `$year_to_year_correlations` — year_pair × subject × grade: pearson_r,
  spearman_r, n_schools, flagged

- `$rank_order_data` — school × year × subject × grade: rank, pct_rank

- `$icc_tables` — grade × subject × year: icc, var_between, var_within
