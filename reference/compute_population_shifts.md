# Compute school- and district-level demographic composition and shifts

Tracks how the demographic composition of the tested population changes
over time at the school and district level, and examines whether
composition shifts correlate with proficiency changes.

## Usage

``` r
compute_population_shifts(
  data,
  performance_data,
  spec,
  school_data = NULL,
  min_n_school = 10
)
```

## Arguments

- data:

  A long-format student results data frame.

- performance_data:

  Named list returned by
  [`compute_performance_trends()`](https://ndadey.github.io/ReportR/reference/compute_performance_trends.md).

- spec:

  Named list describing the assessment program.

- school_data:

  `school_results_long` data frame, or `NULL`. When `NULL`, co-op and
  locale breakdowns are set to `NULL`.

- min_n_school:

  Integer. Minimum school size for inclusion. Default `10`.

## Value

A named list:

- `$school_demographic_composition` — long format: school × year × group
  × pct

- `$district_demographic_composition` — long format: district × year ×
  group × pct

- `$demographic_trend_slopes` — school × group: slope over years

- `$composition_proficiency_join` — school × year: delta_composition
  columns and delta_pct_proficient for each subject

- `$by_coop` — `NULL` (requires school_results_long)

- `$by_locale` — `NULL` (requires school_results_long)
