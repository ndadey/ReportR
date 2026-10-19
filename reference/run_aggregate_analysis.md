# Run all Section 5 aggregate (school and district) analyses

Orchestrates the five sub-analyses that can run from
`student_results_long` alone. Sub-analyses that require
`school_results_long` (accountability grade distributions, intervention
analysis, outlier characterization by co-op or locale) are set to `NULL`
with explanatory comments.

## Usage

``` r
run_aggregate_analysis(
  student_results_long,
  assessment_spec,
  school_results_long = NULL,
  report_years = NULL,
  min_n_school = 10,
  min_n_subgroup = 10,
  improvement_threshold = 3,
  outlier_sd_threshold = 2,
  outlier_n_funnel = 20,
  correlation_flag_threshold = 0.85
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Must
  contain school and district identifier columns.

- assessment_spec:

  Named list describing the assessment program.

- school_results_long:

  Optional school-level data frame containing accountability grades,
  indicator scores, HQIM_FLAG, LITERACY_COACH_FLAG, CO_OP, LOCALE, etc.
  Default `NULL`. Currently unused — pass it when the data becomes
  available to enable sub-analyses 5.2 and 5.7.

- report_years:

  Integer vector of years to include. `NULL` uses all years in the data.
  Default `NULL`.

- min_n_school:

  Integer. Minimum students per school × year × subject × grade cell.
  Default `10`.

- min_n_subgroup:

  Integer. Minimum subgroup size for disaggregated analyses. Default
  `10`.

- improvement_threshold:

  Numeric. Percentage-point change to classify a school as improving or
  declining. Default `3`.

- outlier_sd_threshold:

  Numeric. SD threshold for residual-based outlier flagging. Default
  `2`.

- outlier_n_funnel:

  Integer. Minimum n for funnel plot inclusion. Default `20`.

- correlation_flag_threshold:

  Numeric. Spearman r threshold for flagging unstable year-to-year
  correlations. Default `0.85`.

## Value

A named list `aggregate_results` with sub-lists:

- `$performance_trends` —
  [`compute_performance_trends()`](https://ndadey.github.io/ReportR/reference/compute_performance_trends.md)

- `$accountability` — `NULL` (requires school_results_long)

- `$population_shifts` —
  [`compute_population_shifts()`](https://ndadey.github.io/ReportR/reference/compute_population_shifts.md)

- `$outliers` —
  [`compute_outlier_schools()`](https://ndadey.github.io/ReportR/reference/compute_outlier_schools.md)

- `$disaggregated` —
  [`compute_school_disaggregated()`](https://ndadey.github.io/ReportR/reference/compute_school_disaggregated.md)

- `$stability` —
  [`compute_stability_analysis()`](https://ndadey.github.io/ReportR/reference/compute_stability_analysis.md)

- `$intervention` — `NULL` (requires school_results_long)

- `$params` — run parameters

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
aggregate_results <- run_aggregate_analysis(
  student_results_long = demo_data,
  assessment_spec      = assessment_spec
)
aggregate_results$performance_trends$improvement_classification
aggregate_results$outliers$positive_outlier_schools
} # }
```
