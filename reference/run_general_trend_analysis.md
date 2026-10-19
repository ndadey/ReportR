# Run all Line 1 general trend analyses

Orchestrates the three Line 1 sub-analyses (score trends, proficiency
trends, and score distributions) and returns a single named results
list. All subsequent report chunks should read from the returned object
rather than re-running analysis.

## Usage

``` r
run_general_trend_analysis(
  student_results_long,
  assessment_spec,
  report_years = NULL,
  min_n = 10,
  score_flat_threshold = 1,
  prof_flat_threshold = 1,
  flag_sd = 1.5
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Should
  already be processed by
  [`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md).

- assessment_spec:

  Named list describing the assessment program (see assessment_spec).

- report_years:

  Integer vector. Years to include. If `NULL` (default) all years
  present in the data are used.

- min_n:

  Integer. Minimum group size for small-n suppression. Default `10`.

- score_flat_threshold:

  Numeric. Scale score point changes smaller than this are labelled
  "relatively flat" in the score trends narrative. Default `1.0`.

- prof_flat_threshold:

  Numeric. Percentage point changes smaller than this are labelled
  "relatively flat" in the proficiency narrative. Default `1.0`.

- flag_sd:

  Numeric. SD threshold for normative transition flagging. Default
  `1.5`.

## Value

A named list `general_trend_results` with sub-lists:

- `$score_trends` — output of
  [`run_score_trends()`](https://ndadey.github.io/ReportR/reference/run_score_trends.md)

- `$proficiency` — output of
  [`run_proficiency()`](https://ndadey.github.io/ReportR/reference/run_proficiency.md)

- `$distributions` — output of
  [`run_score_density()`](https://ndadey.github.io/ReportR/reference/run_score_density.md)

- `$achievement_transitions` — output of
  [`run_achievement_transitions()`](https://ndadey.github.io/ReportR/reference/run_achievement_transitions.md)

- `$params` — list of run parameters

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
results <- run_general_trend_analysis(
  student_results_long = demo_data,
  assessment_spec      = assessment_spec
)
results$score_trends$plot$mean_trend
results$proficiency$plot$trend
results$distributions$plot$density[["READING"]]
} # }
```
