# Report-ready wide means/SD table for a single subject

A thin wrapper around
[`compute_score_summary()`](https://ndadey.github.io/ReportR/reference/compute_score_summary.md)
that filters to one subject and returns the `$ScaleScore_Stats_Report`
element directly. Named to match the aspirational function name used in
`01_General_Trend_Analysis.qmd` so the template works without
modification once this function exists.

## Usage

``` r
mean_sd_table_wide(
  student_results_long,
  assessment_spec,
  subject,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  min_n = 10
)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- subject:

  Character scalar. Subject code to include (e.g. `"ELA"`).

- probs:

  Numeric vector of percentiles. Default
  `c(0.05, 0.25, 0.50, 0.75, 0.95)`.

- min_n:

  Integer. Minimum group size for suppression. Default `10`.

## Value

A data frame: one row per grade, year-grouped columns `<YEAR>_Mean`,
`<YEAR>_SD`, `<YEAR>_N`.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
mean_sd_table_wide(demo_data, assessment_spec, subject = "READING")
} # }
```
