# Run the Line 1 score trends sub-analysis

Computes distributional score summaries, generates trend plots, and
produces narrative text. This is the primary score trends sub-analysis
for Line 1 (General Trend Analysis).

## Usage

``` r
run_score_trends(
  student_results_long,
  assessment_spec,
  min_n = 10,
  flat_threshold = 1
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract).

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum group size for small-n suppression. Default `10`.

- flat_threshold:

  Numeric. Changes smaller than this (in scale score points) are
  labelled "relatively flat" in the narrative. Default `1.0`.

## Value

A named list:

- `$table` — `ScaleScore_Stats_Report` wide table

- `$plot` — list of ggplots: `$mean_trend`, `$mean_sd`, `$median_iqr`,
  `$grade_profile`

- `$text` — list: `$narrative`, `$by_subject`, `$table`

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
out <- run_score_trends(demo_data, assessment_spec)
out$plot$mean_trend
} # }
```
