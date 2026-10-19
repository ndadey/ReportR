# Run the Line 1 proficiency sub-analysis

Computes percent proficient by year/subject/grade, generates trend and
heatmap plots, and produces narrative text.

## Usage

``` r
run_proficiency(
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

  Numeric. Changes smaller than this (in percentage points) are labelled
  "relatively flat" in the narrative. Default `1.0`.

## Value

A named list:

- `$table` — proficiency summary data frame

- `$plot` — list of ggplots: `$trend`, `$heatmap`

- `$text` — list: `$narrative`, `$by_subject`, `$table`

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
out <- run_proficiency(demo_data, assessment_spec)
out$plot$trend
} # }
```
