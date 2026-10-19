# Summarize score trends in plain text

Computes first-to-last year mean changes by subject and grade and
generates a narrative markdown summary.

## Usage

``` r
summarize_score_trends(score_summary, assessment_spec, flat_threshold = 1)
```

## Arguments

- score_summary:

  Named list returned by
  [`compute_score_summary()`](https://ndadey.github.io/ReportR/reference/compute_score_summary.md).

- assessment_spec:

  Named list describing the assessment program.

- flat_threshold:

  Numeric. Changes smaller than this (in scale score points) are
  labelled "relatively flat" in the narrative. Default `1.0`.

## Value

A named list:

- `$table` — `ScaleScore_Stats_Report` data frame

- `$by_subject` — named list of per-subject narrative strings

- `$narrative` — markdown bullet list summarising key changes

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
ss <- compute_score_summary(demo_data, assessment_spec)
summarize_score_trends(ss, assessment_spec)$narrative
} # }
```
