# Summarize proficiency trends in plain text

Computes first-to-last year changes in percent proficient by subject and
produces a markdown narrative.

## Usage

``` r
summarize_proficiency_findings(
  proficiency_table,
  assessment_spec,
  flat_threshold = 1
)
```

## Arguments

- proficiency_table:

  Data frame returned by
  [`compute_proficiency()`](https://ndadey.github.io/ReportR/reference/compute_proficiency.md).

- assessment_spec:

  Named list describing the assessment program.

- flat_threshold:

  Numeric. Changes smaller than this (in percentage points) are labelled
  "relatively flat" in the narrative. Default `1.0`.

## Value

A named list:

- `$table` — the input `proficiency_table`

- `$by_subject` — named list of per-subject narrative strings

- `$narrative` — markdown bullet list

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
prof <- compute_proficiency(demo_data, assessment_spec)
summarize_proficiency_findings(prof, assessment_spec)$narrative
} # }
```
