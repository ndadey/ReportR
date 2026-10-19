# Summarize score density in plain text

Returns a brief narrative describing what the score density plots show.

## Usage

``` r
summarize_score_density(student_results_long, assessment_spec)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

## Value

A named list with one element: `$narrative` (character string).

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
summarize_score_density(demo_data, assessment_spec)$narrative
} # }
```
