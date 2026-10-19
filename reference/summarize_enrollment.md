# Summarize enrollment trends in plain text

Generates a markdown narrative from the pre-computed enrollment results
list, describing overall enrollment size and year-over-year trends.

## Usage

``` r
summarize_enrollment(enrollment_results, assessment_spec)
```

## Arguments

- enrollment_results:

  Named list returned by
  [`run_enrollment_analysis()`](https://ndadey.github.io/ReportR/reference/run_enrollment_analysis.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

A named list:

- `$narrative` — character string (markdown)

- `$by_subject` — named list of per-subject strings (empty for
  enrollment)

- `$table` — by-year counts data frame, or empty data frame if
  unavailable

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
res <- run_enrollment_analysis(demo_data, assessment_spec)
cat(res$text$narrative)
} # }
```
