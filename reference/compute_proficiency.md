# Compute percent proficient by year, subject, and grade

Determines the proficiency threshold from
`assessment_spec$achievement_levels` using
[`benchmark_index()`](https://ndadey.github.io/ReportR/reference/benchmark_index.md)
and counts students at or above that level.

## Usage

``` r
compute_proficiency(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Must
  contain `YEAR`, `SUBJECT`, `GRADE`, and `ACHIEVEMENT_LEVEL`.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Groups with fewer than `min_n` students have their statistics
  set to `NA`. Default `10`.

## Value

A data frame with one row per `YEAR × SUBJECT × GRADE`: `n`,
`n_proficient`, `pct_proficient` (NA when `n < min_n`).

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
compute_proficiency(demo_data, assessment_spec)
} # }
```
