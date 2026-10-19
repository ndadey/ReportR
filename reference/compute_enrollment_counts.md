# Count enrolled students by arbitrary grouping dimensions

Deduplicates records to unique student-year combinations (or
student-year-subject when `SUBJECT` is in `by`) before counting, so
students tested in multiple subjects in the same year are not
double-counted in cross-sectional tallies.

## Usage

``` r
compute_enrollment_counts(data, by = NULL, assessment_spec, min_n = 10)
```

## Arguments

- data:

  A long-format student results data frame (see data contract). Must
  contain `STUDENT_ID` and `YEAR`.

- by:

  Character vector of grouping column names, or `NULL` for an overall
  total. Supported values: `"YEAR"`, `"GRADE"`, `"SUBJECT"`, demographic
  column names (e.g. `"RACE_ETHNICITY"`, `"FRL"`), `"SCHOOL_ID"`,
  `"DISTRICT_ID"`, and their `_NAME` equivalents.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Year-over-year change columns are suppressed (set to `NA`)
  for rows where `N < min_n`. Default `10`.

## Value

A data frame with the columns in `by` plus:

- `N` — count of unique student-year (or student-year-subject) records

- `N_change` — year-over-year change in `N` (only when `YEAR` is in
  `by`)

- `N_pct_change` — percent change (only when `YEAR` is in `by`)

- `suppressed` — logical; `TRUE` when `N < min_n`

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
compute_enrollment_counts(demo_data, by = c("YEAR", "GRADE"), assessment_spec)
} # }
```
