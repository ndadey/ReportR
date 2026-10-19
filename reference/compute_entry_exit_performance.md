# Compute performance of entering and exiting students

Identifies students who entered (present in year T but not T-1) or
exited (present in year T-1 but not T) the tested population, then
computes their mean scale score and percent proficient. Requires
`STUDENT_ID` to track students across years.

## Usage

``` r
compute_entry_exit_performance(
  data,
  direction = "entering",
  assessment_spec,
  by = c("YEAR", "SUBJECT"),
  min_n = 10
)
```

## Arguments

- data:

  A long-format student results data frame (see data contract). Must
  contain `STUDENT_ID`, `YEAR`, `SCALE_SCORE`, and (optionally)
  `ACHIEVEMENT_LEVEL`.

- direction:

  Character scalar: `"entering"`, `"exiting"`, or `"both"`. Default
  `"entering"`.

- assessment_spec:

  Named list describing the assessment program.

- by:

  Character vector of grouping columns for performance statistics.
  Default `c("YEAR", "SUBJECT")`.

- min_n:

  Integer. Cells with fewer than `min_n` students have their statistics
  set to `NA`. Default `10`.

## Value

A data frame with columns: `YEAR`, `group` (entering/exiting), grouping
columns from `by`, `N`, `mean_score`, `pct_proficient`, `suppressed`.
Returns an empty data frame if `STUDENT_ID` is absent or fewer than 2
years are present.

## Details

Presence is determined at the year level (a student is "present" in year
T if they have at least one record in the data for that year, regardless
of subject). For exiting students, performance statistics come from year
T-1 (their last year of testing), but the row is labelled with
`YEAR = T` (the transition year) so that entering and exiting rows align
on the same YEAR axis in output tables.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
compute_entry_exit_performance(demo_data, direction = "both", assessment_spec)
} # }
```
