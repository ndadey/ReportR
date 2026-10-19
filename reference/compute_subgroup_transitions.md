# Compute year-over-year subgroup composition shifts

Tracks how the proportion of students in each category of a single
demographic variable changes across consecutive testing years. When
`cohort = TRUE`, the analysis tracks cohorts (grade-aligned year
sequences) rather than raw cross-sectional counts.

## Usage

``` r
compute_subgroup_transitions(
  data,
  subgroup_var,
  assessment_spec,
  cohort = FALSE,
  min_n = 10
)
```

## Arguments

- data:

  A long-format student results data frame (see data contract).

- subgroup_var:

  Character scalar. Name of the demographic column to track (e.g.
  `"RACE_ETHNICITY"`, `"FRL"`).

- assessment_spec:

  Named list describing the assessment program.

- cohort:

  Logical. If `TRUE`, include `GRADE` in the grouping so year-over-year
  comparisons are within the same grade level (cohort tracking). Default
  `FALSE`.

- min_n:

  Integer. Cells with `N < min_n` have `pct` set to `NA`. Default `10`.

## Value

A named list:

- `$table` — data frame with `YEAR` (plus `GRADE` when `cohort = TRUE`),
  `subgroup_var`, `subgroup_val`, `N`, `total_N`, `pct`, `suppressed`

- `$summary` — data frame of year-over-year percentage-point changes per
  subgroup category: `subgroup_val`, `YEAR`, `prior_year`, `pct_change`

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
compute_subgroup_transitions(demo_data, "RACE_ETHNICITY", assessment_spec)$summary
} # }
```
