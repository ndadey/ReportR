# Compute subgroup proportions within an enrollment grouping

For each variable in `subgroup_vars`, computes the proportion of
students in each category within the specified `by` grouping. Returns a
long-format data frame with one row per `by`-group × subgroup variable ×
subgroup value.

## Usage

``` r
compute_enrollment_proportions(
  data,
  by,
  subgroup_vars = NULL,
  assessment_spec,
  denominator = "total_by_year",
  min_n = 10
)
```

## Arguments

- data:

  A long-format student results data frame (see data contract).

- by:

  Character vector of grouping column names (e.g. `c("YEAR", "GRADE")`).

- subgroup_vars:

  Character vector of demographic column names to compute proportions
  for. Defaults to `names(assessment_spec$demographics)`.

- assessment_spec:

  Named list describing the assessment program.

- denominator:

  Character scalar controlling what counts as the total enrollment for
  proportion calculation. One of:

  - `"total_by_year"` — all students in that year (default)

  - `"total_overall"` — all students across all years

  - `"within_grade"` — students in that year × grade

  - `"within_school"` — students in that year × school

  - `"within_district"` — students in that year × district

- min_n:

  Integer. Cells with `N < min_n` have their `pct` set to `NA`. Default
  `10`.

## Value

A long-format data frame with columns: `by` grouping columns,
`subgroup_var` (the demographic variable name), `subgroup_val` (the
category), `N` (count), `total_N` (denominator), `pct` (proportion ×
100), `suppressed`.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
compute_enrollment_proportions(demo_data, by = "YEAR", assessment_spec)
} # }
```
