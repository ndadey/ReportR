# Compute school- or district-level enrollment counts and subgroup proportions

Aggregates enrollment to the school or district level, computing total
enrollment and year-over-year changes. Optionally computes subgroup
proportions for each entity.

## Usage

``` r
compute_school_district_enrollment(
  data,
  level = "school",
  assessment_spec,
  subgroup_vars = NULL,
  min_n = 10
)
```

## Arguments

- data:

  A long-format student results data frame (see data contract).

- level:

  Character scalar: `"school"` or `"district"`.

- assessment_spec:

  Named list describing the assessment program.

- subgroup_vars:

  Character vector of demographic columns to compute proportions for.
  Defaults to `names(assessment_spec$demographics)`.

- min_n:

  Integer. Subgroup cells with `N < min_n` have `pct_sg` set to `NA`.
  Default `10`.

## Value

A named list:

- `$totals` — data frame: entity ID/name columns, `YEAR`, `N`,
  `N_change`, `N_pct_change`

- `$by_subgroup` — long-format data frame: entity columns, `YEAR`,
  `subgroup_var`, `subgroup_val`, `N_sg`, `pct_sg`, `suppressed` Returns
  an empty list if entity ID columns are not present in the data.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
# sgpData_LONG has no SCHOOL_ID — this returns an empty list for that dataset
compute_school_district_enrollment(demo_data, level = "school", assessment_spec)
} # }
```
