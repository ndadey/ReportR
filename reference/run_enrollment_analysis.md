# Run the Line 1.2 enrollment trend analysis

Orchestrates all enrollment sub-analyses — counts, proportions, subgroup
transitions, entry/exit performance, and school/district aggregations —
and returns a single named results list suitable for passing to the
enrollment report templates.

## Usage

``` r
run_enrollment_analysis(
  student_results_long,
  assessment_spec,
  report_years = NULL,
  subgroup_vars = NULL,
  min_n = 10
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Should
  already be processed by
  [`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md).

- assessment_spec:

  Named list describing the assessment program (see assessment_spec).

- report_years:

  Integer vector of years to include. `NULL` (default) uses all years
  present in the data.

- subgroup_vars:

  Character vector of demographic column names to use as subgroups.
  `NULL` (default) uses all variables listed in
  `assessment_spec$demographics`.

- min_n:

  Integer. Minimum group size for small-n suppression across all
  sub-analyses. Default `10`.

## Value

A named list with sub-lists:

- `$counts` — outputs of
  [`compute_enrollment_counts()`](https://ndadey.github.io/ReportR/reference/compute_enrollment_counts.md)
  at various grains

- `$proportions` — outputs of
  [`compute_enrollment_proportions()`](https://ndadey.github.io/ReportR/reference/compute_enrollment_proportions.md)

- `$transitions` — named by subgroup variable; each element is the
  output of
  [`compute_subgroup_transitions()`](https://ndadey.github.io/ReportR/reference/compute_subgroup_transitions.md)

- `$entry_exit` — entering, exiting, and continuing student performance

- `$aggregate` — school- and district-level summaries

- `$text` — output of
  [`summarize_enrollment()`](https://ndadey.github.io/ReportR/reference/summarize_enrollment.md)

- `$params` — run parameters

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
res <- run_enrollment_analysis(demo_data, assessment_spec)
res$counts$by_year
res$proportions$by_subgroup
} # }
```
