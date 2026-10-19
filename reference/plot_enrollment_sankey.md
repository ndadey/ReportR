# Plot enrollment composition as an alluvial (Sankey) diagram

Visualizes how the composition of the tested population shifts across
years for one subgroup variable. Requires the `ggalluvial` package.

## Usage

``` r
plot_enrollment_sankey(
  transition_table,
  subgroup_var,
  years = NULL,
  assessment_spec,
  palette = NULL
)
```

## Arguments

- transition_table:

  A data frame — the `$table` element returned by
  [`compute_subgroup_transitions()`](https://ndadey.github.io/ReportR/reference/compute_subgroup_transitions.md).
  Must contain columns `YEAR`, `subgroup_val`, `N`, and `suppressed`.

- subgroup_var:

  Character scalar. The demographic variable being plotted (used for
  axis labels and the plot title).

- years:

  Integer vector of years to include. `NULL` includes all years present
  in `transition_table`. Default `NULL`.

- assessment_spec:

  Named list describing the assessment program.

- palette:

  Named character vector of colors, keyed by `subgroup_val`. `NULL` uses
  `ggplot2::scale_fill_brewer(palette = "Set2")`. Default `NULL`.

## Value

A ggplot2 object (ggalluvial layers). Returns an informative blank plot
if all data is suppressed or if `ggalluvial` is not installed.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
trans <- compute_subgroup_transitions(demo_data, "RACE_ETHNICITY", assessment_spec)
plot_enrollment_sankey(trans$table, "RACE_ETHNICITY", assessment_spec = assessment_spec)
} # }
```
