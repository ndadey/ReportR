# Plot proficiency trends and heatmap

Creates two ggplot objects from the output of
[`compute_proficiency()`](https://ndadey.github.io/ReportR/reference/compute_proficiency.md):
a trend line plot and a heatmap, both faceted by subject.

## Usage

``` r
plot_proficiency(proficiency_table, assessment_spec)
```

## Arguments

- proficiency_table:

  Data frame returned by
  [`compute_proficiency()`](https://ndadey.github.io/ReportR/reference/compute_proficiency.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

A named list:

- `$trend` — line plot: YEAR on x, pct_proficient on y, grade as color,
  faceted by subject

- `$heatmap` — heatmap: YEAR on x, grade on y, fill = pct_proficient,
  faceted by subject

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
prof <- compute_proficiency(demo_data, assessment_spec)
plots <- plot_proficiency(prof, assessment_spec)
plots$trend
} # }
```
