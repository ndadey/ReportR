# Create trend plots from a score summary

Produces four ggplot objects visualising scale score trends over time:
mean trends, mean with SD bands, median with IQR bands, and grade-level
profiles. All plots are faceted by subject using labels from
`assessment_spec`.

## Usage

``` r
plot_score_trends(score_summary, assessment_spec)
```

## Arguments

- score_summary:

  Named list returned by
  [`compute_score_summary()`](https://ndadey.github.io/ReportR/reference/compute_score_summary.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

A named list with four ggplot elements:

- `$mean_trend` — mean per grade over time, faceted by subject

- `$mean_sd` — mean ± 1 SD bands, faceted by subject

- `$median_iqr` — median with IQR bands, faceted by subject

- `$grade_profile` — grade on x-axis, year as color, faceted by subject

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
ss <- compute_score_summary(demo_data, assessment_spec)
plots <- plot_score_trends(ss, assessment_spec)
plots$mean_trend
} # }
```
