# Plot year-to-year outliers

Returns a named list of two Cleveland-style connected dot plots showing
year-to-year changes in mean scale score and percent proficient by
grade, faceted by subject. Outlier cells are highlighted with a distinct
point shape and size.

## Usage

``` r
plot_outliers(outlier_table, assessment_spec)
```

## Arguments

- outlier_table:

  Data frame returned by
  [`compute_outliers()`](https://ndadey.github.io/ReportR/reference/compute_outliers.md).

- assessment_spec:

  Named list describing the assessment program (used for subject
  labels).

## Value

A named list with elements `$score` and `$proficiency`, each a `ggplot`
object.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_outliers(demo_data, assessment_spec)
plots <- plot_outliers(tbl, assessment_spec)
plots$score
plots$proficiency
} # }
```
