# Summarize year-to-year outlier findings

Returns a narrative and summary table describing the number and nature
of flagged year-to-year outlier cells.

## Usage

``` r
summarize_outliers(outlier_table)
```

## Arguments

- outlier_table:

  Data frame returned by
  [`compute_outliers()`](https://ndadey.github.io/ReportR/reference/compute_outliers.md).

## Value

A named list with `$table` (the full outlier table), `$by_subject`
(count of flagged cells by subject and severity), and `$narrative`
(character vector of 1–3 sentences).

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_outliers(demo_data, assessment_spec)
summarize_outliers(tbl)
} # }
```
