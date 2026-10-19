# Summarize cohort correlation findings

Returns a narrative, by-subject summary, the full long-format
correlation table, and a wide-format display table suitable for
rendering with flextable.

## Usage

``` r
summarize_correlations(correlation_table)
```

## Arguments

- correlation_table:

  Data frame returned by
  [`compute_correlations()`](https://ndadey.github.io/ReportR/reference/compute_correlations.md).

## Value

A named list with:

- `$table` — full long-format correlation table

- `$wide_table` — wide-format data frame (Subject + Grade Pair rows,
  year-pair column groups with Correlation and N sub-columns)

- `$by_subject` — median correlation and flagged cell counts by subject

- `$narrative` — character vector of 1–3 sentences

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_correlations(demo_data, assessment_spec)
summarize_correlations(tbl)
} # }
```
