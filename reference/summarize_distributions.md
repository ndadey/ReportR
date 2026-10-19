# Summarize distribution shape findings

Returns a narrative, by-subject summary, and the full distribution table
describing any unusual score distribution shapes detected.

## Usage

``` r
summarize_distributions(distribution_table)
```

## Arguments

- distribution_table:

  Data frame returned by
  [`compute_distributions()`](https://ndadey.github.io/ReportR/reference/compute_distributions.md).

## Value

A named list with `$table` (the full distribution table), `$by_subject`
(flagged cell counts by SUBJECT and flag type), and `$narrative`
(character vector of 1–3 sentences).

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_distributions(demo_data, assessment_spec)
summarize_distributions(tbl)
} # }
```
