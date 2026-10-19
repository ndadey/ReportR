# Summarize distribution shape findings

Returns a short narrative, by-subject counts, and the full distribution
table. The narrative counts flagged cells by type and subject and names
the subject-grades flagged in most years; individual cells are left to
the table.

## Usage

``` r
summarize_distributions(distribution_table, assessment_spec = NULL)
```

## Arguments

- distribution_table:

  Data frame returned by
  [`compute_distributions()`](https://ndadey.github.io/ReportR/reference/compute_distributions.md).

- assessment_spec:

  Optional named list describing the assessment program, used for
  subject labels. Default `NULL` (subject codes are used).

## Value

A named list with `$table` (the full distribution table), `$by_subject`
(flagged cell counts by SUBJECT and flag type), and `$narrative` (a
short markdown paragraph).

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_distributions(demo_data, assessment_spec)
summarize_distributions(tbl, assessment_spec)
} # }
```
