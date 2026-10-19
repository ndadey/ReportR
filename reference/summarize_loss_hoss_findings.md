# Summarize HOSS/LOSS findings into narrative text and tables

Produces a subject × grade summary of the worst-observed flag level
across all available years, a subject-level narrative string, and an
overall narrative paragraph ready for inline Quarto rendering.

## Usage

``` r
summarize_loss_hoss_findings(loss_hoss_table)
```

## Arguments

- loss_hoss_table:

  Data frame returned by
  [`compute_loss_hoss_table()`](https://ndadey.github.io/ReportR/reference/compute_loss_hoss_table.md).

## Value

A named list with:

- `$table` — data frame of max flag per subject/grade

- `$by_subject` — data frame of subject-level narrative summaries

- `$narrative` — character vector of paragraph-length narrative strings

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
txt <- summarize_loss_hoss_findings(tbl)
cat(txt$narrative, sep = "\n\n")
} # }
```
