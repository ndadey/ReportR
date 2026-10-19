# Summarize score integrity findings

Produces a short narrative and a table filtered to rows with issues.

## Usage

``` r
summarize_score_integrity(score_integrity_result)
```

## Arguments

- score_integrity_result:

  Named list returned by
  [`compute_score_integrity()`](https://ndadey.github.io/ReportR/reference/compute_score_integrity.md).

## Value

A named list with:

- `$narrative` – character vector of 2-3 narrative sentences.

- `$table` – summary data frame filtered to rows where
  `has_any_issue == TRUE`. If no issues exist, returns the full summary
  with an attribute `note` explaining that all checks passed.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- compute_score_integrity(demo_data, assessment_spec)
txt    <- summarize_score_integrity(result)
cat(txt$narrative, sep = "\n\n")
} # }
```
