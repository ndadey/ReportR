# Attach LOSS/HOSS (and optionally cut scores) to a data frame

The single supported way to bring scale-score bounds from an
`assessment_spec` onto data. Span-tested subjects are matched on
`SUBJECT` + `GRADE`; EOC subjects (those declared `grades = NULL`) are
matched on `SUBJECT` alone.

## Usage

``` r
join_score_bounds(data, assessment_spec, cuts = FALSE, warn_unmatched = TRUE)
```

## Arguments

- data:

  A data frame with `SUBJECT` and `GRADE` columns. Works on
  student-level data and on already-summarized tables alike.

- assessment_spec:

  Named list describing the assessment program.

- cuts:

  Logical. When `TRUE`, also attach a `CUTS` list-column. Default
  `FALSE`.

- warn_unmatched:

  Logical. When `TRUE` (default), raise a single warning naming each
  `SUBJECT`/`GRADE` cell present in `data` that has no entry in the
  spec, with its row count. Set `FALSE` in downstream callers so one run
  warns once.

## Value

`data` with `GRADE` coerced to integer and `LOSS`, `HOSS` (and `CUTS`,
when requested) attached. Cells with no spec entry get `NA`.

## Details

The EOC rule matters: states routinely report a real tested grade on EOC
records — Mississippi's Algebra I records span grades 7 through 12 —
while the spec declares EOC subjects without grades. Joining on
`SUBJECT` + `GRADE` silently misses every one of those rows and leaves
`LOSS`/`HOSS` as `NA`.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
dat <- join_score_bounds(student_results_long, assessment_spec)
} # }
```
