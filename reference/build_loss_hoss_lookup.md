# Build a LOSS/HOSS lookup table from an assessment spec

Iterates over every subject (and grade, for span-tested subjects)
defined in `assessment_spec$subjects` and returns a data frame of LOSS
and HOSS values suitable for joining to student-level data.

## Usage

``` r
build_loss_hoss_lookup(assessment_spec, cuts = FALSE)
```

## Arguments

- assessment_spec:

  Named list describing the assessment program.

- cuts:

  Logical. When `TRUE`, append a `CUTS` list-column holding each row's
  cut-score vector. Default `FALSE`.

## Value

A data frame with columns `SUBJECT` (character), `GRADE` (integer, `NA`
for EOC subjects), `LOSS` (numeric), `HOSS` (numeric), and — when
`cuts = TRUE` — `CUTS` (list). One row per subject/grade combination
that has a matching entry in `assessment_spec$scale_scores`.

Note that EOC rows carry `GRADE = NA`. Do not join this table to student
data on `SUBJECT` + `GRADE` directly — states commonly report a real
tested grade on EOC records, which would miss. Use
[`join_score_bounds()`](https://ndadey.github.io/ReportR/reference/join_score_bounds.md)
instead.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
build_loss_hoss_lookup(assessment_spec)
} # }
```
