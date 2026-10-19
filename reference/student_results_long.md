# student_results_long: Expected data format for ReportR analysis functions

All ReportR analysis functions expect a long-format data frame produced
by
[`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md).
This page documents the required columns and their expected types.

## Format

A data frame with one row per student per year per subject, with the
following columns:

- STUDENT_ID:

  character. Unique student identifier. Required for correlation
  analysis
  ([`run_correlations()`](https://ndadey.github.io/ReportR/reference/run_correlations.md)).

- YEAR:

  integer. Administration year. `"YYYY_YYYY"` academic-year strings are
  automatically converted to the end year by
  [`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md).

- GRADE:

  integer. Tested grade. `NA` for EOC subjects.

- SUBJECT:

  character. Subject code matching keys in `assessment_spec$subjects`
  (e.g. `"ELA"`, `"MATHEMATICS"`).

- SCALE_SCORE:

  numeric. Student scale score.

- ACHIEVEMENT_LEVEL:

  character. Achievement level label matching
  `assessment_spec$achievement_levels$labels`.

- SCHOOL_ID:

  character. School identifier.

- SCHOOL_NAME:

  character. School name.

- DISTRICT_ID:

  character. District identifier.

- DISTRICT_NAME:

  character. District name.

- RACE_ETHNICITY:

  character. Race/ethnicity category.

- FRL:

  character. Free/reduced lunch status (binary demographic).

- EL:

  character. English Learner status (binary demographic).

- IEP:

  character. Individualized Education Program status (binary
  demographic).

- GENDER:

  character. Student gender.

## Details

Use
[`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md)
to convert raw assessment data to this format. The function handles
common variations in column naming (e.g. `CONTENT_AREA` -\> `SUBJECT`,
`SS` -\> `SCALE_SCORE`) and coerces `YEAR` and `GRADE` to integer.

**Important:** Always call
[`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md)
before filtering by year. Raw data from SGPdata uses `"YYYY_YYYY"` year
strings; filtering by integer year before standardizing returns 0 rows.

## Examples

``` r
if (FALSE) { # \dontrun{
library(SGPdata)
library(dplyr)

student_results_long <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results()
} # }
```
