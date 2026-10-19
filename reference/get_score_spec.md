# Retrieve scale score specification for a subject/grade combination

Looks up the `loss`, `hoss`, and `cuts` entry from
`assessment_spec$scale_scores`. Span-tested subjects are keyed by grade
(`scale_scores$SUBJECT$GRADE`); EOC subjects have no grade nesting
(`scale_scores$SUBJECT`) and are resolved on `subject` alone, so a
`grade` supplied for an EOC subject is ignored rather than causing a
lookup miss.

## Usage

``` r
get_score_spec(assessment_spec, subject, grade = NULL)
```

## Arguments

- assessment_spec:

  Named list describing the assessment program.

- subject:

  Character scalar. Subject code matching a key in
  `assessment_spec$subjects`.

- grade:

  Integer scalar or `NULL`. Grade level for span-tested subjects;
  ignored for EOC subjects.

## Value

A named list with elements `loss`, `hoss`, and `cuts`, or `NULL` if the
combination is not found in the spec.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
get_score_spec(assessment_spec, "ELA", 3)
get_score_spec(assessment_spec, "ENG_II")
get_score_spec(assessment_spec, "ALG_I", 9)  # EOC — grade ignored
} # }
```
