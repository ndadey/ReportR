# Compute school-level mean scale scores and year-over-year changes

Aggregates student scale scores to the school × year × subject level and
computes year-over-year changes in mean score. Intended to be joined
with enrollment change data to examine the relationship between
enrollment shifts and score trends.

## Usage

``` r
compute_school_score_summary(data, assessment_spec, min_n = 10)
```

## Arguments

- data:

  A long-format student results data frame (see data contract). Must
  contain `SCALE_SCORE`, `YEAR`, and `SUBJECT`, plus a school ID column
  (`SCHOOL_NUMBER` or `SCHOOL_ID`).

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. School × year × subject cells with fewer than `min_n`
  students have their `mean_score` set to `NA`. Default `10`.

## Value

A data frame with columns: school ID, `YEAR`, `SUBJECT`, `N`,
`mean_score`, `delta_mean` (year-over-year change in mean score),
`year_pair` (e.g. `"2023-2024"`), `suppressed`. Returns an empty data
frame if no school ID column is found.
