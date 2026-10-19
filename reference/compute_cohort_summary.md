# Compute cohort-level scale score and proficiency summaries

Follows each cohort (defined by `COHORT_YEAR`) through grades, computing
mean scores, standard deviations, proficiency rates, and per-level
counts.

## Usage

``` r
compute_cohort_summary(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Cells with fewer students have numeric columns set to `NA`.
  Default `10`.

## Value

A data frame: `SUBJECT`, `COHORT_YEAR`, `GRADE`, `YEAR`, `N`,
`MEAN_SCORE`, `SD_SCORE`, `MEDIAN_SCORE`, `Q1_SCORE`, `Q3_SCORE`,
`PCT_PROFICIENT`, plus one `N_<Level>` column per achievement level.
