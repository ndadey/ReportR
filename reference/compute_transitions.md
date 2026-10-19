# Compute year-to-year achievement level transition tables

Matches students cohort-style: Grade G in year T is matched to Grade G+1
in year T+1 for span-tested subjects, and on STUDENT_ID + SUBJECT only
for EOC subjects. Produces long-format transition counts/proportions, a
summary table of total and net transition rates, and average matrices
pooled across year-pairs.

## Usage

``` r
compute_transitions(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Must
  contain `STUDENT_ID`, `YEAR`, `SUBJECT`, `GRADE`, and
  `ACHIEVEMENT_LEVEL`.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Cells with fewer students have `prop` set to `NA`. Default
  `10`.

## Value

A named list:

- `$long_table` — one row per SUBJECT × GRADE × year-pair × from_level ×
  to_level

- `$summary_table` — one row per SUBJECT × GRADE × year-pair with
  `n_total`, `total_transition_rate`, `net_upward_rate`

- `$avg_matrices` — average transition proportions pooled across
  year-pairs
