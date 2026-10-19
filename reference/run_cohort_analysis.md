# Run all Line 1.3 cohort analysis sub-analyses

Calls all `compute_*()` and `plot_*()` functions and returns a single
named results list. This is the primary function for cohort analysis;
[`run_general_cohort()`](https://ndadey.github.io/ReportR/reference/run_general_cohort.md)
wraps it with RDS saving and report rendering.

## Usage

``` r
run_cohort_analysis(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract).

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum cell size for suppression. Default `10`.

## Value

A named list containing all tables, plots, and narrative text.
