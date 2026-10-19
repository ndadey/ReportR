# Run the Line 1 achievement transitions sub-analysis

Computes transition matrices, flags normatively large transitions,
compares results to scale score and proficiency trends, and produces
plots and narrative text.

## Usage

``` r
run_achievement_transitions(
  student_results_long,
  assessment_spec,
  score_trends,
  proficiency,
  min_n = 10,
  flag_sd = 1.5
)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- score_trends:

  Named list returned by
  [`run_score_trends()`](https://ndadey.github.io/ReportR/reference/run_score_trends.md).

- proficiency:

  Named list returned by
  [`run_proficiency()`](https://ndadey.github.io/ReportR/reference/run_proficiency.md).

- min_n:

  Integer. Minimum cell size for suppression. Default `10`.

- flag_sd:

  Numeric. SD threshold for normative flagging. Default `1.5`.

## Value

A named list:

- `$matrices` — transition data and plots

- `$flags` — normative flag table, plot, and narrative

- `$comparisons` — correlations, divergence flags, and scatter plots

- `$text` — overall narrative
