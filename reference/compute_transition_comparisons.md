# Compare transition patterns to mean score and proficiency trends

Joins transition summary statistics to already-computed score and
proficiency trends, computes correlations, and flags divergent cases
where transition direction contradicts scale score or proficiency
direction.

## Usage

``` r
compute_transition_comparisons(
  transition_data,
  score_trends,
  proficiency,
  assessment_spec
)
```

## Arguments

- transition_data:

  Named list returned by
  [`compute_transitions()`](https://ndadey.github.io/ReportR/reference/compute_transitions.md).

- score_trends:

  Named list returned by
  [`run_score_trends()`](https://ndadey.github.io/ReportR/reference/run_score_trends.md).

- proficiency:

  Named list returned by
  [`run_proficiency()`](https://ndadey.github.io/ReportR/reference/run_proficiency.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

A named list:

- `$correlations` — Pearson correlations between net_upward_rate and
  delta_mean / delta_pct_proficient by SUBJECT

- `$divergences` — flagged divergent cases with annotations

- `$joined` — full joined table for downstream plotting
