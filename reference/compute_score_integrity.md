# Compute score integrity checks: invalid scores and level mismatches

Runs two checks against `assessment_spec`:

## Usage

``` r
compute_score_integrity(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Must
  contain `STUDENT_ID`, `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, and
  `ACHIEVEMENT_LEVEL`.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Groups with fewer than `min_n` non-missing scores have
  `pct_invalid` and `pct_mismatch` set to `NA`. Default `10`.

## Value

A named list with three elements:

- `$summary` – one row per `YEAR` x `SUBJECT` x `GRADE` with columns
  `n_total`, `n_below_loss`, `n_above_hoss`, `n_invalid`, `pct_invalid`,
  `n_mismatch`, `pct_mismatch`, `n_unknown_level`, `has_any_issue`.

- `$invalid_records` – individual rows with invalid scores; columns
  `STUDENT_ID`, `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, `LOSS`,
  `HOSS`, `direction`. Empty data frame if none found.

- `$mismatch_records` – individual rows with level mismatches (valid
  scores only); columns `STUDENT_ID`, `YEAR`, `SUBJECT`, `GRADE`,
  `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, `EXPECTED_LEVEL`. Empty data frame
  if none found.

## Details

**Check 1 – Invalid scores**: flags any `SCALE_SCORE` below `LOSS` or
above `HOSS` for its subject/grade combination.

**Check 2 – Level mismatches**: for scores that passed Check 1, derives
the expected achievement level from the `cuts` vector
(`sum(score > cuts) + 1` gives the level index) and compares it to the
recorded `ACHIEVEMENT_LEVEL`. Rows where the recorded level is not in
`assessment_spec$achievement_levels$labels` are excluded from the
mismatch count and reported separately in the narrative.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
result <- compute_score_integrity(demo_data, assessment_spec)
result$summary
} # }
```
