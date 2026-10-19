# Compute HOSS/LOSS pileup rates by year, subject, and grade

Joins student-level data to the LOSS/HOSS boundaries from
`assessment_spec`, flags students within `point_adjust` points of either
boundary, aggregates to year × subject × grade, applies severity flags,
and suppresses cells with fewer than `min_n` students.

## Usage

``` r
compute_loss_hoss_table(
  student_results_long,
  assessment_spec,
  point_adjust = 5,
  min_n = 10,
  mild_thresh = 5,
  moderate_thresh = 10,
  severe_thresh = 15
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Must
  contain `YEAR`, `SUBJECT`, `GRADE`, and `SCALE_SCORE`.

- assessment_spec:

  Named list describing the assessment program (see assessment_spec).
  Must contain `$subjects` and `$scale_scores`.

- point_adjust:

  Integer. Number of scale-score points within the boundary that counts
  as "near" LOSS or HOSS. Default `5`.

- min_n:

  Integer. Minimum group size; cells with fewer students have
  `pct_near_loss` and `pct_near_hoss` set to `NA`. Default `10`.

- mild_thresh:

  Numeric. Percentage threshold for a Mild flag. Default `5`.

- moderate_thresh:

  Numeric. Percentage threshold for a Moderate flag. Default `10`.

- severe_thresh:

  Numeric. Percentage threshold for a Severe flag. Default `15`.

## Value

A data frame with one row per year × subject × grade combination,
columns: `YEAR`, `SUBJECT`, `GRADE`, `LOSS`, `HOSS`, `n`, `n_near_loss`,
`n_near_hoss`, `pct_near_loss`, `pct_near_hoss`, `small_n`, `hoss_flag`,
`loss_flag`, `pct_near_hoss_change`, `pct_near_loss_change`.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
tbl <- compute_loss_hoss_table(demo_data, assessment_spec)
} # }
```
