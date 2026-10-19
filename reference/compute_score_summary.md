# Compute scale score distributional summaries by year, subject, and grade

Produces long- and wide-format summary tables of student scale score
distributions, a counts table, and a report-ready wide table with grades
as rows and year-grouped column blocks (Mean / SD / N). Designed as the
primary data source for all Line 1 score trend reporting.

## Usage

``` r
compute_score_summary(
  student_results_long,
  assessment_spec,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  by_grade = TRUE,
  min_n = 10
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Must
  contain `YEAR`, `SUBJECT`, `SCALE_SCORE`, and `GRADE` when
  `by_grade = TRUE`.

- assessment_spec:

  Named list describing the assessment program. Used to validate that
  subjects in the data match the spec.

- probs:

  Numeric vector of percentiles in \[0, 1\]. Default
  `c(0.05, 0.25, 0.50, 0.75, 0.95)`.

- by_grade:

  Logical. If `TRUE` (default) summaries are computed per
  `YEAR × GRADE × SUBJECT`. If `FALSE`, `GRADE` is ignored and summaries
  are per `YEAR × SUBJECT`.

- min_n:

  Integer. Groups with fewer than `min_n` non-missing scores have their
  statistics set to `NA`. Default `10`.

## Value

A named list with four elements:

- `$ScaleScore_Stats_Long` — long-format data frame: grouping columns
  plus `metric` and `value`. Metrics: `mean`, `sd`, `p05`, `p25`, `p50`,
  `p75`, `p95` (or as determined by `probs`).

- `$ScaleScore_Stats_Wide` — wide-format data frame: one row per group,
  one column per metric.

- `$ScaleScore_Stats_Report` — report-ready wide table: one row per
  grade, with year-grouped column blocks `<YEAR>_Mean`, `<YEAR>_SD`,
  `<YEAR>_N`.

- `$ScaleScore_Counts` — data frame with `n_total`, `n_nonmissing`,
  `n_missing`, and `pct_missing` per group.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
out <- compute_score_summary(demo_data, assessment_spec)
head(out$ScaleScore_Stats_Wide)
} # }
```
