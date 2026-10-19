# Compute distribution shape diagnostics

Calculates summary statistics for scale score distributions within each
YEAR x SUBJECT x GRADE cell to screen for unusual shapes (flat, bimodal,
or highly skewed). All statistics are suppressed to `NA` where
`n < min_n`.

## Usage

``` r
compute_distributions(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum group size for suppression. Default `10`.

## Value

A data frame with one row per YEAR x SUBJECT x GRADE containing shape
statistics (`mean`, `sd`, `skewness`, `kurtosis`,
`bimodality_coefficient`, `p05`–`p95`) plus `distribution_flag` and
`flag_reason`.

## Details

Each cell gets at most one flag, checked in this order:

- **Bimodal**: Sarle's bimodality coefficient above 0.555.

- **Skewed**: absolute skewness above 1.

- **Flat**: excess kurtosis below -1, meaning scores are spread evenly
  with no clear peak. A uniform distribution has excess kurtosis of
  -1.2; a normal distribution has 0.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
tbl <- compute_distributions(demo_data, assessment_spec)
} # }
```
