# Format an enrollment data frame as a flextable

Applies the package's standard `flextable` styling to an enrollment
counts or proportions data frame. Suppressed cells are shown as `"—"`
and rows with year-over-year proportion changes greater than 5
percentage points are highlighted in yellow.

## Usage

``` r
plot_enrollment_table(
  counts_or_proportions,
  type = "counts",
  assessment_spec,
  shift_threshold = 5
)
```

## Arguments

- counts_or_proportions:

  A data frame — output of
  [`compute_enrollment_counts()`](https://ndadey.github.io/ReportR/reference/compute_enrollment_counts.md)
  or
  [`compute_enrollment_proportions()`](https://ndadey.github.io/ReportR/reference/compute_enrollment_proportions.md).

- type:

  Character scalar: `"counts"`, `"proportions"`, or `"combined"`.
  Controls which highlight logic is applied. Default `"counts"`.

- assessment_spec:

  Named list describing the assessment program.

- shift_threshold:

  Numeric. Absolute percentage-point change above which a row is
  highlighted. Applies only when `pct_change` is in the data. Default
  `5`.

## Value

A `flextable` object.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
counts <- compute_enrollment_counts(demo_data, by = "YEAR", assessment_spec)
plot_enrollment_table(counts, assessment_spec = assessment_spec)
} # }
```
