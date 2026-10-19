# Individual histograms for each grade flagged for HOSS/LOSS pileups

Produces one histogram per flagged subject x grade combination. Each
histogram shows all years overlaid as separate density curves and
includes vertical lines for the HOSS/LOSS boundary, the `point_adjust`
threshold, and every cut score defined in `assessment_spec$scale_scores`
for that subject/grade. Each plot uses its own x-axis range. Subjects
and grades with no flags are omitted.

## Usage

``` r
plot_loss_hoss_distributions(
  student_results_long,
  assessment_spec,
  loss_hoss_table,
  distribution_table = NULL,
  point_adjust = 5,
  x_margin = 10
)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract).

- assessment_spec:

  Named list describing the assessment program.

- loss_hoss_table:

  Data frame returned by
  [`compute_loss_hoss_table()`](https://ndadey.github.io/ReportR/reference/compute_loss_hoss_table.md).

- distribution_table:

  Optional data frame returned by
  [`compute_distributions()`](https://ndadey.github.io/ReportR/reference/compute_distributions.md).
  When provided, per-year skewness and Sarle's bimodality coefficient
  are added to the in-plot annotation. Default `NULL`.

- point_adjust:

  Integer. Points from boundary used as the threshold line. Should match
  the value used in
  [`compute_loss_hoss_table()`](https://ndadey.github.io/ReportR/reference/compute_loss_hoss_table.md).
  Default `5`.

- x_margin:

  Numeric. Extra scale-score points added outside the LOSS and HOSS
  boundaries when setting the x-axis limits. Default `10`.

## Value

A flat named list of ggplot objects, one per subject/grade combination
(e.g. `"ELA_Grade3"`). Each plot shows overlaid density curves for all
years with LOSS and HOSS boundary lines, cut score lines, and a per-year
statistics annotation.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
tbl   <- compute_loss_hoss_table(demo_data, assessment_spec)
plots <- plot_loss_hoss_distributions(demo_data, assessment_spec, tbl)
plots[["READING_Grade3"]]
} # }
```
