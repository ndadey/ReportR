# Run the HOSS/LOSS pileup analysis and return the results sub-list

Orchestrates
[`compute_loss_hoss_table()`](https://ndadey.github.io/ReportR/reference/compute_loss_hoss_table.md),
all four plot functions, and
[`summarize_loss_hoss_findings()`](https://ndadey.github.io/ReportR/reference/summarize_loss_hoss_findings.md)
into the standardized sub-list consumed by
[`run_anomaly_analysis()`](https://ndadey.github.io/ReportR/reference/run_anomaly_analysis.md).

## Usage

``` r
run_loss_hoss(
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

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- point_adjust:

  Integer. Points from boundary to flag as "near". Default `5`.

- min_n:

  Integer. Minimum group size for suppression. Default `10`.

- mild_thresh:

  Numeric. Percent threshold for Mild flag. Default `5`.

- moderate_thresh:

  Numeric. Percent threshold for Moderate flag. Default `10`.

- severe_thresh:

  Numeric. Percent threshold for Severe flag. Default `15`.

## Value

A named list with elements: `$table`, `$plot_hoss_cleveland`,
`$plot_loss_cleveland`, `$plot_hoss_heat`, `$plot_loss_heat`,
`$plot_hoss_line`, `$plot_loss_line`, `$plot_distributions`, `$text`.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
sub <- run_loss_hoss(demo_data, assessment_spec)
} # }
```
