# Run the Line 1 score distributions sub-analysis

Produces kernel density plots showing scale score distributional shifts
over time by subject and grade.

## Usage

``` r
run_score_density(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract).

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum group size; grades below this in a given year are
  excluded. Default `10`.

## Value

A named list:

- `$plot` — list: `$density` (named list of ggplots, one per subject)

- `$text` — list: `$narrative`

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
out <- run_score_density(demo_data, assessment_spec)
out$plot$density[["READING"]]
} # }
```
