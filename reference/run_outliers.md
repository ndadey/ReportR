# Run the outlier sub-analysis and return the results sub-list

Run the outlier sub-analysis and return the results sub-list

## Usage

``` r
run_outliers(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Minimum group size for suppression. Default `10`.

## Value

A named list with `$table`, `$plot` (list of `$score` and
`$proficiency`), and `$text` (list of `$table`, `$by_subject`,
`$narrative`).

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
sub <- run_outliers(demo_data, assessment_spec)
} # }
```
