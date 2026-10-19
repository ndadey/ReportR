# Plot scale score distributions by subject

Creates one kernel density plot per subject. Within each plot, grades
are shown as facets and years are overlaid as separate colored curves.
The x-axis is constrained to the LOSS/HOSS range for that subject (with
a small margin) using `assessment_spec`.

## Usage

``` r
plot_score_density(student_results_long, assessment_spec, min_n = 10)
```

## Arguments

- student_results_long:

  A long-format student results data frame (see data contract). Must
  contain `YEAR`, `SUBJECT`, `GRADE`, and `SCALE_SCORE`.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Grades with fewer than `min_n` students in a given year are
  excluded from the density. Default `10`.

## Value

A named list of ggplot objects, one per subject (keyed by subject code,
e.g. `"ELA"`).

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
plots <- plot_score_density(demo_data, assessment_spec)
plots[["READING"]]
} # }
```
