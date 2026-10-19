# Return the index of the policy benchmark level in the achievement level labels

Return the index of the policy benchmark level in the achievement level
labels

## Usage

``` r
benchmark_index(assessment_spec)
```

## Arguments

- assessment_spec:

  Named list describing the assessment program.

## Value

An integer scalar: the position of `policy_benchmark` in
`achievement_levels$labels`.

## Examples

``` r
if (FALSE) { # \dontrun{
source(system.file("specs/00_MS_Assessment_Spec.R", package = "ReportR"))
benchmark_index(assessment_spec)  # 4 for MS (Proficient is 4th of 5 levels)
} # }
```
