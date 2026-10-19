# Plot mobility status crossed with achievement level

Plot mobility status crossed with achievement level

## Usage

``` r
plot_mobility_by_achievement(mob_ach_df, assessment_spec)
```

## Arguments

- mob_ach_df:

  Data frame from
  [`compute_mobility_by_achievement()`](https://ndadey.github.io/ReportR/reference/compute_mobility_by_achievement.md).

- assessment_spec:

  Named list describing the assessment program.

## Value

Named list of ggplot objects keyed by SUBJECT, or `NULL`.
