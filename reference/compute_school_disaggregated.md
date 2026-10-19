# Compute school-level disaggregated proficiency and within-school gaps

Aggregates `student_results_long` to the school level by demographic
group and computes proficiency rates, within-school gap estimates,
comparisons to statewide subgroup averages, and gap trend
classifications.

## Usage

``` r
compute_school_disaggregated(data, spec, min_n_subgroup = 10)
```

## Arguments

- data:

  A long-format student results data frame.

- spec:

  Named list describing the assessment program.

- min_n_subgroup:

  Integer. Minimum students per subgroup cell. Default `10`.

## Value

A named list:

- `$school_subgroup_proficiency` — school × year × subject × grade ×
  group

- `$district_subgroup_proficiency` — district × year × subject × grade ×
  group

- `$within_school_gaps` — school × year × subject × gap_pair:
  gap_estimate

- `$subgroup_vs_state_comparison` — school × year × subject × group:
  delta

- `$gap_trend_classification` — school × subject × gap_pair: direction
