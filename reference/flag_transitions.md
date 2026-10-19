# Flag unusual achievement level transitions

Compares each subject x grade x year pair with the same subject and
grade's other year pairs. For each rate (up, stable, down) and each
from-to level share, the difference from the other years' average is
divided by the standard deviation of such differences across all
subjects and grades, and differences beyond `flag_sd` are flagged.
Comparing within subject and grade keeps stable differences between
grades from being flagged every year; pooling the standard deviation
keeps the test usable with only a few year pairs.

## Usage

``` r
flag_transitions(transition_data, assessment_spec, flag_sd = 1.5)
```

## Arguments

- transition_data:

  Named list returned by
  [`compute_transitions()`](https://ndadey.github.io/ReportR/reference/compute_transitions.md).

- assessment_spec:

  Named list describing the assessment program.

- flag_sd:

  Numeric. Standard deviation threshold for flagging. Default `1.5`.

## Value

A named list:

- `$table` — flagged subject x grade x year pairs, with observed
  (`pct_*`) and typical (`typical_*`) rates, z-scores, and annotations

- `$table_cells` — flagged from-to level shares; `mean_prop` is the
  typical share in the same subject and grade's other year pairs

- `$narrative` — markdown summary naming the largest flags
