# Flag normatively large achievement level transitions

Computes z-scores for `total_transition_rate` and `net_upward_rate`
across all SUBJECT × GRADE × year-pair cells and flags those beyond
`flag_sd` standard deviations. Generates a plain-language annotation for
each flag.

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

- `$table` — flagged cells with z-scores and annotations

- `$narrative` — markdown summary of flags
