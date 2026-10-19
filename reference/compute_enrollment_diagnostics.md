# Flag unusual enrollment changes by grade and year

Flag unusual enrollment changes by grade and year

## Usage

``` r
compute_enrollment_diagnostics(
  student_results_long,
  assessment_spec,
  min_n = 10,
  flag_sd = 1.5
)
```

## Arguments

- student_results_long:

  A long-format student results data frame.

- assessment_spec:

  Named list describing the assessment program.

- min_n:

  Integer. Default `10`.

- flag_sd:

  Numeric. SD threshold for z-score flagging. Default `1.5`.

## Value

A named list:

- `$table` — data frame: `SUBJECT`, `GRADE`, `YEAR`, `N`, `Z_SCORE`,
  `FLAGGED`

- `$narrative` — plain-language markdown summary
