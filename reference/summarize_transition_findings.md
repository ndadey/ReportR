# Summarize achievement transition findings in plain text

Summarize achievement transition findings in plain text

## Usage

``` r
summarize_transition_findings(transition_data, assessment_spec, flags = NULL)
```

## Arguments

- transition_data:

  Named list returned by
  [`compute_transitions()`](https://ndadey.github.io/ReportR/reference/compute_transitions.md).

- assessment_spec:

  Named list describing the assessment program.

- flags:

  Optional named list returned by
  [`flag_transitions()`](https://ndadey.github.io/ReportR/reference/flag_transitions.md).
  When supplied, the number of flagged grade-year combinations is noted
  for each subject. Default `NULL`.

## Value

A named list with `$narrative`: one markdown bullet per subject giving
the average shares that stayed, moved up, and moved down, plus the most
extreme grade-year combinations.
