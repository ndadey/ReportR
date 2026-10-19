# Suppress statistics for small enrollment groups

Sets specified numeric columns to `NA_real_` for rows where the count
column falls below `min_n`. Adds a logical `suppressed` column to the
returned data frame.

## Usage

``` r
suppress_small_n_enroll(df, n_col, min_n, suppress_cols)
```

## Arguments

- df:

  A data frame.

- n_col:

  Character scalar. Name of the column holding the group count.

- min_n:

  Integer. Groups below this size are suppressed.

- suppress_cols:

  Character vector of column names to set to `NA`.

## Value

`df` with `suppress_cols` set to `NA_real_` where suppressed, plus a
logical `suppressed` column.

## Details

Uses base-R row indexing rather than
[`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html) +
[`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
to avoid the NA-assignment-into-list-column ambiguity documented in
CLAUDE.md (same pattern as
[`compute_distributions()`](https://ndadey.github.io/ReportR/reference/compute_distributions.md)
small-n suppression).
