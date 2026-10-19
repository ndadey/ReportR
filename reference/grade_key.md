# Ordering key for grade labels

Converts common grade labels (PK, K, numeric grades, etc.) into a
numeric key used for sorting.

## Usage

``` r
grade_key(g)
```

## Arguments

- g:

  A vector of grade labels (character, integer, or factor).

## Value

A numeric vector. Unknown labels map to `Inf` so they sort last.

## Examples

``` r
grade_key(c("PK", "K", "3", "Grade 5", "8"))
#> [1] -1  0  3  5  8
```
