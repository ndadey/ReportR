# Standardize student results to the canonical column names

Renames columns from various raw source formats to the canonical names
used throughout the ReportR pipeline (see the data contract in the
package documentation). Call this once after loading raw data, before
passing the data frame to any analysis function.

## Usage

``` r
standardize_student_results(df)
```

## Arguments

- df:

  A data frame of student-level assessment results in long format.

## Value

The same data frame with canonical column names. `YEAR` and `GRADE` are
coerced to integer. Columns that are already using canonical names are
left unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
library(SGPdata)
library(dplyr)
demo_data <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results()
} # }
```
