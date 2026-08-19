# =============================================================================
# states/DEMO/_setup.R — Demo data setup
#
# Uses the public SGPdata package — no local data file required.
# This is the reference implementation showing the pattern all state
# setup scripts must follow:
#   - Load raw data
#   - Apply any state-specific column/level recoding
#   - Call standardize_student_results()
#   - Leave student_results_long in the local environment
#
# standardize_student_results() is available because run_state() calls
# devtools::load_all() before sourcing this script.
# =============================================================================

library(SGPdata)
library(dplyr)

student_results_long <- sgpData_LONG |>
  dplyr::filter(.data[["VALID_CASE"]] == "VALID_CASE") |>
  standardize_student_results()
