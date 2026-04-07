# ReportR

A modular, repeatable analysis pipeline for state summative assessment data.
ReportR reduces the work of building annual reports for state educational agencies
by providing a standard set of analyses that can be run with minimal configuration
changes across different states and years.

## Installation

```r
# install.packages("devtools")
devtools::install_github("ndadey/ReportR")
```

To run the demo examples you will also need the SGPdata package:

```r
install.packages("SGPdata")
```

## Overview

The package is built around four lines of analysis:

| Line | Focus |
|------|-------|
| 1 | **General Trend Analysis** — systemwide performance over time |
| 2 | **Student Group Analysis** — disaggregated results by demographics |
| 3 | **Aggregate Analysis** — school- and district-level performance |
| 4 | **Score Quality Analysis** — diagnostic checks for unusual patterns |

Line 4 (Score Quality Analysis) is fully implemented. Lines 1–3 are in development.

## Quick Start

### Step 1 — Create an assessment spec

Copy `inst/specs/00_TEMPLATE_Spec.R` and fill in your assessment's scale score
ranges, achievement level labels, and grade lists. Two complete examples are
provided:

- `inst/specs/00_DEMO_Assessment_Spec.R` — SGPdata demo (runnable out of the box)
- `inst/specs/00_MS_Assessment_Spec.R` — Mississippi MAAP (5-level reference)

### Step 2 — Run the analysis

```r
library(ReportR)
library(SGPdata)
library(dplyr)

# Load and standardize data
student_results_long <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results()

# Load spec
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))

# Run all five Score Quality sub-analyses
anomaly_results <- run_anomaly_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  min_n                = 10
)

# Save for use in the report
saveRDS(anomaly_results, "anomaly_results.rds")
```

### Step 3 — Render the report

```r
library(quarto)

quarto::quarto_render(
  input          = system.file("templates/04_Anomaly_Report.qmd", package = "ReportR"),
  output_format  = "docx",
  execute_params = list(
    pkg_path     = find.package("ReportR"),
    spec_file    = "specs/00_DEMO_Assessment_Spec.R",
    results_file = "anomaly_results.rds",
    min_n        = 10L
  )
)
```

## Score Quality Analysis — Sub-analyses

`run_anomaly_analysis()` runs five checks and returns a named results list:

| Sub-analysis | What it checks |
|---|---|
| `$loss_hoss` | Pileups of scores near the floor (LOSS) or ceiling (HOSS) |
| `$score_integrity` | Scores outside the valid range; level–cut mismatches |
| `$outliers` | Year-to-year changes in mean score or proficiency rate flagged at ±2/3 SD |
| `$correlations` | Cohort Spearman correlations across adjacent grades and years |
| `$distributions` | Flat, bimodal, or skewed score distributions |

## Package Structure

```
ReportR/
├── R/                          # All analysis functions
│   ├── setup_format_data.R     # standardize_student_results()
│   ├── spec_helpers.R          # get_score_spec(), build_loss_hoss_lookup()
│   ├── run_anomaly_analysis.R  # Line 4 orchestrator
│   ├── 04_anomaly_*.R          # Five score quality sub-analyses
│   └── 01_trend_overall.R      # Line 1 (partial)
├── inst/
│   ├── specs/                  # Assessment specification files
│   └── templates/              # Quarto report templates
└── tests/testthat/             # Unit tests
```

## License

MIT
