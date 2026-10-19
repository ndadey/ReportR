# ReportR

A modular, repeatable analysis pipeline for state summative assessment data.
ReportR reduces the work of building annual reports for state educational agencies
by providing a standard set of analyses that can be run with minimal configuration
changes across different states and years.

**Documentation site:** <https://ndadey.github.io/ReportR/>, including a
[step-by-step walkthrough](https://ndadey.github.io/ReportR/articles/walkthrough.html)
on demo data and a reference page for every function.

## Installation

```r
# install.packages("devtools")
devtools::install_github("ndadey/ReportR")
```

To run the demo examples you will also need the SGPdata package:

```r
install.packages("SGPdata")
```

Optional packages unlock additional plot types:

```r
install.packages(c("patchwork", "ggalluvial", "ggrepel", "openxlsx", "lme4"))
```

## Overview

The package is built around five lines of analysis. Each line follows the same
three-step workflow: **Setup → Modular Analysis → Quarto Report**.

| Line | Focus | Status |
|------|-------|--------|
| 1 | **General Trend Analysis** — systemwide performance over time | Complete |
| 1.2 | **Enrollment Trends** — enrollment counts, subgroup composition, entry/exit | Complete |
| 1.3 | **Cohort Analysis** — same students followed across grades | Complete |
| 2 | **Student Group Analysis** — disaggregated results by demographics | Not yet built |
| 5 | **Aggregate Analysis** — school- and district-level performance | Complete (partial — see below) |
| 4 | **Score Quality Analysis** — diagnostic checks for unusual patterns | Complete |

## Data Format

All analysis functions consume a single long-format data frame called
`student_results_long` — one row per student × year × subject. It is derived from the
**SGP long format** used by the Center for Assessment's SGP ecosystem and produced by
`standardize_student_results()`.

See **[`internal_docs/NOTES-student_results_long.md`](internal_docs/NOTES-student_results_long.md)**
for the full data contract: required columns, raw alias map, state-specific recoding
patterns, EOC handling, and the SGP lineage.

---

## Quick Start

### Step 1 — Create an assessment spec

Copy `inst/specs/00_TEMPLATE_Spec.R` and fill in your assessment's scale score
ranges, achievement level labels, and grade lists. Complete examples are provided:

- `inst/specs/00_DEMO_Assessment_Spec.R` — SGPdata demo (runnable out of the box)
- `inst/specs/00_MS_Assessment_Spec.R` — Mississippi MAAP (5-level reference)
- `inst/specs/00_AR_Assessment_Spec.R` — Arkansas ATLAS

### Step 2 — Run an analysis

```r
library(ReportR)
library(SGPdata)
library(dplyr)

# Load and standardize data (always standardize before filtering by year),
# then drop records with no score and strip the "ELL: " style label prefixes
student_results_long <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results() |>
  filter(!is.na(SCALE_SCORE)) |>
  mutate(across(c(FRL, EL, IEP, GENDER), ~ sub("^.*: ", "", .x)))

# Load spec
source(system.file("specs/00_DEMO_Assessment_Spec.R", package = "ReportR"))
```

Then call the orchestrator for whichever line you need (see sections below).

---

## Line 1 — General Trend Analysis

`run_general_trend_analysis()` runs four sub-analyses and returns `general_trend_results`.

```r
general_trend_results <- run_general_trend_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec
)
```

| Sub-analysis | What it produces |
|---|---|
| `$score_trends` | Mean and SD by year × subject × grade; trend plots; narrative |
| `$proficiency` | Percent proficient by year × subject × grade; heatmap; narrative |
| `$distributions` | Score density plots per subject, one panel per grade |
| `$achievement_transitions` | Cohort-matched level transition tables, heatmaps, flow plots, flags |

Report template: `inst/templates/01_General_Trend_Report.qmd`

---

## Line 1.2 — Enrollment Trends

`run_general_enrollment_analysis()` analyzes enrollment patterns and subgroup composition.

```r
enrollment_results <- run_general_enrollment_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec
)
```

| Sub-analysis | What it produces |
|---|---|
| `$counts` | Total enrollment by year, grade, subject, subgroup, school, district |
| `$proportions` | Subgroup percentages by year, grade, school |
| `$transitions` | Year-over-year subgroup composition changes |
| `$entry_exit` | Performance of entering, continuing, and exiting students |
| `$aggregate` | School- and district-level enrollment summaries |
| `$score_enrollment_trends` | Enrollment change vs. mean score change |

Report templates: `inst/templates/01_Enrollment_Trends_Report.qmd`,
`inst/templates/01_Enrollment_Brief.qmd`

---

## Line 1.3 — Cohort Analysis

`run_general_cohort()` follows the **same students** across grades using persistent
`STUDENT_ID`. Student-ID-dependent analyses (transitions, correlations, mobility,
persistence) return zero-row data frames when IDs are absent.

```r
# Set spec_file attribute so the QMD can source the spec internally
attr(assessment_spec, "spec_file") <- system.file(
  "specs/00_DEMO_Assessment_Spec.R", package = "ReportR"
)

cohort_results <- run_general_cohort(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  output_dir           = "output/cohort",
  render_report        = TRUE
)
```

| Sub-analysis | What it produces |
|---|---|
| `$summary` | Mean score, SD, proficiency by cohort × grade × year |
| `$demographics` | Subgroup composition by cohort |
| `$transitions` | Achievement level transitions across adjacent grades |
| `$rate_of_change` | Improving / stable / declining trajectory proportions |
| `$correlations` | Pearson r of adjacent-grade scale scores |
| `$crosssectional` | Cohort vs. cross-sectional comparison |
| `$mobility` | Entering / continuing / exiting students by grade |
| `$mobility_by_achiev` | Mobility crossed with achievement level |
| `$persistence` | Original cohort retention from the anchor grade |
| `$enrollment_diag` | Z-score flags for unusual enrollment changes |

Report template: `inst/templates/01_General_Cohort_Report.qmd`

---

## Line 4 — Score Quality Analysis

`run_anomaly_analysis()` runs five diagnostic checks and returns `anomaly_results`.

```r
anomaly_results <- run_anomaly_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec
)
```

| Sub-analysis | What it checks |
|---|---|
| `$loss_hoss` | Pileups of scores near the floor (LOSS) or ceiling (HOSS) |
| `$score_integrity` | Scores outside the valid range; level–cut mismatches |
| `$outliers` | Year-to-year changes in mean score or proficiency flagged at ±2/3 SD |
| `$correlations` | Cohort Spearman correlations across adjacent grades and years |
| `$distributions` | Flat, bimodal, or skewed score distributions |

Report template: `inst/templates/04_Anomaly_Report.qmd`

---

## Line 5 — Aggregate (School and District) Analysis

`run_aggregate_analysis()` summarizes performance at the school and district level.

```r
aggregate_results <- run_aggregate_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec
)
```

| Sub-analysis | What it produces | Requires |
|---|---|---|
| `$performance_trends` | School-level mean scores, proficiency, improvement classification | `student_results_long` |
| `$population_shifts` | Demographic composition changes by school | `student_results_long` |
| `$outliers` | Funnel-plot outlier identification; above/below expectation tables | `student_results_long` |
| `$disaggregated` | Within-school achievement gaps by subgroup | `student_results_long` |
| `$stability` | ICC-based rank-order stability across years | `student_results_long` |
| `$accountability` | Accountability grade distributions | `school_results_long` (pending) |
| `$intervention` | HQIM/intervention program analysis | `school_results_long` (pending) |

Report template: `inst/templates/05_Aggregate_Analysis_Report.qmd`

---

## Package Structure

```
ReportR/
├── R/
│   ├── setup_format_data.R             # standardize_student_results()
│   ├── spec_helpers.R                  # get_score_spec(), build_loss_hoss_lookup()
│   ├── globals.R                       # NSE variable declarations
│   ├── utils.R / utils_enrollment.R    # Shared helpers
│   ├── run_general_trend_analysis.R    # Line 1 orchestrator
│   ├── 01_trend_overall.R              # Score trends
│   ├── 01_trend_proficiency.R          # Proficiency trends
│   ├── 01_trend_distributions.R        # Score distributions
│   ├── 01_trend_transitions.R          # Achievement transitions
│   ├── 01_trend_enrollment.R           # Line 1.2 sub-analyses
│   ├── run_general_enrollment_analysis.R  # Line 1.2 orchestrator
│   ├── 01_general_cohort.R             # Line 1.3 sub-analyses
│   ├── run_general_cohort.R            # Line 1.3 orchestrator
│   ├── run_anomaly_analysis.R          # Line 4 orchestrator
│   ├── 04_anomaly_*.R                  # Five score quality sub-analyses
│   ├── aggregate_helpers.R             # Line 5 sub-analyses
│   └── run_aggregate_analysis.R        # Line 5 orchestrator
├── inst/
│   ├── specs/                          # Assessment specification files
│   │   ├── 00_TEMPLATE_Spec.R
│   │   ├── 00_DEMO_Assessment_Spec.R
│   │   ├── 00_MS_Assessment_Spec.R
│   │   └── 00_AR_Assessment_Spec.R
│   ├── templates/                      # Quarto report templates
│   │   ├── 01_General_Trend_Report.qmd
│   │   ├── 01_Enrollment_Trends_Report.qmd
│   │   ├── 01_Enrollment_Brief.qmd
│   │   ├── 01_General_Cohort_Report.qmd
│   │   ├── 04_Anomaly_Report.qmd
│   │   └── 05_Aggregate_Analysis_Report.qmd
│   └── examples/                       # Step-by-step walkthroughs on SGPdata demo data
│       ├── workshop_anomaly_analysis.R
│       └── workshop_general_trend_analysis.R
├── run/                                # Not installed with the package
│   └── run_state.R                     # Builds results lists for a state; renders reports
├── internal/                           # Not installed with the package
│   └── workshop_archive/               # Superseded state-specific driver scripts
├── states/                             # Per-state config + data loaders (mostly gitignored)
└── tests/testthat/                     # Unit tests
```

The demo walkthroughs are installed with the package; find them with
`system.file("examples", package = "ReportR")`.

## License

MIT
