# CLAUDE.md — Project Constitution for the ReportR Package

This file is the authoritative guide for all code generation and editing in this project.
Read it fully before writing any code.

---

## 1. Project Purpose

This is an R package named **`ReportR`** that provides a **repeatable,
modular analysis pipeline** for state summative assessment data. The goal is to reduce
the work of building annual reports for state educational agencies (SEAs) by producing a
standard set of analyses that can be run with minimal configuration changes across
different states and years.

The package is built around four **lines of analysis**:

1. **General Trend Analysis** — systemwide performance over time (means, distributions,
   proficiency, enrollment shifts, cohort patterns)
2. **Student Group Analysis** — disaggregated results by demographic groups and
   intersectional subgroups
3. **Aggregate Analysis** — school- and district-level performance, variability, and outliers
4. **Score Quality Analysis** — diagnostic checks for unusual patterns (HOSS/LOSS pileups,
   score integrity, year-to-year outliers, correlation shifts, distribution anomalies)

Each line of analysis follows the same three-step workflow:
**Setup → Modular Analysis → Report**

---

## 2. Current Package State

`devtools::check()` passes with 0 errors, 0 warnings. The one recurring NOTE
("unable to verify current time") is a network/firewall issue and is not fixable.
All R files live flat in `R/` (R packages do not auto-load subdirectories).
The `dev/` folder contains test scripts and prompt files — not part of the package.

### What is complete

| File | Status | Notes |
|---|---|---|
| `R/setup_format_data.R` | ✅ Complete | `standardize_student_results()` — handles `YYYY_YYYY` year format |
| `R/spec_helpers.R` | ✅ Complete | `get_score_spec()`, `build_loss_hoss_lookup()`, `benchmark_index()` |
| `R/utils.R` | ✅ Complete | `grade_key()` |
| `R/globals.R` | ✅ Complete | All NSE variable declarations; `@importFrom rlang .data :=` |
| `R/run_anomaly_analysis.R` | ✅ Complete | Orchestrator for all five Line 4 sub-analyses |
| `R/04_anomaly_loss_hoss.R` | ✅ Complete | HOSS/LOSS pileup detection |
| `R/04_anomaly_score_integrity.R` | ✅ Complete | Outside-of-range scores + level mismatch checks |
| `R/04_anomaly_outliers.R` | ✅ Complete | Year-to-year outlier detection |
| `R/04_anomaly_correlations.R` | ✅ Complete | Cohort-style Spearman correlation analysis |
| `R/04_anomaly_distributions.R` | ✅ Complete | Distribution shape diagnostics (bimodal, skewed, flat) |
| `R/01_trend_overall.R` | 🔶 Partial | `compute_score_summary()`, `mean_sd_table_wide()` ported; proficiency/density/enrollment functions not yet ported |
| `inst/templates/04_Anomaly_Report.qmd` | ✅ Complete | Full report template for Line 4 |
| `inst/specs/00_MS_Assessment_Spec.R` | ✅ Complete | Mississippi MAAP (5 levels) — reference implementation |
| `inst/specs/00_DEMO_Assessment_Spec.R` | ✅ Complete | SGPdata demo (4 levels) — primary runnable example |
| `inst/specs/00_TEMPLATE_Spec.R` | ✅ Complete | Blank template for new states |

### What is not yet built

- Lines 1 (remaining functions), 2, and 3 analysis functions
- `run_trend_analysis.R`, `run_group_analysis.R`, `run_aggregate_analysis.R` orchestrators
- `inst/templates/` for Lines 1, 2, 3
- GitHub repo and `README.md` (next step before sharing)

---

## 3. Core Workflow

### Step 1: Setup
- The Quarto document calls `devtools::load_all(here::here())` to load all package
  functions, then sources the assessment spec via `params$setup_file`
- Data is loaded into `student_results_long` inside the setup file and passed through
  `standardize_student_results()` to canonicalize column names
- `here::here()` is used for all paths to ensure correct resolution regardless of
  render location

### Step 2: Modular Analysis (one orchestrator per line of analysis)
Each `run_*_analysis()` orchestrator:
1. Calls all `run_*()` sub-functions for its line of analysis
2. Returns a named results list (e.g., `anomaly_results`)
3. Each sub-list contains: `$table` (data frame), `$plot` or `$plot_*` (ggplot objects),
   `$text` (list with at minimum `$narrative`)

### Step 3: Reports
- The orchestrator is called **once** in the setup chunk of the `.qmd` — all subsequent
  chunks only read from the returned object, never re-run analysis
- The rendered `.docx` (primary) + `.html` is the deliverable
- `flextable` is used for all Word-compatible formatted tables
- Narrative text is pulled from `$text$narrative` — never written inline in the `.qmd`

---

## 4. The `assessment_spec` Object

This is the central configuration object. It is a named R list defined in a
`00_<STATE>_Assessment_Spec.R` file. All analysis functions must accept
`assessment_spec` as an argument and read from it — **never hardcode scale score
ranges, grade lists, or achievement level labels**.

### Structure

```r
assessment_spec <- list(

  # 1. Program identity
  program = list(
    name, short_name, state, department, program_type
  ),

  # 2. Subjects
  #    grades = integer vector for span-tested subjects
  #    grades = NULL for EOC subjects
  subjects = list(
    ELA     = list(label = "English Language Arts", grades = 3:8),
    MATH    = list(label = "Mathematics",            grades = 3:8),
    ENG_II  = list(label = "English II",             grades = NULL),
    ALG_I   = list(label = "Algebra I",              grades = NULL)
  ),

  # 3. Achievement levels
  #    labels:           ordered vector, lowest to highest
  #    policy_benchmark: label string of the "proficient" cut
  achievement_levels = list(
    labels           = c("Minimal", "Basic", "Passing", "Proficient", "Advanced"),
    policy_benchmark = "Proficient"
  ),

  # 4. Scale scores
  #    Keys: <SUBJECT>_<GRADE> for span-tested, <SUBJECT> for EOC
  #    loss: lowest obtainable scale score
  #    hoss: highest obtainable scale score
  #    cuts: upper bounds of each level except the top (whose ceiling
  #          is hoss). Length always = length(achievement_levels$labels) - 1.
  #          Ordered low to high.
  scale_scores = list(
    ELA_3  = list(loss = 301,  hoss = 399,  cuts = c(334, 349, 364, 386)),
    ELA_4  = list(loss = 401,  hoss = 499,  cuts = c(428, 449, 464, 487)),
    # ... remaining span-tested entries follow same pattern
    ENG_II = list(loss = 1001, hoss = 1099, cuts = c(1036, 1049, 1064, 1080)),
    ALG_I  = list(loss = 1001, hoss = 1099, cuts = c(1038, 1049, 1064, 1087))
    # EOC subjects use subject name alone as key (no grade suffix)
  ),

  # 5. Years & cohorts
  years = list(
    tested_years        = 2018:2025,
    cohort_anchor_grade = 3
  ),

  # 6. Demographics
  demographics = list(
    ETHNICITY = list(type = "categorical"),
    IEP       = list(type = "binary"),
    LEP       = list(type = "binary"),
    FRL       = list(type = "binary"),
    GENDER    = list(type = "categorical")
  )
)
```

### Key conventions

- **Scale score lookup**: `get_score_spec(assessment_spec, subject, grade = NULL)` — use
  this helper; do not build the key manually in analysis functions
- **Level derivation**: level index = `sum(score > cuts) + 1`; label =
  `achievement_levels$labels[index]`
- **Number of levels**: always `length(achievement_levels$labels)` — never hardcode 4 or 5.
  MS has 5 levels; DEMO has 4. All functions must handle both.
- **Policy benchmark index**: `benchmark_index(assessment_spec)` helper in `spec_helpers.R`
- **Binary vs categorical demographics**: `demographics[[var]]$type == "binary"` — only
  binary vars get a single `pct_*` column; categorical vars (RACE_ETHNICITY) require
  separate handling

**Two real specs and one demo are provided:**
- `inst/specs/00_MS_Assessment_Spec.R` — Mississippi MAAP (5 levels) — **primary reference**
- `inst/specs/00_DEMO_Assessment_Spec.R` — SGPdata demo (4 levels) — **primary runnable example**
- `inst/specs/00_TEMPLATE_Spec.R` — blank template for new states

---

## 5. Data Contract: `student_results_long`

After calling `standardize_student_results()`, the expected column names are:

| Column             | Type      | Notes                                          |
|--------------------|-----------|------------------------------------------------|
| `STUDENT_ID`       | character | Required for correlation analysis              |
| `YEAR`             | integer   | Administration year; `YYYY_YYYY` format auto-converted |
| `GRADE`            | integer   | Tested grade (NA for EOC subjects)             |
| `SUBJECT`          | character | Matches keys in `assessment_spec$subjects`     |
| `SCALE_SCORE`      | numeric   | Student scale score                            |
| `ACHIEVEMENT_LEVEL`| character | Level label matching `achievement_levels$labels` |
| `SCHOOL_ID`        | character |                                                |
| `SCHOOL_NAME`      | character |                                                |
| `DISTRICT_ID`      | character |                                                |
| `DISTRICT_NAME`    | character |                                                |
| `RACE_ETHNICITY`   | character |                                                |
| `FRL`              | character | Binary demographic                             |
| `EL`               | character | Binary demographic (English Learner)           |
| `IEP`              | character | Binary demographic                             |
| `GENDER`           | character |                                                |

`standardize_student_results()` (in `R/setup_format_data.R`) handles messy incoming
column names via `rename_if_exists()`. It also converts `YYYY_YYYY` academic-year
strings (e.g. `"2015_2016"`) to the end year integer (`2016`).

---

## 6. Line 4: Score Quality Analysis — Implemented Sub-analyses

Line 4 is the most complete module and is the canonical template for all other lines.
The orchestrator is `run_anomaly_analysis()` in `R/run_anomaly_analysis.R`.

**Important orchestrator behaviour:** `run_anomaly_analysis()` runs all five sub-analyses
in order, then regenerates `anomaly_results$loss_hoss$plot_distributions` with the
distribution stats from `anomaly_results$distributions$table` included. This means the
unified score distribution plots contain both HOSS/LOSS and shape statistics.

### 4.1 `anomaly_results$loss_hoss` — HOSS/LOSS Pileups (`R/04_anomaly_loss_hoss.R`)

**Functions:** `compute_loss_hoss_table()`, `plot_loss_hoss_cleveland()`,
`plot_loss_hoss_heatmap()`, `plot_loss_hoss_over_time()`, `summarize_loss_hoss_findings()`,
`plot_loss_hoss_distributions()`, `run_loss_hoss()`

**`run_loss_hoss()` returns:**
- `$table` — pct near HOSS/LOSS by YEAR × SUBJECT × GRADE, with `pct_near_hoss_change`
  and `pct_near_loss_change` delta columns; severity flags (None/Mild/Moderate/Severe)
- `$plot_hoss_cleveland`, `$plot_loss_cleveland` — Cleveland dot plots: years on y-axis
  (nested within grade), dotted leader lines from x=0 to each point, grade as colour
- `$plot_hoss_heat`, `$plot_loss_heat` — heatmaps (computed but not shown in current QMD)
- `$plot_hoss_line`, `$plot_loss_line` — trend lines (computed but not shown in current QMD)
- `$plot_distributions` — **flat** named list of ggplot objects, one per subject/grade
  (e.g. `"ELA_Grade3"`). Each plot shows all years overlaid, x-axis restricted to
  LOSS–HOSS ± margin, both HOSS (red dashed) and LOSS (blue dashed) boundaries, cut
  score lines, and a per-year annotation (% near HOSS, % near LOSS, skewness, BC).
  Regenerated by the orchestrator after distributions are computed.
- `$text` — `$narrative` (markdown bullet list), `$by_subject`, `$table`

**Key parameters:** `point_adjust = 5`, `mild_thresh = 5`, `moderate_thresh = 10`,
`severe_thresh = 15`

**`plot_loss_hoss_distributions()` signature:**
```r
plot_loss_hoss_distributions(student_results_long, assessment_spec, loss_hoss_table,
                              distribution_table = NULL, point_adjust = 5, x_margin = 10)
```
`distribution_table` is the output of `compute_distributions()` — when supplied, skewness
and bimodality coefficient are added to the per-year annotation.

### 4.2 `anomaly_results$score_integrity` — Score Integrity (`R/04_anomaly_score_integrity.R`)

**Functions:** `compute_score_integrity()`, `plot_score_integrity()`,
`summarize_score_integrity()`, `run_score_integrity()`

**`run_score_integrity()` returns:**
- `$table` — summary by YEAR × SUBJECT × GRADE: `n_invalid` (outside LOSS–HOSS),
  `pct_invalid`, `n_mismatch` (level doesn't match cuts), `pct_mismatch`
- `$invalid_records` — individual student records with outside-of-range scores
- `$mismatch_records` — individual student records with level mismatches
- `$plot` — `$invalid` (bar chart), `$mismatch` (heatmap), or NULL if nothing flagged
- `$text` — `$narrative`, `$table`

### 4.3 `anomaly_results$outliers` — Year-to-Year Outliers (`R/04_anomaly_outliers.R`)

**Functions:** `compute_outliers()`, `plot_outliers()`, `summarize_outliers()`,
`run_outliers()`

**`run_outliers()` returns:**
- `$table` — one row per YEAR × SUBJECT × GRADE with `mean_score`, `pct_proficient`,
  `pct_<binary_demo>` columns, corresponding `delta_*` columns, plus `outlier_flag`
  (None/Mild/Severe), `outlier_metric`, `outlier_direction`
- `$plot` — `$score` and `$proficiency`: year-pair on x, delta on y, lines per grade,
  faceted by subject, flagged points highlighted
- `$text` — `$narrative`, `$by_subject` (flagged counts by SUBJECT × severity), `$table`

**Flagging:** ±2 SD = Mild, ±3 SD = Severe (standardised across all SUBJECT × GRADE cells)

### 4.4 `anomaly_results$correlations` — Year-to-Year Correlations (`R/04_anomaly_correlations.R`)

**Functions:** `compute_correlations()`, `.make_corr_wide()` (internal),
`plot_correlations()`, `summarize_correlations()`, `run_correlations()`

**Correlation approach:** Cohort-style — matches student Grade G in year T to their
Grade G+1 score in year T+1. Each row represents one SUBJECT × grade-pair × year-pair.

**`run_correlations()` returns:**
- `$table` — one row per SUBJECT × grade-pair × year-pair: `YEAR_1`, `YEAR_2`,
  `GRADE_1`, `GRADE_2`, `n`, `correlation` (Spearman, NA if `n < min_n`),
  `low_flag` (< 0.5), `drop_flag` (> 0.15 drop from prior year-pair)
- `$plot` — `$line` (grade-pair lines over year-pairs, faceted by subject) and
  `$heatmap` (diverging fill anchored at 0.7)
- `$text` — `$narrative`, `$wide_table` (Subject + Grade Pair rows, interleaved
  Correlation/N columns per year-pair), `$by_subject`, `$table`

**Requires `STUDENT_ID`** — returns empty frame with message if absent.

### 4.5 `anomaly_results$distributions` — Distribution Shape (`R/04_anomaly_distributions.R`)

**Functions:** `compute_distributions()`, `plot_distributions()`,
`summarize_distributions()`, `run_distributions()`

**`run_distributions()` returns:**
- `$table` — one row per YEAR × SUBJECT × GRADE: `mean`, `sd`, `skewness`, `kurtosis`,
  `p05`–`p95` percentiles, `bimodality_coefficient`, `distribution_flag`
  (None/Bimodal/Skewed/Flat), `flag_reason`
- `$plot` — `$flag_summary` (bar, NULL if no flags), `$shape_heatmap`
  (bimodality coefficient heatmap), `$density_flagged` (named list of density plots
  for flagged cells only)
- `$text` — `$narrative`, `$by_subject`, `$table`

**Flagging thresholds:** Bimodal = BC > 0.555, Skewed = |skewness| > 1.0,
Flat = SD < 0.25 × (HOSS − LOSS). Priority: Bimodal > Skewed > Flat.

---

## 7. Report Structure: `inst/templates/04_Anomaly_Report.qmd`

The QMD is titled **"Section 4. Score Quality Analysis"**. Section numbering uses
Quarto's `number-sections: true`. The major sections are:

1. **Introduction**
2. **Score Distribution Patterns** *(combines HOSS/LOSS and distribution diagnostics)*
   - 2.1 Potential Ceiling and Floor Effects — narrative, summary table, Cleveland plots
   - 2.2 Score Distribution Shape — narrative, distribution shape statistics table (all
     grades/subjects, columns: Year, Subject, Grade, N, Mean, SD, Skewness, Bimodality)
   - 2.3 Score Distribution Plots — unified density plots from `plot_distributions`
3. **Score Integrity**
4. **Year-to-Year Outliers** — narrative, delta summary table, flagged cells table, plots
5. **Year-to-Year Correlations** — narrative, wide summary table, flagged grade pairs,
   line plot, heatmap
6. **Distribution Diagnostics** *(shape narrative only — plots in §2)*

**Key QMD conventions:**
- All analysis runs once in the `setup` chunk via `run_anomaly_analysis()`
- Subsequent chunks only read from `anomaly_results`
- `flextable` for all tables; `merge_v()` used for Subject/Grade columns where appropriate

---

## 8. File and Package Structure

### Actual R Package Layout (as built)

```
ReportR/
├── DESCRIPTION
├── NAMESPACE
├── CLAUDE.md
├── .Rbuildignore                          ← excludes dev/, .claude/, Word temp files
├── R/
│   ├── setup_format_data.R               ← standardize_student_results()
│   ├── utils.R                           ← grade_key()
│   ├── spec_helpers.R                    ← get_score_spec(), build_loss_hoss_lookup(),
│   │                                        benchmark_index()
│   ├── globals.R                         ← globalVariables() + @importFrom rlang .data :=
│   ├── run_anomaly_analysis.R            ← orchestrator → returns anomaly_results
│   ├── 04_anomaly_loss_hoss.R            ← anomaly_results$loss_hoss ✅
│   ├── 04_anomaly_score_integrity.R      ← anomaly_results$score_integrity ✅
│   ├── 04_anomaly_outliers.R             ← anomaly_results$outliers ✅
│   ├── 04_anomaly_correlations.R         ← anomaly_results$correlations ✅
│   ├── 04_anomaly_distributions.R        ← anomaly_results$distributions ✅
│   └── 01_trend_overall.R               ← trend_results$overall (partial) 🔶
│
├── inst/
│   ├── specs/
│   │   ├── 00_MS_Assessment_Spec.R       ← real spec (MAAP, 5 levels) — reference
│   │   ├── 00_DEMO_Assessment_Spec.R     ← SGPdata demo (4 levels) — runnable example
│   │   └── 00_TEMPLATE_Spec.R            ← blank template for new states
│   └── templates/
│       └── 04_Anomaly_Report.qmd         ← full report for Line 4 ✅
│
├── man/                                  ← roxygen2-generated docs
├── tests/
│   └── testthat/
│       ├── test-04_anomaly_loss_hoss.R
│       └── test-01_trend_overall.R
└── dev/                                  ← not part of package (in .Rbuildignore)
    ├── prompts/                          ← implementation prompt specs
    ├── references/                       ← old QMD versions
    └── test_*.R                          ← manual test scripts
```

### Important: R packages do not auto-load subdirectories

All function files must live directly in `R/` — not in subdirectories. Files in
subdirectories are silently ignored by `devtools::load_all()` and `R CMD check`.
Use the `NN_module_subsection` filename prefix for organisation instead.

### One file per sub-analysis — the governing rule

Every file in `R/` prefixed with a module number corresponds to exactly one sub-list in
its results object. The file contains all `compute_*()`, `plot_*()`, `summarize_*()`,
and `run_*()` functions for that sub-list.

### File naming conventions
- Analysis function files: `NN_module_subsection.R` (e.g., `04_anomaly_loss_hoss.R`)
- Orchestrators: `run_<module>_analysis.R`
- Quarto reports: `NN_Module_Name.qmd` in `inst/templates/`
- Assessment specs: `00_<STATE>_Assessment_Spec.R` in `inst/specs/`

### Do not edit `dev/` files
`dev/` is a scratch area for manual testing and reference. Do not modify files there
unless explicitly asked.

---

## 9. Coding Conventions

### Style
- **Tidyverse throughout**: use `dplyr`, `tidyr`, `ggplot2`; pipe with `|>` (native pipe)
- No base R apply loops where a `dplyr` chain is cleaner
- Use `.data[["col"]]` pronoun in dplyr/ggplot — not bare names
- `stringsAsFactors = FALSE` always when constructing data frames manually
- Do **not** use `isTRUE()` on a vector column — use `!is.na(x) & x` instead
- Do **not** use `dplyr::if_else()` for scalar conditions inside loops — use base R `if/else`

### Function signatures
- `compute_*()` — first two args: `student_results_long`, `assessment_spec`; optional
  tuning params with sensible defaults (always include `min_n = 10`)
- `plot_*()` — first arg: output of the corresponding `compute_*()`; second arg:
  `assessment_spec` when subject labels or spec values are needed
- `summarize_*()` — returns a list with at minimum `$table`, `$by_subject`, `$narrative`
- `run_*()` — calls `compute_*()`, `plot_*()`, `summarize_*()`; returns fully populated
  sub-list; defined at the bottom of its functions file
- All functions must include `stopifnot()` input validation at the top

### NSE variable declarations
Any bare column name used in `dplyr`/`ggplot2` non-standard evaluation must be declared
in `R/globals.R` via `utils::globalVariables()`. Add new variables there whenever a
new function introduces them.

### roxygen2 Documentation
Every exported function needs:
```r
#' @title
#' @description
#' @param student_results_long A long-format student results data frame (see data contract)
#' @param assessment_spec Named list describing the assessment program (see assessment_spec)
#' @return
#' @export
#' @examples
```

---

## 10. Quarto Report Conventions

### Setup chunk pattern (all templates)

```r
devtools::load_all(here::here())
source(here::here(params$setup_file))   # loads assessment_spec and student_results_long

results <- run_*_analysis(
  student_results_long = student_results_long,
  assessment_spec      = assessment_spec,
  report_years         = params$report_years,
  min_n                = params$min_n
)
```

### YAML params pattern

```yaml
params:
  setup_file:   "MS/_setup_light.R"   # sources spec + loads data
  report_years: [2023, 2024, 2025]
  min_n:        10
```

- `setup_file` sources both the spec and loads the data, putting `assessment_spec`
  and `student_results_long` in scope
- Do not add params for things already in the spec (no `ASSESSMENT_NAME`, etc.)
- `report_years` and `min_n` are runtime decisions that legitimately live in params

### flextable for Word output
All formatted tables in `.qmd` templates must use `flextable` — not `knitr::kable()` —
for Word-compatible output. Standard pattern:

```r
flextable::flextable(df) |>
  flextable::bold(part = "header") |>
  flextable::fontsize(size = 9, part = "all") |>
  flextable::set_table_properties(layout = "autofit", width = 1)
```

Color-code severity: Severe = `"#f4cccc"` (red), Mild/flagged = `"#fce8b2"` (yellow),
drops/secondary = `"#f6b26b"` (orange), informational = `"#9fc5e8"` (blue).

### Output formats

**Full analysis report** — `.docx` (primary) + `.html`:
```yaml
format:
  docx:
    toc: true
    number-sections: true
  html:
    toc: true
    toc-float: true
    code-fold: true
execute:
  echo: false
  warning: false
  message: false
```

---

## 11. DESCRIPTION File

Current state of `DESCRIPTION`:

```
Package: ReportR
Title: Repeatable Analysis Pipeline for State Summative Assessment Data
Version: 0.1.0
Imports:
    dplyr (>= 1.1.0),
    tidyr (>= 1.3.0),
    ggplot2 (>= 3.4.0),
    rlang,
    stats
Suggests:
    testthat (>= 3.0.0),
    devtools,
    here,
    flextable,
    SGPdata
```

`flextable` and `here` are in `Suggests` (used in templates/dev scripts, not in package
functions). `stats` is a base R package and could be dropped from `Imports` without
consequence. `rlang` ships with `dplyr` but is kept as a direct dependency because
`.data` and `:=` originate there.

---

## 12. Testing Expectations

Every `compute_*()` function must have at least one `testthat` test in
`tests/testthat/test-NN_module_subsection.R`.

### Demo data setup

```r
library(SGPdata)
library(dplyr)

demo_data <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results()
```

**Important:** `sgpData_LONG` uses `YYYY_YYYY` year strings (e.g. `"2015_2016"`).
`standardize_student_results()` converts these to the end year integer automatically.
The DEMO spec's `tested_years` are `2013:2019`; the data contains grades 3–10 but the
spec only defines grades 3–8.

**Limitations of demo data:**
- No `SCHOOL_ID` or `DISTRICT_ID` — Line 3 cannot be run
- `VALID_CASE` must be filtered before use
- Scale score cutpoints in DEMO spec are approximate
- No persistent student IDs across grades — correlation analysis returns 0 rows

### Minimal synthetic data for correlation testing (persistent IDs across grades)

```r
set.seed(42)
n <- 200
test_data <- do.call(rbind, lapply(c("ELA", "MATHEMATICS"), function(s) {
  do.call(rbind, lapply(seq_len(6), function(step) {
    data.frame(
      STUDENT_ID = paste0("S", seq_len(n)),
      YEAR = 2013L + step - 1L, GRADE = 3L + step - 1L, SUBJECT = s,
      SCALE_SCORE = rnorm(n, 450 + (3L + step - 1L) * 10, 30),
      ACHIEVEMENT_LEVEL = sample(c("Unsatisfactory","Partially Proficient",
                                   "Proficient","Advanced"), n, replace = TRUE),
      SCHOOL_ID = "SCH001", DISTRICT_ID = "DIST01",
      stringsAsFactors = FALSE
    )
  }))
}))
```

### What each test must cover

- **Happy path**: function runs without error; output has expected named elements
- **Structure check**: `$table` is a data frame with expected column names
- **Small-n suppression**: values are `NA` (not dropped) when `n < min_n`
- **Variable level counts**: correct output with 4-level DEMO spec and 5-level MS spec

---

## 13. Line 1: Porting Notes for Remaining Functions

`R/01_trend_overall.R` has `compute_score_summary()` and `mean_sd_table_wide()`.
The following pre-package scripts still need porting:

### What needs to change in every file
1. **Column names**: replace `CONTENT_AREA` → `SUBJECT` throughout
2. **`assessment_spec` integration**: add as second argument; read proficiency cuts,
   level labels, and grade lists from spec rather than inferring from data
3. **Function naming**: rename to `compute_*()` / `plot_*()` / `summarize_*()` pattern

### Target file mapping

| Existing script | Target package file | New function name(s) |
|---|---|---|
| `make_proficiency.R` | `R/01_trend_overall.R` | `compute_proficiency()` |
| `summarize_proficiency.R` | `R/01_trend_overall.R` | `compute_proficiency_summary()` |
| `visualize_proficiency.R` | `R/01_trend_overall.R` | `plot_proficiency()` |
| `visualize_score_density.R` | `R/01_trend_overall.R` | `plot_score_density()` |
| `summarize_enrollment.R` | `R/01_trend_enrollment.R` | `compute_enrollment_summary()` |
| `summarize_enrollment_change.R` | `R/01_trend_enrollment.R` | `compute_enrollment_change()` |

`make_proficiency.R` currently takes `n_proficiencies` and infers level order from label
strings — replace with `cuts` and `labels` from `assessment_spec`.

`summarize_enrollment.R` and `summarize_enrollment_change.R` use `stats::aggregate()`,
`merge()`, and `for` loops — rewrite entirely in dplyr.

---

## 14. Getting Started (for Claude Code in a new session)

1. Read this file completely before touching any code.
2. Run `devtools::check()` first to confirm the baseline is clean (0 errors, 0 warnings).
   The persistent NOTE ("unable to verify current time") is a network issue — ignore it.
3. Line 4 (Score Quality Analysis) is the canonical template — match its patterns exactly
   when building other modules.
4. Before adding any new NSE variable to a function, add it to `R/globals.R`.
5. Use `dev/test_*.R` scripts for rapid manual testing; use `testthat` for automated tests.
   Do **not** edit files in `dev/` unless explicitly asked.
6. The DEMO spec + `sgpData_LONG` is the primary runnable example. The MS spec is the
   reference for real-data structure. Always test new functions against both.
7. Do not use `isTRUE()` on a vector — use `!is.na(x) & x` for logical column filtering.
8. Do not use `dplyr::if_else()` for scalar conditions (e.g., inside a loop or `if/else`
   branch on a single value) — use base R `if/else` instead.
