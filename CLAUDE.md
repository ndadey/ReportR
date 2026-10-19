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
   proficiency, achievement transitions)
2. **Student Group Analysis** — disaggregated results by demographic groups and
   intersectional subgroups
3. **Aggregate Analysis** — school- and district-level performance, variability, and outliers
4. **Score Quality Analysis** — diagnostic checks for unusual patterns (HOSS/LOSS pileups,
   score integrity, year-to-year outliers, correlation shifts, distribution anomalies)

Each line of analysis follows the same three-step workflow:
**Setup → Modular Analysis → Report**

**Package family context:** `ARCHITECTURE.md` (repo root) describes the planned `stateR`
family, in which this package is slated to be renamed `stateSummative`
(`[PROPOSED — CONFIRM]`) alongside `stateSpecs`, `stateELP`, and others. Read it before
any structural change. Do **not** rename the package, move functions between packages,
or adopt the family naming until the user confirms.

---

## 2. Current Package State

`devtools::check()` baseline (2026-10-19): **0 errors, 0 warnings, 0 notes**. Keep it
that way: any new warning or note is a regression. The only acceptable exception is
the intermittent NOTE "unable to verify current time", a network/firewall issue.
All R files live flat in `R/` (R packages do not auto-load subdirectories).
The `dev/` folder contains test scripts and prompt files — not part of the package.

The package is on GitHub at **github.com/ndadey/ReportR** and can be installed with:
```r
devtools::install_github("ndadey/ReportR")
```

### What is complete

| File | Status | Notes |
|---|---|---|
| `R/setup_format_data.R` | ✅ Complete | `standardize_student_results()` — handles `YYYY_YYYY` year format |
| `R/spec_helpers.R` | ✅ Complete | `get_score_spec()`, `build_loss_hoss_lookup()`, `benchmark_index()`, `join_score_bounds()` |
| `R/utils.R` | ✅ Complete | `grade_key()` |
| `R/export_tables.R` | ✅ Complete | `export_tables_to_excel()` — exported helper that writes every data frame in a results list to xlsx |
| `R/student_results_long.R` | ✅ Complete | roxygen-only doc page for the `student_results_long` data contract |
| `R/globals.R` | ✅ Complete | All NSE variable declarations; `@importFrom rlang .data :=` |
| `R/run_anomaly_analysis.R` | ✅ Complete | Orchestrator for all five Line 4 sub-analyses |
| `R/04_anomaly_loss_hoss.R` | ✅ Complete | HOSS/LOSS pileup detection |
| `R/04_anomaly_score_integrity.R` | ✅ Complete | Outside-of-range scores + level mismatch checks |
| `R/04_anomaly_outliers.R` | ✅ Complete | Year-to-year outlier detection |
| `R/04_anomaly_correlations.R` | ✅ Complete | Cohort-style Spearman correlation analysis |
| `R/04_anomaly_distributions.R` | ✅ Complete | Distribution shape diagnostics (bimodal, skewed, flat) |
| `R/run_general_trend_analysis.R` | ✅ Complete | Orchestrator for all Line 1 sub-analyses |
| `R/01_trend_overall.R` | ✅ Complete | Score summary, trend plots, narrative |
| `R/01_trend_proficiency.R` | ✅ Complete | Proficiency trends + narrative |
| `R/01_trend_distributions.R` | ✅ Complete | Score density plots per subject/grade |
| `R/01_trend_transitions.R` | ✅ Complete | Achievement level transition matrices + flags |
| `inst/templates/04_Anomaly_Report.qmd` | ✅ Complete | Full report template for Line 4 |
| `inst/templates/01_General_Trend_Report.qmd` | ✅ Complete | Full report template for Line 1 |
| `inst/specs/00_MS_Assessment_Spec.R` | ✅ Complete | Mississippi MAAP (5 levels) — nested scale_scores structure |
| `inst/specs/00_AR_Assessment_Spec.R` | ✅ Complete | Arkansas ATLAS (4 levels) — nested scale_scores structure |
| `inst/specs/00_DEMO_Assessment_Spec.R` | ✅ Complete | SGPdata 28 demo (4 levels): READING + MATHEMATICS, grades 3–10, 2020–2024; LOSS/HOSS and cuts from `SGP::SGPstateData[["DEMO"]]` |
| `inst/specs/00_TEMPLATE_Spec.R` | ✅ Complete | Blank template for new states |
| `inst/examples/workshop_anomaly_analysis.R` | ✅ Complete | Step-by-step walkthrough for Line 4 (demo data; installed with the package) |
| `inst/examples/workshop_general_trend_analysis.R` | ✅ Complete | Step-by-step walkthrough for Line 1 (demo data; installed with the package) |
| `R/01_general_cohort.R` | ✅ Complete | All Line 1.3 cohort compute/plot/summarize/run functions |
| `R/run_general_cohort.R` | ✅ Complete | Line 1.3 orchestrator — saves RDS, renders report |
| `inst/templates/01_General_Cohort_Report.qmd` | ✅ Complete | Full report template for Line 1.3 |
| `R/utils_enrollment.R` | ✅ Complete | `suppress_small_n_enroll()` helper + Step 1 findings comment |
| `R/01_trend_enrollment.R` | ✅ Complete | All Line 1.2 compute/plot/summarize/run functions |
| `R/run_general_enrollment_analysis.R` | ✅ Complete | Line 1.2 orchestrator — returns `general_enrollment_results` |
| `inst/templates/01_Enrollment_Trends_Report.qmd` | ✅ Complete | Full report template for Line 1.2 |
| `inst/templates/01_Enrollment_Brief.qmd` | ✅ Complete | One-page brief template for Line 1.2 |
| `R/aggregate_helpers.R` | ✅ Complete | All five Line 5 compute_* functions |
| `R/run_aggregate_analysis.R` | ✅ Complete | Line 5 orchestrator — returns `aggregate_results` |
| `inst/templates/05_Aggregate_Analysis_Report.qmd` | ✅ Complete | Full report template for Line 5 (student_results_long sections) |
| `run/run_state.R` | ✅ Complete | Generic single-state runner (not installed) — set `STATE <- "AR"` etc. to switch states; full xlsx export + re-export-only mode (see Section 3) |
| `internal/workshop_archive/` | 🗄 Archived | Superseded per-line MS MAAP driver scripts (`MS_workshop_*.R`), replaced by `run/run_state.R`; not installed |
| `README.md` | ✅ Complete | Install instructions, quick start, package overview |
| `ARCHITECTURE.md` | 📝 Planning doc | `stateR` package-family architecture; contains `[PROPOSED — CONFIRM]` items |
| `_pkgdown.yml`, `pkgdown/` | ✅ Complete | pkgdown site config, home page (`pkgdown/index.md`), and `extra.css` (see Section 9b) |
| `vignettes/articles/walkthrough.Rmd` | ✅ Complete | Site-only walkthrough article; runs every line on SGPdata at build time |
| `.github/workflows/pkgdown.yaml` | ✅ Complete | Builds the site on every push to `main` and deploys it to `gh-pages` |

### What is not yet built

- Line 2 (Student Group Analysis) functions and report
- Line 3 (school/district) functions dependent on `school_results_long` (accountability grades,
  HQIM flags, intervention analysis) — placeholders exist in `run_aggregate_analysis.R`
- `inst/templates/` for Line 2

---

## 3. Core Workflow

### Step 1: Setup
- User sources an assessment spec file (e.g. `inst/specs/00_DEMO_Assessment_Spec.R`)
  which puts `assessment_spec` in scope
- Data is loaded and passed through `standardize_student_results()` to canonicalize
  column names

### Step 2: Modular Analysis (one orchestrator per line of analysis)
Each `run_*_analysis()` orchestrator:
1. Calls all `run_*()` sub-functions for its line of analysis
2. Returns a named results list (e.g., `anomaly_results`)
3. Each sub-list contains: `$table` (data frame), `$plot` or `$plot_*` (ggplot objects),
   `$text` (list with at minimum `$narrative`)
4. Results are saved to RDS via `saveRDS()` for use in the report

### Step 3: Reports
- The QMD reads pre-computed results from RDS — analysis never re-runs inside the QMD
- `assessment_spec` is sourced directly in the QMD setup chunk (needed for labels/titles)
- The rendered `.docx` (primary) + `.html` is the deliverable
- `flextable` is used for all Word-compatible formatted tables
- Narrative text is pulled from `$text$narrative` — never written inline in the `.qmd`

### Driver scripts
- `run/run_state.R` — **the runner**: it builds the results list for each requested line,
  saves the RDS, writes the xlsx, and renders the report. Edit the user-settings block
  at the top, then source the file. It sources `states/_config.R` automatically.
  Settings: `STATE`, `ANALYSES` (any of `"trend"`, `"enrollment"`, `"cohort"`,
  `"anomaly"`, `"aggregate"`), `RENDER`, `MIN_N`, `REPORT_YEARS`, `RE_EXPORT_XLSX`,
  `RE_EXPORT_LINES`.
- Demo walkthroughs: `inst/examples/workshop_anomaly_analysis.R` and
  `inst/examples/workshop_general_trend_analysis.R`. These are installed with the
  package, so they must stay portable: use `library(ReportR)` and `system.file()`,
  never a machine-specific path.

**Folder roles:** `run/` holds scripts that build results lists; `internal/` holds
internal and archived material. Both are tracked in git but excluded from the package
build via `.Rbuildignore`. Only `inst/` is installed. Never put a script with a
machine-specific path under `inst/`. `internal/workshop_archive/` holds the old per-line
`MS_workshop_*.R` drivers; don't extend them — add new behavior to `run/run_state.R`.

### RDS + render pattern

Save RDS directly to the output directory. Copy the QMD there before rendering
(to keep Quarto's intermediate `_files/` folder out of the package source tree),
then remove it afterwards:

```r
# Save RDS directly
saveRDS(results, file.path(output_dir, "results.rds"))

# Copy QMD, render, clean up
qmd_local <- file.path(output_dir, "Report.qmd")
file.copy(qmd_src, qmd_local, overwrite = TRUE)
quarto::quarto_render(input = qmd_local, ...,
  execute_params = list(results_file = file.path(output_dir, "results.rds")))
file.remove(qmd_local)
```

`run_state.R`'s `.render_report()` copies the template from the **source tree**
(`file.path(pkg_path, "inst", "templates", qmd_name)`) and falls back to
`system.file()` only if that is missing, so template edits take effect without
reinstalling the package.

### Excel export

`export_tables_to_excel()` (`R/export_tables.R`) writes **every non-empty data frame**
in a results list to its own sheet; plots and atomic values are skipped. Sheet names
are the slot path joined with `.` (e.g. `loss_hoss.table`), made to fit Excel's
31-character limit by `.shorten_sheet_name()`:
1. abbreviate known segments via the `.seg_abbr` lookup
   (e.g. `achievement_transitions` → `ach_trans`, `transitions` → `trans`);
2. replace characters Excel does not allow with `_`;
3. if still too long, trim the longest segment, so every path level stays visible.

`.dedupe_sheet_names()` then makes names unique ignoring case (`_2`, `_3`, ...).
**When you add a results slot with a long name, add an entry to `.seg_abbr`** so its
tab name stays readable.

`run_state.R` calls `export_tables_to_excel()` for each line and writes
`<line>_full.xlsx` (e.g. `general_trend_full.xlsx`) next to its RDS.

**Re-export without re-running analysis:** set `RE_EXPORT_XLSX <- TRUE` and
`RE_EXPORT_LINES` to the line(s) wanted, then source the file. `run_state()` is
skipped; the existing RDS files in the state's output directory are read and the
xlsx files rewritten. The cohort RDS lives at `cohort/cohort_results.rds`.

---

## 4. QMD Params Pattern

```yaml
params:
  pkg_path:     null   # absolute path to package root (for devtools::load_all)
  spec_file:    null   # path to spec file, relative to pkg_path
  results_file: null   # absolute path to pre-computed RDS
  min_n:        10
  report_years: null   # years shown in the main body; NULL = all years
```

**`report_years`** restricts the years shown in the report's main sections; templates
that use it note the historical years omitted. `run_state.R` passes `REPORT_YEARS`
through to every render.

**`pkg_path`** is a development-only workaround — `devtools::load_all(pkg_path)` needs
to find `DESCRIPTION`. Once the package is installed by end users, the setup chunk
becomes `library(ReportR)` and `pkg_path` is no longer needed.

**`spec_file`** is relative to `pkg_path` (e.g. `"inst/specs/00_MS_Assessment_Spec.R"`).
With an installed package, users pass the output of `system.file(...)` directly.

**`results_file`** is an absolute path. Driver scripts pass the path directly in `output_dir` — no intermediate tempdir is used.

---

## 5. The `assessment_spec` Object

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

  # 4. Scale scores — NESTED structure (changed from flat in prior versions)
  #    Span-tested: scale_scores$SUBJECT$GRADE (character grade key)
  #    EOC:         scale_scores$SUBJECT        (no grade nesting)
  scale_scores = list(
    ELA = list(
      `3` = list(loss = 301, hoss = 399, cuts = c(334, 349, 364, 386)),
      `4` = list(loss = 401, hoss = 499, cuts = c(428, 449, 464, 487))
      # ... remaining grades follow same pattern
    ),
    ENG_II = list(loss = 1001, hoss = 1099, cuts = c(1036, 1049, 1064, 1080)),
    ALG_I  = list(loss = 1001, hoss = 1099, cuts = c(1038, 1049, 1064, 1087))
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
- **Scale score structure is NESTED** — `scale_scores$ELA$`3`` for span-tested,
  `scale_scores$ENG_II` for EOC. The OLD flat structure (`ELA_3 = list(...)`) is no
  longer used. Any code that builds keys like `paste(subj, grade, sep="_")` is wrong.
- **EOC subjects match on SUBJECT alone.** Subjects declared `grades = NULL` have a
  single ungraded `scale_scores` entry, but state data routinely reports a real tested
  grade on EOC records (MS Algebra I spans grades 7–12). **Never** join a spec lookup to
  data on `SUBJECT` + `GRADE` directly — every EOC row misses and silently gets
  `LOSS`/`HOSS` = `NA`. Use `join_score_bounds(data, assessment_spec, cuts = )` in
  `spec_helpers.R`, which joins span-tested subjects on `SUBJECT` + `GRADE`, EOC subjects
  on `SUBJECT`, and warns naming any cell the spec does not cover. For a single row,
  `get_score_spec()` applies the same rule and ignores `grade` for EOC subjects.
  `build_loss_hoss_lookup()` still emits `GRADE = NA` for EOC — it is the raw table
  behind `join_score_bounds()`, not something to join directly.
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

## 6. Data Contract: `student_results_long`

After calling `standardize_student_results()`, the expected column names are:

| Column             | Type      | Notes                                          |
|--------------------|-----------|------------------------------------------------|
| `STUDENT_ID`       | character | Required for correlation and transition analysis |
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

**Critical:** Always standardize before filtering by year. `sgpData_LONG` uses
`YYYY_YYYY` strings — filtering by integer year before `standardize_student_results()`
will return 0 rows. Correct order:

```r
data |> filter(VALID_CASE == "VALID_CASE") |> standardize_student_results() |> filter(YEAR %in% 2016:2019)
```

**Renamed columns no longer exist after standardizing.** `standardize_student_results()`
renames `CONTENT_AREA` → `SUBJECT` (and `SCHOOL_NUMBER`/`DISTRICT_NUMBER` →
`SCHOOL_ID`/`DISTRICT_ID`). Any recoding or filtering in a state `_setup.R` that runs
after the call must use the new names. `df$CONTENT_AREA` silently returns `NULL`, so a
filter like `df[toupper(df$CONTENT_AREA) != "SCIENCE", ]` drops **every row** and
later fails far downstream (e.g. a `facet_wrap` crash at report render time).

---

## 7. Line 4: Score Quality Analysis

Line 4 is the most complete module and is the canonical template for all other lines.
The orchestrator is `run_anomaly_analysis()` in `R/run_anomaly_analysis.R`.

**Important orchestrator behavior:** `run_anomaly_analysis()` runs all five sub-analyses
in order, then regenerates `anomaly_results$loss_hoss$plot_distributions` with the
distribution stats from `anomaly_results$distributions$table` included. `run_loss_hoss()`
itself returns `plot_distributions = NULL` — the orchestrator fills it in.

### 4.1 `anomaly_results$loss_hoss` — HOSS/LOSS Pileups (`R/04_anomaly_loss_hoss.R`)

**Functions:** `compute_loss_hoss_table()`, `plot_loss_hoss_cleveland()`,
`plot_loss_hoss_heatmap()`, `plot_loss_hoss_over_time()`, `summarize_loss_hoss_findings()`,
`plot_loss_hoss_distributions()`, `run_loss_hoss()`

**`run_loss_hoss()` returns:**
- `$table` — pct near HOSS/LOSS by YEAR × SUBJECT × GRADE, with delta columns and severity flags
- `$plot_hoss_cleveland`, `$plot_loss_cleveland` — Cleveland dot plots
- `$plot_hoss_heat`, `$plot_loss_heat` — heatmaps
- `$plot_hoss_line`, `$plot_loss_line` — trend lines
- `$plot_distributions` — `NULL` from `run_loss_hoss()`; regenerated by orchestrator
- `$text` — `$narrative`, `$by_subject`, `$table`

**`plot_loss_hoss_distributions()` signature:**
```r
plot_loss_hoss_distributions(student_results_long, assessment_spec, loss_hoss_table,
                              distribution_table = NULL, point_adjust = 5,
                              x_margin = 10)
```
Uses a package-level helper `.build_lh_density_plot()` and pre-computes kernel density
statistics (512 points per year) to avoid storing raw student data in ggplot objects.

### 4.2–4.5: Score Integrity, Outliers, Correlations, Distributions

See the sub-analysis files for full details. Key patterns:
- `$score_integrity$invalid_records` and `$mismatch_records` store individual student records
- Correlations are cohort-style (Grade G → Grade G+1); requires `STUDENT_ID`
- Distribution flags (one per cell, in this priority): Bimodal (Sarle's BC > 0.555),
  Skewed (|skewness| > 1.0), Flat (excess kurtosis < -1.0; uniform = -1.2, normal = 0).
  Flat used to be "SD < 0.25 × (HOSS − LOSS)", which is true of nearly every real
  distribution (normal SD ≈ range/6) and flagged almost every cell — don't reintroduce it.
- `$distributions$plot$density_flagged` uses `.build_flagged_density_plot()` helper
  (pre-computed density, no raw data in ggplot)

---

## 8. Line 1: General Trend Analysis

The orchestrator is `run_general_trend_analysis()` in `R/run_general_trend_analysis.R`.

```r
run_general_trend_analysis(
  student_results_long,
  assessment_spec,
  report_years         = NULL,
  min_n                = 10,
  score_flat_threshold = 1.0,   # scale score points; changes smaller than this = "relatively flat"
  prof_flat_threshold  = 1.0,   # percentage points
  flag_sd              = 1.5    # SD threshold for transition flagging
)
```

Returns `general_trend_results` with sub-lists:

### `$score_trends` (`R/01_trend_overall.R`)
- `$table` — wide table: SUBJECT × GRADE rows, `<YEAR>_Mean`, `<YEAR>_SD`, `<YEAR>_N`
  columns grouped by stat (all Means, then all SDs, then all Ns)
- `$plot$mean_trend`, `$plot$mean_sd`, `$plot$median_iqr`, `$plot$grade_profile` —
  each is a **named list** keyed by SUBJECT (one ggplot per subject, faceted by grade).
  Y-axis per grade panel anchored to that grade's LOSS/HOSS via `geom_blank` + `scales="free_y"`.
- `$text$narrative`, `$text$by_subject`, `$text$table`

### `$proficiency` (`R/01_trend_proficiency.R`)
- `$table` — long: YEAR × SUBJECT × GRADE, `n`, `n_proficient`, `pct_proficient`
- `$plot$trend`, `$plot$heatmap` — both `NULL` when every `pct_proficient` is `NA`
- `$text$narrative`, `$text$by_subject`, `$text$table`

### `$distributions` (`R/01_trend_distributions.R`)
- `$plot$density` — named list keyed by SUBJECT. Each plot is a `patchwork` arrangement
  of per-grade ggplots (2 columns), x-axis set to each grade's LOSS/HOSS via
  `coord_cartesian`. Requires `patchwork` package (in Suggests). Fallback to faceted
  plot if not installed.

### `$achievement_transitions` (`R/01_trend_transitions.R`)
- `$matrices$long_table`, `$matrices$summary_table`, `$matrices$avg_matrices`
- `$matrices$plot$heatmap` — named list by SUBJECT
- `$matrices$plot$alluvial` — named list by SUBJECT_Grade (requires `ggalluvial`)
- `$matrices$plot$rates` — single dot chart of transition rates
- `$flags$table`, `$flags$narrative` — normative z-score flags (threshold: `flag_sd`)
- `$comparisons$correlations`, `$comparisons$divergences` — link to score/proficiency trends

**Transition matching is cohort-style**: Grade G in year T → Grade G+1 in year T+1 (same
as correlations). EOC subjects matched on STUDENT_ID + SUBJECT only.

Transitions need at least two years with non-NA `STUDENT_ID`; otherwise
`compute_transitions()` returns `long_table`, `summary_table`, and `avg_matrices` as `NULL` and
emits a message reporting how many years had matched records. `$matrices$summary_table` always carries `stable`, `up`, and
`down` columns (filled with 0 when a direction never occurs).

---

## 8b. Line 1.2: Enrollment Trends

The orchestrator is `run_general_enrollment_analysis()` in
`R/run_general_enrollment_analysis.R`. It calls `run_enrollment_analysis()` (in
`R/01_trend_enrollment.R`) which runs all sub-analyses and returns
`general_enrollment_results`.

```r
run_general_enrollment_analysis(
  student_results_long,
  assessment_spec,
  report_years  = NULL,
  subgroup_vars = NULL,   # NULL = all from assessment_spec$demographics
  min_n         = 10
)
```

**Column name resolution:** All enrollment functions accept both raw SGP names
(`SCHOOL_NUMBER`, `DISTRICT_NUMBER`) and standardized names (`SCHOOL_ID`, `DISTRICT_ID`).
The pattern `intersect(c("SCHOOL_NUMBER", "SCHOOL_ID"), names(data))[1]` is used throughout.
Raw names take priority so functions work without `standardize_student_results()`.

### Return structure (`general_enrollment_results`)

```
$counts$overall              — total unique student-year records
$counts$by_year              — N and year-over-year change by year
$counts$by_year_grade        — by year × grade
$counts$by_year_subject      — by year × subject
$counts$by_subgroup          — by year × all subgroup vars
$counts$by_school            — by year × SCHOOL_NUMBER (if available)
$counts$by_district          — by year × DISTRICT_NUMBER (if available)
$proportions$by_subgroup     — subgroup % by year (long: subgroup_var, subgroup_val, N, pct)
$proportions$by_year_grade   — subgroup % within grade
$proportions$by_school_subgroup — subgroup % within school
$transitions[[sg]]           — per subgroup var: $table and $summary (pct_change)
$entry_exit$entering         — performance of Entering students
$entry_exit$exiting          — performance of Exiting students
$entry_exit$continuing       — performance of Continuing students
$aggregate$school            — list: $totals, $by_subgroup (school-level)
$aggregate$district          — list: $totals, $by_subgroup (district-level)
$score_enrollment_trends     — YEAR × SUBJECT × GRADE: N_change, delta_mean, year_pair
$plot$enroll_vs_score        — scatter: enrollment change vs. mean score change
$text$narrative              — 4-sentence policy summary (auto-generated)
$params                      — min_n, report_years, subgroup_vars
```

**Entry/exit group labels** are always Title Case: `"Entering"`, `"Continuing"`,
`"Exiting"`. These are set inside `compute_entry_exit_performance()`.

**`weighted.mean` ordering in `summarize()`:** Always compute the weighted stat
*before* the aggregated weight column within the same `summarize()` call. Placing the
weight column first causes dplyr to resolve `.data[["N"]]` to the scalar sum instead
of the per-row vector, producing a length-mismatch error.

```r
# CORRECT — pct_proficient first, N second
dplyr::summarize(
  pct_proficient = round(stats::weighted.mean(.data[["pct_proficient"]],
                                               .data[["N"]], na.rm = TRUE), 1),
  N = sum(.data[["N"]], na.rm = TRUE),
  .groups = "drop"
)
```

---

## 8c. Line 1.3: General Cohort Analysis

The orchestrator is `run_general_cohort()` in `R/run_general_cohort.R`. It
wraps `run_cohort_analysis()` (in `R/01_general_cohort.R`) with RDS saving and
report rendering. This is a **standalone line**, parallel to Line 1 and Line 4
— not nested within either.

**Key difference from Line 1:** Line 1 tracks population-level trends; Line 1.3
follows the **same students** (cohorts) across grades using persistent `STUDENT_ID`.
When `STUDENT_ID`s are not persistent across years, all STUDENT_ID-dependent
functions return 0-row data frames without error. (The SGPdata demo does have
persistent IDs, so every cohort analysis runs on it.)

```r
run_general_cohort(
  student_results_long,
  assessment_spec,
  output_dir,
  report_years  = NULL,
  min_n         = 10,
  render_report = TRUE,
  open_report   = FALSE
)
```

**Important:** Set `attr(assessment_spec, "spec_file") <- "/abs/path/to/spec.R"`
before calling `run_general_cohort()` so the QMD can source the spec internally.

### Return structure (`cohort_results`)

```
$summary            — SUBJECT × COHORT_YEAR × GRADE × YEAR stats
                      (N, MEAN_SCORE, SD_SCORE, MEDIAN_SCORE, Q1_SCORE,
                       Q3_SCORE, PCT_PROFICIENT, N_<Level> per level)
$demographics       — long format: DEMOGRAPHIC × GROUP × cell
$transitions        — FROM_LEVEL × TO_LEVEL proportions across adjacent grades
$rate_of_change     — Improving/Stable/Declining trajectory proportions
$correlations       — Pearson r across adjacent grades (GRADE_X, GRADE_Y, R, P_VALUE)
$crosssectional     — list: $cohort (same as $summary) and $crosssectional
                      (all-student grade × year snapshot)
$mobility           — Entering/Continuing/Exiting by FROM_GRADE × TO_GRADE
$mobility_by_achiev — mobility crossed with ACHIEVEMENT_LEVEL
$persistence        — N_ORIGINAL, N_ENTERING, N_TOTAL, PCT_RETAINED by cohort
$enrollment_diag    — list: $table (Z_SCORE, FLAGGED), $narrative
$plot               — named lists by SUBJECT for each plot type
$text               — $narrative, $by_subject, $table
```

### Cohort derivation

`COHORT_YEAR = YEAR - (GRADE - cohort_anchor_grade)`. Computed by `.derive_cohort()`
internal helper. EOC records (GRADE = NA) get COHORT_YEAR = NA and are excluded.
Always use `assessment_spec$years$cohort_anchor_grade` — never hardcode `3`.

### Mobility labels

`MOBILITY_STATUS` values are always title-case: `"Entering"`, `"Continuing"`,
`"Exiting"`. Factor level order is always `c("Entering", "Continuing", "Exiting")`.
Color palette: Entering = `"#2ca02c"`, Continuing = `"#aec7e8"`, Exiting = `"#d62728"`.

---

## 8d. Line 5: Aggregate (School and District) Analysis

The orchestrator is `run_aggregate_analysis()` in `R/run_aggregate_analysis.R`.
Helper functions live in `R/aggregate_helpers.R`.

```r
run_aggregate_analysis(
  student_results_long,
  assessment_spec,
  school_results_long        = NULL,   # not yet available; enables 5.2 and 5.7
  report_years               = NULL,
  min_n_school               = 10,
  min_n_subgroup             = 10,
  improvement_threshold      = 3,      # pp change to classify improving/declining
  outlier_sd_threshold       = 2,
  outlier_n_funnel           = 20,
  correlation_flag_threshold = 0.85
)
```

### Sub-analyses and return structure

| Sub-analysis | Slot | Data required | Status |
|---|---|---|---|
| 5.1 Performance trends | `$performance_trends` | `student_results_long` | ✅ runs |
| 5.2 Accountability grades | `$accountability` | `school_results_long` | NULL — deferred |
| 5.3 Population shifts | `$population_shifts` | `student_results_long` (co-op/locale deferred) | ✅ partial |
| 5.4 Outlier identification | `$outliers` | `student_results_long` | ✅ runs |
| 5.4 Outlier characterization | `$outliers$outlier_characteristics` | `school_results_long` | NULL — deferred |
| 5.5 Disaggregated gaps | `$disaggregated` | `student_results_long` | ✅ runs |
| 5.6 Stability | `$stability` | `performance_data` (from 5.1) | ✅ runs |
| 5.7 Intervention analysis | `$intervention` | `school_results_long` | NULL — deferred |

**Key patterns in `aggregate_helpers.R`:**
- `.resolve_agg_cols(data)` — resolves school_id, district_id, subject column names
  (tries `SCHOOL_NUMBER`/`DISTRICT_NUMBER` before `SCHOOL_ID`/`DISTRICT_ID`)
- `.add_proficient(data, spec)` — derives `PROFICIENT` column from `ACHIEVEMENT_LEVEL`
- ICC computation uses `lme4::lmer()` (Suggests); returns empty data frame if not installed
- `compute_stability_analysis()` takes `performance_data` (already computed), not raw data
- `compute_outlier_schools()` reads demographic proportions from
  `performance_data$school_level_summary` (populated by `compute_performance_trends()`)

**`school_results_long`** is a planned data source (not yet available) containing
`ACCOUNTABILITY_GRADE`, `ACCOUNTABILITY_INDEX`, `INDICATOR_*`, `HQIM_FLAG`,
`LITERACY_COACH_FLAG`, `CO_OP`, `LOCALE`, `SCHOOL_LEVEL`, `SCHOOL_TYPE`.
When it becomes available, implement `compute_accountability_distributions()` and
`compute_intervention_analysis()` and pass `school_results_long` to
`run_aggregate_analysis()`.

---

## 9. Report Structure

### `inst/templates/04_Anomaly_Report.qmd`
Titled **"Section 4. Score Quality Analysis"**. Sections:
1. Introduction
2. Score Distribution Patterns (HOSS/LOSS + shape diagnostics)
3. Score Integrity
4. Year-to-Year Outliers
5. Year-to-Year Correlations
6. Distribution Diagnostics

### `inst/templates/01_General_Trend_Report.qmd`
Titled **"Section 1. General Trend Analysis"**. Sections:
1. Introduction
2. Scale Score Trends (mean table, change table, mean_trend plots, mean_sd plots)
3. Proficiency Trends (pct proficient table, change table, trend plot, heatmap)
4. Scale Score Distributions (density plots per subject)

### `inst/templates/01_General_Cohort_Report.qmd`
Titled **"Section 1.3. Cohort Composition and Performance"**. Sections:
1. Sample Sizes and Cohort Coverage → 1b. Enrollment Diagnostics → 1c. Persistence → 1d. Mobility
2. Mean Scale Scores by Cohort
3. Proficiency Rates by Cohort
4. Score Distributions by Cohort
5. Achievement Level Transitions (Sankey, guarded)
6. Rates of Change Across Cohorts (guarded)
7. Demographic Composition
8. Cohort vs. Cross-Sectional Comparison
9. Score Correlations (guarded)
Appendix: Full Summary Tables

All STUDENT_ID-dependent sections wrapped in `nrow() > 0` guards for data without
persistent student IDs.

### `inst/templates/01_Enrollment_Trends_Report.qmd`
Titled **"Section 1.2. Enrollment Trends"**. All tables use years across the top
(pivot pattern from `01_General_Trend_Report.qmd`). Key sections:
1. Total Enrollment Over Time (overall, by grade, by subject)
2. Subgroup Composition (N, Pct, dN, dPct per year; year-over-year transition table)
3. School-Level Enrollment Patterns (distribution of year-to-year changes; boxplot)
4. District-Level Enrollment Patterns
5. Enrollment and Score Trends (scatter: N_change vs. delta_mean by subject/grade)
6. Entering, Continuing, and Exiting Students

### `inst/templates/01_Enrollment_Brief.qmd`
One-page brief: key figures, top 3 notable shifts, one Sankey diagram (most
policy-relevant subgroup auto-selected).

### `inst/templates/05_Aggregate_Analysis_Report.qmd`
Titled **"Section 5. School and District Analysis"**. Renders only from
`student_results_long`; sections requiring `school_results_long` show a pending notice.
Key sections:
1. School Performance Trends (improvement classification, proficiency boxplots, slope histogram)
2. Demographic Composition and Population Shifts
3. Outlier Schools (funnel plot, above/below tables, stability)
4. Within-School Achievement Gaps (gap trend classification, gap boxplots)
5. Rank-Order Stability (ICC trends)
6. District-Level Summary
7. Sections Pending school_results_long (placeholder)

**QMD robustness rule:** Every `facet_wrap` call must be preceded by a row-count guard
on the filtered data. `facet_wrap` errors with "Faceting variables must have at least one
value" when the input data frame has zero rows. Pattern:

```r
plot_data <- raw_data |> dplyr::filter(!is.na(.data[["col"]]))
if (nrow(plot_data) > 0) {
  ggplot2::ggplot(plot_data, ...) + ggplot2::facet_wrap(...)
} else {
  cat("*No data available.*\n")
}
```

**Stored plots may be `NULL`.** Plot functions return `NULL` (or a list containing
`NULL`s) when their input data is empty. Every chunk that prints a plot from the
results object must guard it:

```r
p <- general_trend_results$proficiency$plot$trend
if (!is.null(p)) print(p) else cat("*Proficiency trend data not available.*\n")
```

**Key QMD conventions for all templates:**
- Setup chunk: conditional `devtools::load_all(pkg_path)` vs `library(ReportR)`
- Subsequent chunks read only from the pre-computed results object
- `flextable` for all tables; `merge_v()` for Subject/Grade columns where appropriate
- `fig-dpi: 300` in docx format

---

## 9b. Documentation Website

The package site is built with **pkgdown** and published by GitHub Pages at
**https://ndadey.github.io/ReportR/** from the `gh-pages` branch.

| Page | Source |
|---|---|
| Home | `pkgdown/index.md` (the site's home; `README.md` stays the GitHub landing page) |
| Walkthrough | `vignettes/articles/walkthrough.Rmd` — a pkgdown *article*, not a package vignette, so it never runs during `R CMD check` |
| Reference | `man/*.Rd`, grouped by line of analysis in `_pkgdown.yml` |
| Styles | `pkgdown/extra.css` (`.rr-flow` step cards, `.rr-narrative` boxes, `.rr-scroll` wide tables) |

**Deployment:** `.github/workflows/pkgdown.yaml` runs on every push to `main`: it
installs the package and its Suggests, builds the site, and pushes `docs/` to
`gh-pages`. Never commit `docs/` (it is gitignored).

**Rules for the site:**
- **Every new exported function must be added to `reference:` in `_pkgdown.yml`**, or
  the build fails ("topics missing from index").
- **pkgdown publishes every `.md` file in the repo root.** The workflow deletes all root
  `.md` files except README, NEWS, and LICENSE before building, which keeps
  `CLAUDE.md` and `ARCHITECTURE.md` off the site. Keep that step.
- The walkthrough executes real code on `sgpData_LONG` in CI (about 3–4 minutes). If
  it errors, the deploy fails and the previous site stays up.
- Articles are referenced as `articles/<name>` in `_pkgdown.yml`.

**Building locally (Windows):**
- Set `RSTUDIO_PANDOC` to Quarto's bundled pandoc
  (`.../Quarto/bin/tools`) when running outside RStudio.
- Run `devtools::document()` and `pkgdown::build_site()` in **separate** R sessions.
  If the package is already loaded, pkgdown cannot install the fresh copy and silently
  falls back to the installed (possibly stale) version.
- pkgdown fails with "Total path length must be less than PATH_MAX: 260" when the
  source path is long. Build from a short path if needed.
- A local build includes `CLAUDE.html` and `ARCHITECTURE.html`; only the CI build
  removes them.

---

## 10. File and Package Structure

```
ReportR/
├── DESCRIPTION
├── NAMESPACE
├── README.md
├── CLAUDE.md
├── ARCHITECTURE.md                        ← stateR package-family plan (see Section 1)
├── .Rbuildignore                          ← excludes dev/, MS/, run/, internal/, states/,
│                                            internal_docs/, ARCHITECTURE.md, CLAUDE.md,
│                                            .claude/, Word temp files. A built package
│                                            contains only DESCRIPTION, LICENSE, NAMESPACE,
│                                            R/, README.md, inst/, man/, tests/
├── .gitignore                             ← excludes dev/, MS/, states/* (except DEMO + template),
│                                            *.docx, *.xlsx, *.pdf, *.rds, *.csv, *.RData,
│                                            *.html, *_files/ (rendered Quarto output)
├── R/
│   ├── setup_format_data.R               ← standardize_student_results()
│   ├── student_results_long.R            ← roxygen doc page for the data contract
│   ├── utils.R                           ← grade_key()
│   ├── spec_helpers.R                    ← get_score_spec(), build_loss_hoss_lookup(),
│   │                                        benchmark_index(), join_score_bounds()
│   ├── export_tables.R                   ← export_tables_to_excel()
│   ├── globals.R                         ← globalVariables() + @importFrom rlang .data :=
│   ├── run_anomaly_analysis.R            ← Line 4 orchestrator
│   ├── 04_anomaly_loss_hoss.R            ← loss_hoss sub-analysis ✅
│   ├── 04_anomaly_score_integrity.R      ← score_integrity sub-analysis ✅
│   ├── 04_anomaly_outliers.R             ← outliers sub-analysis ✅
│   ├── 04_anomaly_correlations.R         ← correlations sub-analysis ✅
│   ├── 04_anomaly_distributions.R        ← distributions sub-analysis ✅
│   ├── run_general_trend_analysis.R      ← Line 1 orchestrator
│   ├── 01_trend_overall.R               ← score_trends sub-analysis ✅
│   ├── 01_trend_proficiency.R           ← proficiency sub-analysis ✅
│   ├── 01_trend_distributions.R         ← distributions sub-analysis ✅
│   ├── 01_trend_transitions.R           ← achievement_transitions sub-analysis ✅
│   ├── run_general_cohort.R             ← Line 1.3 orchestrator ✅
│   ├── 01_general_cohort.R             ← all Line 1.3 cohort functions ✅
│   ├── utils_enrollment.R              ← suppress_small_n_enroll() + Step 1 comment ✅
│   ├── 01_trend_enrollment.R           ← all Line 1.2 enrollment functions ✅
│   ├── run_general_enrollment_analysis.R ← Line 1.2 orchestrator ✅
│   ├── aggregate_helpers.R             ← all Line 5 compute_* functions ✅
│   └── run_aggregate_analysis.R        ← Line 5 orchestrator ✅
│
├── inst/
│   ├── specs/
│   │   ├── 00_MS_Assessment_Spec.R       ← MS MAAP (5 levels, nested scale_scores)
│   │   ├── 00_AR_Assessment_Spec.R       ← Arkansas ATLAS (4 levels, nested scale_scores)
│   │   ├── 00_DEMO_Assessment_Spec.R     ← SGPdata demo (4 levels, nested scale_scores)
│   │   └── 00_TEMPLATE_Spec.R            ← blank template
│   ├── templates/
│   │   ├── 04_Anomaly_Report.qmd         ← Line 4 report ✅
│   │   ├── 01_General_Trend_Report.qmd   ← Line 1 report ✅
│   │   ├── 01_General_Cohort_Report.qmd  ← Line 1.3 report ✅
│   │   ├── 01_Enrollment_Trends_Report.qmd ← Line 1.2 report ✅
│   │   ├── 01_Enrollment_Brief.qmd       ← Line 1.2 one-page brief ✅
│   │   ├── 05_Aggregate_Analysis_Report.qmd ← Line 5 report ✅
│   │   └── old/                          ← archived templates (not used by runners)
│   └── examples/                         ← installed; must stay portable
│       ├── workshop_anomaly_analysis.R   ← Line 4 demo data walkthrough
│       └── workshop_general_trend_analysis.R ← Line 1 demo data walkthrough
│
├── run/                                  ← in git, NOT installed (.Rbuildignore)
│   └── run_state.R                       ← builds results lists, RDS, xlsx, reports ✅
│
├── internal/                             ← in git, NOT installed (.Rbuildignore)
│   └── workshop_archive/                 ← superseded MS_workshop_*.R drivers, dated
│                                            snapshots, Old/ — reference only
│
├── internal_docs/                        ← module build notes (NOTES-*.md); NOT installed
│
├── states/                               ← NOT installed (.Rbuildignore)
│   ├── _config_template.R            ← skeleton; copy to _config.R locally and fill in paths
│   ├── _config.R                     ← gitignored: maps state codes → spec/setup/output paths
│   ├── DEMO/_setup.R                 ← portable SGPdata loader (committed)
│   ├── MS/_setup.R                   ← gitignored: MS data loader + recoding
│   └── AR/_setup.R                   ← gitignored: AR data loader + recoding
│
├── _pkgdown.yml                          ← site config: navbar, reference index groups
├── pkgdown/                              ← site home page (index.md) + extra.css
├── vignettes/articles/walkthrough.Rmd    ← site-only article (Rbuildignored)
├── .github/workflows/pkgdown.yaml        ← builds + deploys the site to gh-pages
├── docs/                                 ← local site builds only (gitignored)
│
├── man/
├── tests/testthat/
│   ├── test-04_anomaly_loss_hoss.R
│   ├── test-01_trend_overall.R
│   ├── test-01_trend_enrollment.R        ← 59 tests for Line 1.2 functions ✅
│   ├── test-04_anomaly_distributions.R   ← distribution shape flags (synthetic shapes)
│   ├── test-export_tables.R              ← sheet naming, dedupe, Excel export
│   └── test-spec_helpers.R               ← get_score_spec(), build_loss_hoss_lookup(),
│                                            join_score_bounds() (EOC handling)
└── dev/                                  ← not in git or package (excluded by both)
```

---

## 11. Coding Conventions

### Style
- **Tidyverse throughout**: use `dplyr`, `tidyr`, `ggplot2`; pipe with `|>` (native pipe)
- No base R apply loops where a `dplyr` chain is cleaner
- Use `.data[["col"]]` pronoun in dplyr/ggplot — not bare names
- `stringsAsFactors = FALSE` always when constructing data frames manually
- Do **not** use `isTRUE()` on a vector column — use `!is.na(x) & x` instead
- Do **not** use `dplyr::if_else()` for scalar conditions inside loops — use base R `if/else`
- Do **not** use `dplyr::recode(!!!list)` — use `left_join()` against a lookup data frame
- After `tidyr::pivot_wider()`, do **not** assume a column exists for every category.
  `values_fill` only fills cells in columns that were created; a category absent from
  every row gets no column at all. Add expected columns explicitly before referencing them:

  ```r
  for (.col in c("stable", "up", "down")) {
    if (!.col %in% names(wide)) wide[[.col]] <- 0
  }
  ```

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

### ggplot objects and large data — CRITICAL performance rule

**Never create a ggplot object inside a function that has `student_results_long` in scope.**
ggplot2 `aes()` quosures capture their creation environment. If `student_results_long`
is in the enclosing environment chain, every ggplot object serialized to RDS will carry
the full student dataset, ballooning the RDS to gigabytes.

**The fix:** define a package-level helper function (outside any other function) that
builds the ggplot from pre-computed summary data:

```r
# CORRECT — package-level helper; no student_results_long in scope
.build_my_plot <- function(summary_df, scalar_args...) {
  ggplot2::ggplot(summary_df, ggplot2::aes(...)) + ...
}

# In the analysis function:
my_plot_function <- function(student_results_long, ...) {
  # Pre-compute: kernel density (512 pts), summary stats, etc.
  dens_df <- ...  # small data frame
  .build_my_plot(dens_df, ...)  # quosures capture .build_my_plot's env only
}
```

For density plots specifically: use `stats::density(x, n = 512L)` to pre-compute curve
data and pass the 512-row data frame to ggplot instead of raw student scores. Examples:
- `.build_lh_density_plot()` in `04_anomaly_loss_hoss.R`
- `.build_flagged_density_plot()` in `04_anomaly_distributions.R`

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

## 12. flextable for Word output
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

---

## 13. DESCRIPTION File (current state)

```
Package: ReportR
Title: Repeatable Analysis Pipeline for State Summative Assessment Data
Version: 0.1.0
Authors@R: person("Nathan", "Dadey", email = "ndadey@nciea.org", role = c("aut", "cre"))
URL: https://ndadey.github.io/ReportR/, https://github.com/ndadey/ReportR
BugReports: https://github.com/ndadey/ReportR/issues
Imports:
    dplyr (>= 1.1.0),
    tidyr (>= 1.3.0),
    ggplot2 (>= 3.4.0),
    rlang,
    stats,
    flextable
Suggests:
    testthat (>= 3.0.0),
    devtools,
    here,
    openxlsx,
    SGPdata,
    ggalluvial,
    patchwork,
    lme4,
    ggrepel
```

`flextable` is in `Imports` (used in package functions). `openxlsx` is in `Suggests`
(export_tables_to_excel). `patchwork` is in `Suggests` (density plot layout in Line 1).
`ggalluvial` is in `Suggests` (alluvial/Sankey plots in Lines 1 and 1.2).
`lme4` is in `Suggests` (ICC computation in Line 5 — `compute_performance_trends()`).
`ggrepel` is in `Suggests` (labeled scatter plots in workshop scripts).

---

## 14. Testing Expectations

Every `compute_*()` function must have at least one `testthat` test in
`tests/testthat/test-NN_module_subsection.R`.

### Demo data setup

```r
library(SGPdata)
library(dplyr)

demo_data <- sgpData_LONG |>
  filter(VALID_CASE == "VALID_CASE") |>
  standardize_student_results() |>
  filter(!is.na(SCALE_SCORE)) |>
  mutate(across(c(FRL, EL, IEP, GENDER), ~ sub("^.*: ", "", .x)))
```

**Important:** `sgpData_LONG` uses `YYYY_YYYY` year strings (e.g. `"2019_2020"`).
`standardize_student_results()` converts these to the end year integer automatically.
Always filter by year AFTER standardizing, not before.

**About the demo data (SGPdata 28.0.0.0):**
- READING and MATHEMATICS, grades 3–10, years 2020–2024, about 366,000 records after
  cleanup. Persistent `STUDENT_ID`s, 121 schools, 3 districts — **every line runs**,
  including cohort, transitions, correlations, and Line 5.
- Two cleanup steps are required (shown above): drop `"No Score"` records (no scale
  score), and strip the `"ELL: "`-style prefixes so binary flags read `Yes`/`No`
  (aggregate code counts `YES`/`Y`/`TRUE`/`1` as yes).
- The DEMO spec's LOSS/HOSS and cuts come from `SGP::SGPstateData[["DEMO"]]`, converted
  to ReportR's cut convention (SGP cut − 1). With the cleanup above, score integrity
  reports 0 out-of-range scores and 0 level mismatches.
- Line 4 distribution flags on the demo data: 21 of 80 cells Skewed (mostly reading),
  none Bimodal or Flat.

### Minimal synthetic data for correlation/transition testing (persistent IDs)

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

## 15. Getting Started (for Claude Code in a new session)

1. Read this file completely before touching any code.
2. Run `devtools::check()` first to confirm the baseline is clean (0 errors, 0 warnings,
   0 notes; see Section 2). Treat anything new as a regression.
3. Line 4 (Score Quality Analysis) is the canonical template — match its patterns exactly
   when building other modules.
4. Before adding any new NSE variable to a function, add it to `R/globals.R`.
5. The DEMO spec + `sgpData_LONG` is the primary runnable example. The MS spec is the
   reference for real-data structure. Always test new functions against both.
6. Do not use `isTRUE()` on a vector — use `!is.na(x) & x` for logical column filtering.
7. Do not use `dplyr::if_else()` for scalar conditions — use base R `if/else`.
8. Do not use `dplyr::recode(!!!list)` — use `left_join()` against a lookup data frame.
9. Do not edit files in `dev/` or `MS/` unless explicitly asked — both are excluded from
   git and the package build.
9b. **Per-state configuration:** `states/_config.R` (gitignored; copy from `states/_config_template.R`)
    maps state codes to spec path, setup script path, and output directory. `PKG_ROOT` is the only
    machine-specific path in the system. Per-state data-loading scripts (`states/{STATE}/_setup.R`)
    are also gitignored; `states/DEMO/_setup.R` is committed and portable.
10. Save RDS directly to the output directory. Copy the QMD to the output directory before
    rendering (keeps Quarto's `_files/` out of the package source tree), then `file.remove()`
    the copied QMD after rendering. See Section 3 for the pattern.
11. `assessment_spec$scale_scores` uses a **nested** structure — `$SUBJECT$GRADE` for
    span-tested, `$SUBJECT` for EOC. Never build flat `SUBJECT_GRADE` keys manually.
12. Never build ggplot objects inside functions that hold `student_results_long` in scope.
    Pre-compute summary data and delegate to a package-level helper. See Section 11.
13. Line 1.3 (cohort) is a **standalone line** with its own orchestrator `run_general_cohort()`.
    It is NOT nested inside `run_general_trend_analysis()`. Call it separately.
14. For Line 1.3: set `attr(assessment_spec, "spec_file") <- "/abs/path"` before calling
    `run_general_cohort()` so the QMD can source the spec. Without this, rendering is skipped.
15. `MOBILITY_STATUS` values are always title-case `"Entering"` / `"Continuing"` / `"Exiting"`.
    Factor level order is always `c("Entering", "Continuing", "Exiting")`.
16. The codebase uses **American English** throughout. Use `summarize()` not `summarise()`,
    `color` not `colour`, etc. `dplyr::summarize()` and `dplyr::summarise()` are exact
    aliases — both work, but `summarize()` is the project standard.
17. **School/district column name resolution:** The MS data uses `SCHOOL_NUMBER` and
    `DISTRICT_NUMBER` (raw SGP format); `standardize_student_results()` renames these to
    `SCHOOL_ID` and `DISTRICT_ID`. All aggregate functions must try both:
    `intersect(c("SCHOOL_NUMBER", "SCHOOL_ID"), names(data))[1]`. Never hard-code
    either form.
18. **`weighted.mean` in `summarize()`:** Always compute the weighted stat *before* the
    weight aggregation column in the same `summarize()` call. If `N = sum(N)` is computed
    first, `.data[["N"]]` in subsequent expressions resolves to the scalar, not the
    per-row vector, producing a length-mismatch error. See Section 8b.
19. Line 1.2 (enrollment) is a **standalone line** with its own orchestrator
    `run_general_enrollment_analysis()`. It accepts `subgroup_vars` to restrict which
    demographics are analyzed. Entry/exit group labels are Title Case: `"Entering"`,
    `"Continuing"`, `"Exiting"`.
20. Line 5 (aggregate) is also standalone. Sub-analyses requiring `school_results_long`
    are set to `NULL` with explanatory comments — do not implement them until the user
    confirms that data source is available. See Section 8d for the full list.
21. **Never join a spec lookup on `SUBJECT` + `GRADE`.** EOC subjects (`grades = NULL`)
    have one ungraded `scale_scores` entry, but real state data reports a tested grade on
    EOC records — MS Algebra I spans grades 7–12 — so such a join misses every EOC row
    and leaves `LOSS`/`HOSS` as `NA`, which silently disables the out-of-range, level-
    mismatch, and flat-distribution checks and crashes `stats::density(from = NA)`.
    Use `join_score_bounds()` (or `get_score_spec()` for one row). See Section 5.
22. **Use post-standardization column names in state setup scripts.** After
    `standardize_student_results()`, `CONTENT_AREA` is `SUBJECT` and
    `SCHOOL_NUMBER`/`DISTRICT_NUMBER` are `SCHOOL_ID`/`DISTRICT_ID`. Referencing the old
    name returns `NULL` and silently empties the data. See Section 6.
23. **Guard every stored plot in a QMD** with `if (!is.null(p))` — plot slots are `NULL`
    when there is no data. And guard every `facet_wrap` with a row-count check. See Section 9.
24. **After `pivot_wider()`, add any expected-but-missing category columns** before using
    them. See Section 11.
25. **New results slots with long names** need an abbreviation in `.seg_abbr` in
    `R/export_tables.R` so their xlsx tab names stay readable. See Section 3.
26. **Script placement:** scripts that build results lists go in `run/`; archived or
    internal scripts go in `internal/`. Only portable code goes in `inst/`, since it is
    installed with the package. See Section 3.
27. **Keep the DEMO spec in sync with SGPdata.** The site's walkthrough runs every line
    on `sgpData_LONG` with the DEMO spec at build time. If an SGPdata release changes
    subjects, grades, years, or levels, update the spec (values from
    `SGP::SGPstateData[["DEMO"]]`) and the walkthrough text. See Sections 9b and 14.
28. **Grade labels in plots must sort numerically.** `paste0("G", grade)` or
    `paste0("Grade ", grade)` as plain character sorts alphabetically (`G10` before
    `G3`), which scrambles axes, facets, and legends once grade 10+ is present. Make
    them factors with levels in numeric grade order (see `.grade_chr()` in
    `R/01_general_cohort.R`).
29. **Never put code between a roxygen block and its function.** roxygen attaches a
    `#'` block to the next object, so a helper inserted in between steals the docs and
    the `@export`: the helper gets exported and the real function silently does not
    (this happened to `plot_distributions()` and `plot_loss_hoss_distributions()`).
    Define `.build_*` helpers above the roxygen block.
30. **Only ASCII in R code.** Write non-ASCII characters in strings as escapes
    (`"→"` for →, `"–"` for –, `"±"` for ±); R CMD check warns
    otherwise. Comments and roxygen text may contain UTF-8.
