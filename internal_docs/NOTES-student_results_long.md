# `student_results_long` — Data Contract Reference

## What it is

`student_results_long` is the single required input data frame for all ReportR
analysis functions. It is a **long-format** table with **one row per student × year ×
subject**. All columns use canonical names defined below; raw state data must pass
through `standardize_student_results()` before being handed to any analysis function.

---

## Lineage: derived from the SGP long format

`student_results_long` inherits its structure directly from the **SGP long data format**
used by the [`SGPdata`](https://github.com/CenterForAssessment/SGPdata) and
[`SGP`](https://github.com/CenterForAssessment/SGP) packages. The SGP format is itself
the canonical long-format spine for longitudinal state assessment data in the Center for
Assessment ecosystem: one row per student × year × content area, with all years stacked
vertically so that growth models and cohort tracking can operate on a single table.

### What is the same

| SGP convention | ReportR treatment |
|---|---|
| Long format: one row per student × year × content area | **Identical.** ReportR adds a × subject dimension in name only. |
| `VALID_CASE` flag distinguishes usable records | Filtered out before standardizing (not a ReportR column) |
| `YEAR` as `"YYYY_YYYY"` academic-year string | Coerced to end-year integer by `standardize_student_results()` |
| `GRADE` as integer | **Identical.** `NA` for EOC subjects in both. |
| `SCALE_SCORE` | **Identical.** Same column name, same semantics. |
| `CONTENT_AREA` | Renamed to `SUBJECT`. Values must then be recoded to match `assessment_spec$subjects` keys. |
| `SCHOOL_NUMBER` / `DISTRICT_NUMBER` | Renamed to `SCHOOL_ID` / `DISTRICT_ID`. Both raw names are accepted throughout (fallback pattern). |

### What differs or is added

| Aspect | SGP long | `student_results_long` |
|---|---|---|
| **`ACHIEVEMENT_LEVEL`** | Not always present; when present may use program-specific codes (`"PL1"`, `"L1"`, etc.) | Required; must exactly match `assessment_spec$achievement_levels$labels` |
| **`STUDENT_ID`** | Present as `ID` or `STUDENT_ID`; persistence varies by state | Renamed to `STUDENT_ID`; must be stable across years for cohort/correlation analyses |
| **Demographic columns** | Vary by state export; may use raw codes (`"1"` for race, `"Y"` for binary flags) | Canonical labels required; recoding is the setup script's job |
| **`VALID_CASE` column** | Present; used to filter unusable records | Filtered before calling `standardize_student_results()` — not carried forward |
| **`ACHIEVEMENT_PERCENTAGE_SCORE`** | Often present | Not used by ReportR |
| **SGP-specific columns** (`SGP`, `SGP_NORM_GROUP`, etc.) | Present in growth-model output | Ignored / dropped |

### The conversion path

```
Raw state export  →  SGP-format file (VALID_CASE, CONTENT_AREA, YEAR as YYYY_YYYY, ...)
      ↓
filter(VALID_CASE == "VALID_CASE")       # drop invalid records
      ↓
standardize_student_results()            # rename aliases, coerce YEAR/GRADE
      ↓
state setup recoding                     # recode SUBJECT, ACHIEVEMENT_LEVEL, demographics
      ↓
student_results_long                     # ready for all ReportR analysis functions
```

The demo data (`SGPdata::sgpData_LONG`) is a public SGP-format dataset and is the
primary runnable example. Its `STUDENT_ID`s are **not** persistent across grades in the
demo version, so cohort, correlation, and transition analyses return 0-row results —
this is expected and is handled gracefully by all `compute_*()` functions.

---

## Column contract

### Required columns (enforced by strictest `stopifnot` guards)

| Column | Type | Notes |
|---|---|---|
| `STUDENT_ID` | character | Stable across years. Required for cohort, correlation, and transition analyses. When not persistent, those sub-analyses return 0-row data frames without error. |
| `YEAR` | integer | Administration year (end year). `"YYYY_YYYY"` strings are auto-converted by `standardize_student_results()`. **Always standardize before filtering by year.** |
| `GRADE` | integer | Tested grade. `NA` for EOC subjects. |
| `SUBJECT` | character | Matches keys in `assessment_spec$subjects` (e.g. `"ELA"`, `"MATH"`, `"ALG_I"`). Must match exactly — no partial matching. |
| `SCALE_SCORE` | numeric | Student scale score on the assessment program's reported scale. |
| `ACHIEVEMENT_LEVEL` | character | Level label exactly matching `assessment_spec$achievement_levels$labels`. |

### Expected columns (in contract, not always runtime-enforced)

| Column | Type | Notes |
|---|---|---|
| `SCHOOL_ID` | character | School identifier. Used by Line 5 (aggregate) and enrollment analyses. |
| `SCHOOL_NAME` | character | School display name. |
| `DISTRICT_ID` | character | District identifier. Used by Line 5. |
| `DISTRICT_NAME` | character | District display name. |
| `RACE_ETHNICITY` | character | Race/ethnicity category label. Categorical demographic (separate breakdown column per group). |
| `FRL` | character | Free/reduced lunch status. Binary demographic (`"Yes"` / `"No"` or similar). |
| `EL` | character | English Learner status. Binary demographic. |
| `IEP` | character | Individualized Education Program status. Binary demographic. |
| `GENDER` | character | Student gender. Categorical demographic. |

**Binary vs. categorical distinction:** `assessment_spec$demographics[[var]]$type` records this.
Binary vars (`FRL`, `EL`, `IEP`) produce a single `pct_*` column in subgroup summaries.
Categorical vars (`RACE_ETHNICITY`, `GENDER`) require a separate column per group value.

---

## How it is produced

Call `standardize_student_results()` on the raw data frame. The function:

1. **Renames** raw column aliases to canonical names (see alias map below).
2. **Coerces** `YEAR` to integer — `"YYYY_YYYY"` strings become the end year (e.g. `"2023_2024"` → `2024`).
3. **Coerces** `GRADE` to integer.
4. Leaves all other columns as-is.

```r
student_results_long <- raw_data |>
  dplyr::filter(.data[["VALID_CASE"]] == "VALID_CASE") |>  # SGPdata only
  standardize_student_results()
```

**Do not filter by year before standardizing.** SGPdata uses `"YYYY_YYYY"` strings; an
integer filter returns 0 rows:

```r
# WRONG — returns 0 rows with SGPdata
raw_data |> filter(YEAR %in% 2020:2024) |> standardize_student_results()

# CORRECT
raw_data |> standardize_student_results() |> filter(YEAR %in% 2020:2024)
```

---

## Raw column alias map

`standardize_student_results()` accepts these raw names and renames them to canonical:

| Canonical name | Accepted raw aliases |
|---|---|
| `STUDENT_ID` | `ID`, `STUDENT_ID_LONG` |
| `SUBJECT` | `CONTENT_AREA`, `CONTENT_AREA_ORIGINAL` |
| `SCALE_SCORE` | `SS`, `SCALE_SCORE_ORIGINAL` |
| `ACHIEVEMENT_LEVEL` | `PERFORMANCE_LEVEL`, `PROF_LEVEL` |
| `SCHOOL_ID` | `SCHOOL_NUMBER`, `SCHOOL_CODE` |
| `DISTRICT_ID` | `DISTRICT_NUMBER`, `DISTRICT_CODE` |
| `SCHOOL_NAME` | `SCHOOL_NAME_ORIGINAL` |
| `DISTRICT_NAME` | `DISTRICT_NAME_ORIGINAL` |
| `RACE_ETHNICITY` | `ETHNICITY`, `RACE` |
| `FRL` | `FREE_REDUCED_LUNCH_STATUS`, `FRL_STATUS` |
| `EL` | `ELL_STATUS`, `EL_STATUS`, `ELL` |
| `IEP` | `IEP_STATUS` |

Note: `YEAR`, `GRADE`, `GENDER` are not renamed — they must already use these names (or be
coerced from their type). State setup scripts (`states/{STATE}/_setup.R`) handle any
renames not covered by this map before calling `standardize_student_results()`.

---

## State-specific recoding

After standardizing, state setup scripts apply any additional munging required to match
the `assessment_spec` contract. Common examples:

**Subject renaming** (MS MAAP):
```r
student_results_long$SUBJECT[student_results_long$SUBJECT == "ALGEBRA I"]  <- "ALG_I"
student_results_long$SUBJECT[student_results_long$SUBJECT == "ENGLISH II"] <- "ENG_II"
```

**Achievement level relabeling** (MS MAAP raw `"PL1"` → `"Minimal"`, etc.):
```r
student_results_long$ACHIEVEMENT_LEVEL <- gsub("PL", "L", student_results_long$ACHIEVEMENT_LEVEL)
# ... then recode L1 → "Minimal", L2 → "Basic", etc.
```

These transforms live in `states/{STATE}/_setup.R` (gitignored for real states;
`states/DEMO/_setup.R` is committed as the portable reference).

---

## EOC subjects

EOC subjects (e.g. `ALG_I`, `ENG_II`, `ALG`, `GEO`, `BIO`) have `GRADE = NA`.
Functions that operate on grade spans (e.g. `compute_score_summary(by_grade = TRUE)`)
filter out NA-grade rows automatically. Cohort derivation excludes EOC records
(`COHORT_YEAR = NA` assigned to any row with `GRADE = NA`).

---

## School / district column fallback

Some raw SGP exports use `SCHOOL_NUMBER` / `DISTRICT_NUMBER` instead of `SCHOOL_ID` /
`DISTRICT_ID`. All aggregate functions resolve the column name with:

```r
school_col <- intersect(c("SCHOOL_NUMBER", "SCHOOL_ID"), names(data))[1]
```

Never hard-code either form in analysis functions.

---

## Minimum functional subsets by line of analysis

| Line | Minimum columns |
|---|---|
| Line 1 — General Trend | `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL` |
| Line 1.2 — Enrollment | `STUDENT_ID`, `YEAR` (+ `SCHOOL_ID` / `DISTRICT_ID` for school/district breakdowns) |
| Line 1.3 — Cohort | `STUDENT_ID`, `YEAR`, `GRADE`, `SUBJECT`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL` |
| Line 4 — Anomaly | `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL` (+ `STUDENT_ID` for correlations/transitions) |
| Line 5 — Aggregate | `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, `SCHOOL_ID`, `DISTRICT_ID` |

Demographic columns (`RACE_ETHNICITY`, `FRL`, `EL`, `IEP`, `GENDER`) unlock subgroup
disaggregation in Lines 1.2, 1.3, and 5. Missing demographic columns produce `NULL`
subgroup slots rather than errors.

---

## Where the schema is formally documented in code

| Location | What it specifies |
|---|---|
| `R/student_results_long.R` | Roxygen `@format` block — the 15-column canonical list (documentation, not runtime) |
| `R/setup_format_data.R` | `standardize_student_results()` — the rename + coerce map (production) |
| Per-function `stopifnot()` | Runtime column guards in each `compute_*()` — see `R/01_general_cohort.R` lines 340–347 and 1557–1562 for the strictest set |
