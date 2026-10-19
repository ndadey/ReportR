# ReportR

**Repeatable annual analyses of state summative assessment data.**

Every year, state assessment programs answer the same questions about
their results. How did performance change? Who was tested? Do the scores
behave as expected? Which schools stand out? ReportR packages those
analyses so a state can rerun them on each new year of data, and a new
state can adopt them, by changing configuration rather than code.

You supply two things: your student records in a standard long format,
and a short file describing your assessment. ReportR returns tables,
plots, and plain-language summaries for each line of analysis, ready to
drop into Word reports.

[Start the
walkthrough](https://ndadey.github.io/ReportR/articles/walkthrough.md)
[Browse the
functions](https://ndadey.github.io/ReportR/reference/index.md)

## How it works

1

### Set up

**Student records** in one long data frame, one row per student, year,
and subject.
[`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md)
renames common source columns to the names ReportR expects.

**An assessment spec**: one R file listing subjects, grades, score
ranges, cut scores, and achievement levels.

2

### Analyze

One function per line of analysis, such as
[`run_general_trend_analysis()`](https://ndadey.github.io/ReportR/reference/run_general_trend_analysis.md),
returns a **results list**: a table, plots, and narrative text for every
sub-analysis.

Save it once as an `.rds` file.

3

### Report

**Quarto templates** read the saved results and render Word reports,
with tables formatted for Word.

[`export_tables_to_excel()`](https://ndadey.github.io/ReportR/reference/export_tables_to_excel.md)
writes every table to a workbook.

Reports read the saved results instead of rerunning the analysis, so you
can revise a report’s layout and render it again in seconds.

## Lines of analysis

| Line | Question it answers | Run it with |
|----|----|----|
| **1. General trend** | How has performance changed over time? Mean scale scores, proficiency rates, score distributions, and achievement-level transitions. | [`run_general_trend_analysis()`](https://ndadey.github.io/ReportR/reference/run_general_trend_analysis.md) |
| **1.2 Enrollment** | Who was tested, and how has the tested population changed? Counts, subgroup composition, and students entering and leaving. | [`run_general_enrollment_analysis()`](https://ndadey.github.io/ReportR/reference/run_general_enrollment_analysis.md) |
| **1.3 Cohort** | How do the same students perform as they move through the grades? | [`run_general_cohort()`](https://ndadey.github.io/ReportR/reference/run_general_cohort.md) |
| **4. Score quality** | Is there anything in the scores to investigate before results are shared? Floor and ceiling pileups, out-of-range scores, unusual year-to-year changes, correlation shifts, and distribution shape. | [`run_anomaly_analysis()`](https://ndadey.github.io/ReportR/reference/run_anomaly_analysis.md) |
| **5. School and district** | How do schools and districts vary, which stand out given their demographics, and how stable are their results? | [`run_aggregate_analysis()`](https://ndadey.github.io/ReportR/reference/run_aggregate_analysis.md) |

Each line has a matching Quarto report template; the
[walkthrough](https://ndadey.github.io/ReportR/articles/walkthrough.html#render-a-report)
lists them. Line 2 (student group analysis) is not yet built.

## Inside a results list

Every analysis returns a named list with one element per sub-analysis.
Most sub-analyses share the same three parts:

    general_trend_results
    ├── $score_trends
    │   ├── $table    data frame: mean, SD, and N by subject, grade, and year
    │   ├── $plot     ggplot objects, one per subject
    │   └── $text     $narrative: a plain-language summary of the changes
    ├── $proficiency
    ├── $distributions
    └── $achievement_transitions

The narratives are written from the results themselves, for example
*“Mathematics: up in 5 of 8 grades, most in grade 4 (+11.1 points)”*, so
the report text always matches the numbers. They lead with what changed
and name only the largest movers; full detail stays in the tables beside
them.

## Built to work across states

- **One spec per assessment.** Subjects, grades, score ranges, cut
  scores, and achievement-level labels live in a single R file, never in
  the analysis code. Copy the template, fill it in, and every analysis
  adapts. Specs for Mississippi (MAAP) and Arkansas (ATLAS) ship with
  the package as worked examples.
- **Different assessment designs.** Four- or five-level systems,
  grade-level subjects, and end-of-course subjects taken across several
  grades.
- **Small-group suppression.** Statistics for groups smaller than
  `min_n` (default 10) are reported as missing rather than shown.
- **Partial data is fine.** Analyses that need data you don’t have, such
  as persistent student IDs for cohort tracking, return empty results
  instead of stopping the run.

## The data ReportR expects

One data frame, `student_results_long`, with one row per student, year,
and subject:

| Columns | Needed for |
|----|----|
| `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL` | Every analysis |
| `STUDENT_ID`, persistent across years | Cohort analysis, achievement-level transitions, correlations |
| `SCHOOL_ID`, `DISTRICT_ID`, and their names | School and district results |
| `RACE_ETHNICITY`, `FRL`, `EL`, `IEP`, `GENDER` | Subgroup results |

[`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md)
renames common source columns (for example `CONTENT_AREA` to `SUBJECT`,
`SCHOOL_NUMBER` to `SCHOOL_ID`, and `ELL_STATUS` to `EL`) and converts
academic-year labels such as `2019_2020` to the year the test was given.
Data in the SGP long format used for Student Growth Percentile analyses
needs little else. See the [data
contract](https://ndadey.github.io/ReportR/reference/student_results_long.md)
for every column.

## Installation

``` r

# install.packages("devtools")
devtools::install_github("ndadey/ReportR")

# Simulated data used in the walkthrough
install.packages("SGPdata")

# Optional: more plot types, Excel export, and school-level ICC models
install.packages(c("patchwork", "ggalluvial", "ggrepel", "openxlsx", "lme4"))
```

Rendering reports also requires [Quarto](https://quarto.org) and the
`quarto` R package.

## Project status

ReportR is in active development (version 0.1.0). Lines 1, 1.2, 1.3, 4,
and 5 are complete. Line 2 is not yet built, and the school
accountability and intervention analyses in Line 5 are waiting on a
school-level data source.
