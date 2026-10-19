# ReportR

**Repeatable annual analyses of state summative assessment data.**

Every year, state assessment programs answer the same questions about their
results. How did performance change? Who was tested? Do the scores behave as
expected? Which schools stand out? ReportR packages those analyses so a state
can rerun them on each new year of data, and a new state can adopt them, by
changing configuration rather than code.

You supply two things: your student records in a standard long format, and a
short file describing your assessment. ReportR returns tables, plots, and
plain-language summaries for each line of analysis, ready to drop into Word
reports.

<p class="rr-cta">
<a class="btn btn-primary" href="articles/walkthrough.html">Start the walkthrough</a>
<a class="btn btn-outline-primary" href="reference/index.html">Browse the functions</a>
</p>

## How it works

<div class="rr-flow">
<div class="rr-step">
<span class="rr-num">1</span>
<h3>Set up</h3>
<p><strong>Student records</strong> in one long data frame, one row per student,
year, and subject.
<a href="reference/standardize_student_results.html"><code>standardize_<wbr>student_<wbr>results()</code></a>
renames common source columns to the names ReportR expects.</p>
<p><strong>An assessment spec</strong>: one R file listing subjects, grades,
score ranges, cut scores, and achievement levels.</p>
</div>
<div class="rr-step">
<span class="rr-num">2</span>
<h3>Analyze</h3>
<p>One function per line of analysis, such as
<a href="reference/run_general_trend_analysis.html"><code>run_general_<wbr>trend_<wbr>analysis()</code></a>,
returns a <strong>results list</strong>: a table, plots, and narrative text for
every sub-analysis.</p>
<p>Save it once as an <code>.rds</code> file.</p>
</div>
<div class="rr-step">
<span class="rr-num">3</span>
<h3>Report</h3>
<p><strong>Quarto templates</strong> read the saved results and render Word
reports, with tables formatted for Word.</p>
<p><a href="reference/export_tables_to_excel.html"><code>export_tables_<wbr>to_excel()</code></a>
writes every table to a workbook.</p>
</div>
</div>

Reports read the saved results instead of rerunning the analysis, so you can
revise a report's layout and render it again in seconds.

## Lines of analysis

| Line | Question it answers | Run it with |
|------|---------------------|-------------|
| **1. General trend** | How has performance changed over time? Mean scale scores, proficiency rates, score distributions, and achievement-level transitions. | `run_general_trend_analysis()` |
| **1.2 Enrollment** | Who was tested, and how has the tested population changed? Counts, subgroup composition, and students entering and leaving. | `run_general_enrollment_analysis()` |
| **1.3 Cohort** | How do the same students perform as they move through the grades? | `run_general_cohort()` |
| **4. Score quality** | Is there anything in the scores to investigate before results are shared? Floor and ceiling pileups, out-of-range scores, unusual year-to-year changes, correlation shifts, and distribution shape. | `run_anomaly_analysis()` |
| **5. School and district** | How do schools and districts vary, which stand out given their demographics, and how stable are their results? | `run_aggregate_analysis()` |

Each line has a matching Quarto report template; the
[walkthrough](articles/walkthrough.html#render-a-report) lists them. Line 2
(student group analysis) is not yet built.

## Inside a results list

Every analysis returns a named list with one element per sub-analysis. Most
sub-analyses share the same three parts:

```
general_trend_results
├── $score_trends
│   ├── $table    data frame: mean, SD, and N by subject, grade, and year
│   ├── $plot     ggplot objects, one per subject
│   └── $text     $narrative: a plain-language summary of the changes
├── $proficiency
├── $distributions
└── $achievement_transitions
```

The narratives are written from the results themselves, for example
*"Grade 4: ↑ 11.1 points (2020 → 2024)"*, so the report text always matches
the numbers.

## Built to work across states

- **One spec per assessment.** Subjects, grades, score ranges, cut scores, and
  achievement-level labels live in a single R file, never in the analysis
  code. Copy the template, fill it in, and every analysis adapts. Specs for
  Mississippi (MAAP) and Arkansas (ATLAS) ship with the package as worked
  examples.
- **Different assessment designs.** Four- or five-level systems, grade-level
  subjects, and end-of-course subjects taken across several grades.
- **Small-group suppression.** Statistics for groups smaller than `min_n`
  (default 10) are reported as missing rather than shown.
- **Partial data is fine.** Analyses that need data you don't have, such as
  persistent student IDs for cohort tracking, return empty results instead of
  stopping the run.

## The data ReportR expects

One data frame, `student_results_long`, with one row per student, year, and
subject:

| Columns | Needed for |
|---------|------------|
| `YEAR`, `SUBJECT`, `GRADE`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL` | Every analysis |
| `STUDENT_ID`, persistent across years | Cohort analysis, achievement-level transitions, correlations |
| `SCHOOL_ID`, `DISTRICT_ID`, and their names | School and district results |
| `RACE_ETHNICITY`, `FRL`, `EL`, `IEP`, `GENDER` | Subgroup results |

`standardize_student_results()` renames common source columns (for example
`CONTENT_AREA` to `SUBJECT`, `SCHOOL_NUMBER` to `SCHOOL_ID`, and `ELL_STATUS`
to `EL`) and converts academic-year labels such as `2019_2020` to the year the
test was given. Data in the SGP long format used for Student Growth Percentile
analyses needs little else. See the
[data contract](reference/student_results_long.html) for every column.

## Installation

```r
# install.packages("devtools")
devtools::install_github("ndadey/ReportR")

# Simulated data used in the walkthrough
install.packages("SGPdata")

# Optional: more plot types, Excel export, and school-level ICC models
install.packages(c("patchwork", "ggalluvial", "ggrepel", "openxlsx", "lme4"))
```

Rendering reports also requires [Quarto](https://quarto.org) and the `quarto`
R package.

## Project status

ReportR is in active development (version 0.1.0). Lines 1, 1.2, 1.3, 4, and 5
are complete. Line 2 is not yet built, and the school accountability and
intervention analyses in Line 5 are waiting on a school-level data source.
