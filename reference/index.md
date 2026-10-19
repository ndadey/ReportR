# Package index

## Prepare data and describe the assessment

Put student records in the expected long format and look up values from
the assessment spec.

- [`standardize_student_results()`](https://ndadey.github.io/ReportR/reference/standardize_student_results.md)
  : Standardize student results to the canonical column names
- [`student_results_long`](https://ndadey.github.io/ReportR/reference/student_results_long.md)
  : student_results_long: Expected data format for ReportR analysis
  functions
- [`get_score_spec()`](https://ndadey.github.io/ReportR/reference/get_score_spec.md)
  : Retrieve scale score specification for a subject/grade combination
- [`join_score_bounds()`](https://ndadey.github.io/ReportR/reference/join_score_bounds.md)
  : Attach LOSS/HOSS (and optionally cut scores) to a data frame
- [`build_loss_hoss_lookup()`](https://ndadey.github.io/ReportR/reference/build_loss_hoss_lookup.md)
  : Build a LOSS/HOSS lookup table from an assessment spec
- [`benchmark_index()`](https://ndadey.github.io/ReportR/reference/benchmark_index.md)
  : Return the index of the policy benchmark level in the achievement
  level labels
- [`grade_key()`](https://ndadey.github.io/ReportR/reference/grade_key.md)
  : Ordering key for grade labels

## Line 1: General trend

Systemwide performance over time.

- [`run_general_trend_analysis()`](https://ndadey.github.io/ReportR/reference/run_general_trend_analysis.md)
  : Run all Line 1 general trend analyses

### Scale score trends

- [`run_score_trends()`](https://ndadey.github.io/ReportR/reference/run_score_trends.md)
  : Run the Line 1 score trends sub-analysis
- [`compute_score_summary()`](https://ndadey.github.io/ReportR/reference/compute_score_summary.md)
  : Compute scale score distributional summaries by year, subject, and
  grade
- [`mean_sd_table_wide()`](https://ndadey.github.io/ReportR/reference/mean_sd_table_wide.md)
  : Report-ready wide means/SD table for a single subject
- [`plot_score_trends()`](https://ndadey.github.io/ReportR/reference/plot_score_trends.md)
  : Create trend plots from a score summary
- [`summarize_score_trends()`](https://ndadey.github.io/ReportR/reference/summarize_score_trends.md)
  : Summarize score trends in plain text

### Proficiency

- [`run_proficiency()`](https://ndadey.github.io/ReportR/reference/run_proficiency.md)
  : Run the Line 1 proficiency sub-analysis
- [`compute_proficiency()`](https://ndadey.github.io/ReportR/reference/compute_proficiency.md)
  : Compute percent proficient by year, subject, and grade
- [`plot_proficiency()`](https://ndadey.github.io/ReportR/reference/plot_proficiency.md)
  : Plot proficiency trends and heatmap
- [`summarize_proficiency_findings()`](https://ndadey.github.io/ReportR/reference/summarize_proficiency_findings.md)
  : Summarize proficiency trends in plain text

### Score distributions

- [`run_score_density()`](https://ndadey.github.io/ReportR/reference/run_score_density.md)
  : Run the Line 1 score distributions sub-analysis
- [`plot_score_density()`](https://ndadey.github.io/ReportR/reference/plot_score_density.md)
  : Plot scale score distributions by subject
- [`summarize_score_density()`](https://ndadey.github.io/ReportR/reference/summarize_score_density.md)
  : Summarize score density in plain text

### Achievement-level transitions

- [`run_achievement_transitions()`](https://ndadey.github.io/ReportR/reference/run_achievement_transitions.md)
  : Run the Line 1 achievement transitions sub-analysis
- [`compute_transitions()`](https://ndadey.github.io/ReportR/reference/compute_transitions.md)
  : Compute year-to-year achievement level transition tables
- [`flag_transitions()`](https://ndadey.github.io/ReportR/reference/flag_transitions.md)
  : Flag unusual achievement level transitions
- [`compute_transition_comparisons()`](https://ndadey.github.io/ReportR/reference/compute_transition_comparisons.md)
  : Compare transition patterns to mean score and proficiency trends
- [`plot_transition_heatmaps()`](https://ndadey.github.io/ReportR/reference/plot_transition_heatmaps.md)
  : Plot average transition heatmaps by subject
- [`plot_transition_alluvial()`](https://ndadey.github.io/ReportR/reference/plot_transition_alluvial.md)
  : Plot alluvial (Sankey) flow diagrams for achievement level
  transitions
- [`plot_transition_rates()`](https://ndadey.github.io/ReportR/reference/plot_transition_rates.md)
  : Plot total and net transition rates across all grade/subject/year
  cells
- [`plot_transition_comparisons()`](https://ndadey.github.io/ReportR/reference/plot_transition_comparisons.md)
  : Plot scatterplots comparing transition rates to score/proficiency
  changes
- [`summarize_transition_findings()`](https://ndadey.github.io/ReportR/reference/summarize_transition_findings.md)
  : Summarize achievement transition findings in plain text

## Line 1.2: Enrollment

Who was tested, and how the tested population changed.

- [`run_general_enrollment_analysis()`](https://ndadey.github.io/ReportR/reference/run_general_enrollment_analysis.md)
  : Run all Line 1.2 general enrollment analyses
- [`run_enrollment_analysis()`](https://ndadey.github.io/ReportR/reference/run_enrollment_analysis.md)
  : Run the Line 1.2 enrollment trend analysis
- [`compute_enrollment_counts()`](https://ndadey.github.io/ReportR/reference/compute_enrollment_counts.md)
  : Count enrolled students by arbitrary grouping dimensions
- [`compute_enrollment_proportions()`](https://ndadey.github.io/ReportR/reference/compute_enrollment_proportions.md)
  : Compute subgroup proportions within an enrollment grouping
- [`compute_subgroup_transitions()`](https://ndadey.github.io/ReportR/reference/compute_subgroup_transitions.md)
  : Compute year-over-year subgroup composition shifts
- [`compute_entry_exit_performance()`](https://ndadey.github.io/ReportR/reference/compute_entry_exit_performance.md)
  : Compute performance of entering and exiting students
- [`compute_school_district_enrollment()`](https://ndadey.github.io/ReportR/reference/compute_school_district_enrollment.md)
  : Compute school- or district-level enrollment counts and subgroup
  proportions
- [`compute_school_score_summary()`](https://ndadey.github.io/ReportR/reference/compute_school_score_summary.md)
  : Compute school-level mean scale scores and year-over-year changes
- [`plot_enrollment_table()`](https://ndadey.github.io/ReportR/reference/plot_enrollment_table.md)
  : Format an enrollment data frame as a flextable
- [`plot_enrollment_sankey()`](https://ndadey.github.io/ReportR/reference/plot_enrollment_sankey.md)
  : Plot enrollment composition as an alluvial (Sankey) diagram
- [`summarize_enrollment()`](https://ndadey.github.io/ReportR/reference/summarize_enrollment.md)
  : Summarize enrollment trends in plain text
- [`suppress_small_n_enroll()`](https://ndadey.github.io/ReportR/reference/suppress_small_n_enroll.md)
  : Suppress statistics for small enrollment groups

## Line 1.3: Cohort

The same students followed across grades and years.

- [`run_general_cohort()`](https://ndadey.github.io/ReportR/reference/run_general_cohort.md)
  : Run General Cohort Analysis
- [`run_cohort_analysis()`](https://ndadey.github.io/ReportR/reference/run_cohort_analysis.md)
  : Run all Line 1.3 cohort analysis sub-analyses
- [`compute_cohort_summary()`](https://ndadey.github.io/ReportR/reference/compute_cohort_summary.md)
  : Compute cohort-level scale score and proficiency summaries
- [`compute_cohort_demographics()`](https://ndadey.github.io/ReportR/reference/compute_cohort_demographics.md)
  : Compute demographic composition of cohorts by grade
- [`compute_cohort_transitions()`](https://ndadey.github.io/ReportR/reference/compute_cohort_transitions.md)
  : Compute achievement level transition rates across adjacent grades
  within cohort
- [`compute_cohort_rate_of_change()`](https://ndadey.github.io/ReportR/reference/compute_cohort_rate_of_change.md)
  : Compute rates of improving, stable, and declining trajectories
  across grades
- [`compute_cohort_correlations()`](https://ndadey.github.io/ReportR/reference/compute_cohort_correlations.md)
  : Compute Pearson correlations of scale scores across adjacent grades
  within cohort
- [`compute_cohort_vs_crosssectional()`](https://ndadey.github.io/ReportR/reference/compute_cohort_vs_crosssectional.md)
  : Compare cohort trajectories to cross-sectional grade snapshots
- [`compute_cohort_mobility()`](https://ndadey.github.io/ReportR/reference/compute_cohort_mobility.md)
  : Classify students as Entering, Continuing, or Exiting between
  adjacent grades
- [`compute_mobility_by_achievement()`](https://ndadey.github.io/ReportR/reference/compute_mobility_by_achievement.md)
  : Cross mobility status with achievement level to identify selection
  patterns
- [`compute_cohort_persistence()`](https://ndadey.github.io/ReportR/reference/compute_cohort_persistence.md)
  : Track retention of the original anchor-grade cohort across
  subsequent grades
- [`compute_enrollment_diagnostics()`](https://ndadey.github.io/ReportR/reference/compute_enrollment_diagnostics.md)
  : Flag unusual enrollment changes by grade and year
- [`plot_cohort_mean_scores()`](https://ndadey.github.io/ReportR/reference/plot_cohort_mean_scores.md)
  : Plot mean scale scores by cohort across grades
- [`plot_cohort_proficiency_rates()`](https://ndadey.github.io/ReportR/reference/plot_cohort_proficiency_rates.md)
  : Plot proficiency rates by cohort across grades
- [`plot_cohort_score_distributions()`](https://ndadey.github.io/ReportR/reference/plot_cohort_score_distributions.md)
  : Plot score distributions by cohort using pre-computed quantiles
- [`plot_cohort_demographic_composition()`](https://ndadey.github.io/ReportR/reference/plot_cohort_demographic_composition.md)
  : Plot demographic composition of cohorts
- [`plot_sankey_transitions()`](https://ndadey.github.io/ReportR/reference/plot_sankey_transitions.md)
  : Plot Sankey / alluvial transition diagrams
- [`plot_rate_of_change()`](https://ndadey.github.io/ReportR/reference/plot_rate_of_change.md)
  : Plot rates of improving, stable, and declining trajectories
- [`plot_score_correlations()`](https://ndadey.github.io/ReportR/reference/plot_score_correlations.md)
  : Plot Pearson correlations of scale scores across adjacent grades
- [`plot_mobility_distribution()`](https://ndadey.github.io/ReportR/reference/plot_mobility_distribution.md)
  : Plot mobility status distribution across grades
- [`plot_mobility_by_achievement()`](https://ndadey.github.io/ReportR/reference/plot_mobility_by_achievement.md)
  : Plot mobility status crossed with achievement level
- [`summarize_cohort_analysis()`](https://ndadey.github.io/ReportR/reference/summarize_cohort_analysis.md)
  : Generate plain-language narrative summary of cohort analysis
  findings

## Line 4: Score quality

Diagnostic checks for patterns worth investigating before results are
shared.

- [`run_anomaly_analysis()`](https://ndadey.github.io/ReportR/reference/run_anomaly_analysis.md)
  : Run all Line 4 score quality analyses

### Floor and ceiling pileups (LOSS/HOSS)

- [`run_loss_hoss()`](https://ndadey.github.io/ReportR/reference/run_loss_hoss.md)
  : Run the HOSS/LOSS pileup analysis and return the results sub-list
- [`compute_loss_hoss_table()`](https://ndadey.github.io/ReportR/reference/compute_loss_hoss_table.md)
  : Compute HOSS/LOSS pileup rates by year, subject, and grade
- [`plot_loss_hoss_cleveland()`](https://ndadey.github.io/ReportR/reference/plot_loss_hoss_cleveland.md)
  : Cleveland dot plot of near-LOSS or near-HOSS percentages
- [`plot_loss_hoss_heatmap()`](https://ndadey.github.io/ReportR/reference/plot_loss_hoss_heatmap.md)
  : Heatmap of near-LOSS or near-HOSS percentages
- [`plot_loss_hoss_over_time()`](https://ndadey.github.io/ReportR/reference/plot_loss_hoss_over_time.md)
  : Line plot of near-LOSS or near-HOSS percentages over time
- [`plot_loss_hoss_distributions()`](https://ndadey.github.io/ReportR/reference/plot_loss_hoss_distributions.md)
  : Individual histograms for each grade flagged for HOSS/LOSS pileups
- [`summarize_loss_hoss_findings()`](https://ndadey.github.io/ReportR/reference/summarize_loss_hoss_findings.md)
  : Summarize HOSS/LOSS findings into narrative text and tables

### Score integrity

- [`run_score_integrity()`](https://ndadey.github.io/ReportR/reference/run_score_integrity.md)
  : Run the score integrity sub-analysis and return the results sub-list
- [`compute_score_integrity()`](https://ndadey.github.io/ReportR/reference/compute_score_integrity.md)
  : Compute score integrity checks: invalid scores and level mismatches
- [`plot_score_integrity()`](https://ndadey.github.io/ReportR/reference/plot_score_integrity.md)
  : Plot score integrity results
- [`summarize_score_integrity()`](https://ndadey.github.io/ReportR/reference/summarize_score_integrity.md)
  : Summarize score integrity findings

### Year-to-year outliers

- [`run_outliers()`](https://ndadey.github.io/ReportR/reference/run_outliers.md)
  : Run the outlier sub-analysis and return the results sub-list
- [`compute_outliers()`](https://ndadey.github.io/ReportR/reference/compute_outliers.md)
  : Compute year-to-year outliers in assessment results
- [`plot_outliers()`](https://ndadey.github.io/ReportR/reference/plot_outliers.md)
  : Plot year-to-year outliers
- [`summarize_outliers()`](https://ndadey.github.io/ReportR/reference/summarize_outliers.md)
  : Summarize year-to-year outlier findings

### Cohort correlations

- [`run_correlations()`](https://ndadey.github.io/ReportR/reference/run_correlations.md)
  : Run the correlation sub-analysis and return the results sub-list
- [`compute_correlations()`](https://ndadey.github.io/ReportR/reference/compute_correlations.md)
  : Compute cohort-style adjacent-grade score correlations
- [`plot_correlations()`](https://ndadey.github.io/ReportR/reference/plot_correlations.md)
  : Plot cohort-style score correlations
- [`summarize_correlations()`](https://ndadey.github.io/ReportR/reference/summarize_correlations.md)
  : Summarize cohort correlation findings

### Distribution shape

- [`run_distributions()`](https://ndadey.github.io/ReportR/reference/run_distributions.md)
  : Run the distribution sub-analysis and return the results sub-list
- [`compute_distributions()`](https://ndadey.github.io/ReportR/reference/compute_distributions.md)
  : Compute distribution shape diagnostics
- [`plot_distributions()`](https://ndadey.github.io/ReportR/reference/plot_distributions.md)
  : Plot distribution shape diagnostics
- [`summarize_distributions()`](https://ndadey.github.io/ReportR/reference/summarize_distributions.md)
  : Summarize distribution shape findings

## Line 5: School and district

School- and district-level performance, variability, and outliers.

- [`run_aggregate_analysis()`](https://ndadey.github.io/ReportR/reference/run_aggregate_analysis.md)
  : Run all Section 5 aggregate (school and district) analyses
- [`compute_performance_trends()`](https://ndadey.github.io/ReportR/reference/compute_performance_trends.md)
  : Compute school- and district-level performance trends
- [`compute_population_shifts()`](https://ndadey.github.io/ReportR/reference/compute_population_shifts.md)
  : Compute school- and district-level demographic composition and
  shifts
- [`compute_outlier_schools()`](https://ndadey.github.io/ReportR/reference/compute_outlier_schools.md)
  : Identify schools performing above or below demographic expectation
- [`compute_school_disaggregated()`](https://ndadey.github.io/ReportR/reference/compute_school_disaggregated.md)
  : Compute school-level disaggregated proficiency and within-school
  gaps
- [`compute_stability_analysis()`](https://ndadey.github.io/ReportR/reference/compute_stability_analysis.md)
  : Compute year-over-year rank-order and correlation stability

## Export

- [`export_tables_to_excel()`](https://ndadey.github.io/ReportR/reference/export_tables_to_excel.md)
  : Export all tables from a ReportR results list to Excel
