# Suppress R CMD CHECK notes for variables used in dplyr/ggplot2 non-standard
# evaluation. Every column name referenced with bare (unquoted) syntax in
# mutate(), filter(), ggplot(), aes(), etc. must appear here.
#
# .data is imported from rlang to enable the .data[["col"]] pronoun pattern.
#' @importFrom rlang .data :=
utils::globalVariables(c(
  ".data",

  # --- shared columns ---
  "STUDENT_ID", "YEAR", "GRADE", "SUBJECT", "SCALE_SCORE",
  "ACHIEVEMENT_LEVEL", "SCHOOL_ID", "SCHOOL_NAME",
  "DISTRICT_ID", "DISTRICT_NAME",
  "RACE_ETHNICITY", "FRL", "EL", "IEP", "GENDER",

  # --- anomaly: loss/hoss ---
  "LOSS", "HOSS", "near_loss", "near_hoss",
  "flag_grade_lbl", "year_lbl", "grade_lbl", "max_flag", "cut",
  "n", "n_nonmissing", "n_near_loss", "n_near_hoss",
  "pct_near_loss", "pct_near_hoss", "small_n",
  "pct_near_hoss_change", "pct_near_loss_change",
  "hoss_flag", "loss_flag",
  "grade", "subject", "row_label", "y_lbl",
  "x_min", "x_max",
  "max_hoss_flag", "max_loss_flag", "years_observed",
  "type", "flag", "grades", "summary_text",

  # --- trend: score summary ---
  "mean", "sd", "metric", "value", "stat", "col_name",
  "Mean", "SD", "N",

  # --- anomaly: score integrity ---
  "n_total", "n_below_loss", "n_above_hoss", "n_invalid", "pct_invalid",
  "n_mismatch", "pct_mismatch", "n_unknown_level", "has_any_issue",
  "direction", "EXPECTED_LEVEL", "CUTS",
  "grade_lbl",

  # --- anomaly: outliers ---
  "outlier_flag", "outlier_metric", "outlier_direction",
  "mean_score", "pct_proficient",
  "delta_mean_score", "delta_pct_proficient",
  "year_pair", "point_type", "subj_label",
  "z_col_nm", "z_val", "abs_z", "metric_name",
  "abs_delta_s", "abs_delta_p", "n_cells",
  "prior_yr_", "consecutive_",

  # --- anomaly: correlations ---
  "correlation", "low_flag", "drop_flag", "prior_corr",
  "YEAR_1", "YEAR_2", "SCORE_1", "SCORE_2",
  "GRADE_1", "GRADE_2", "grade_pair",
  "median_corr", "n_low_flag", "n_drop_flag", "cell_lbl",

  # --- anomaly: distributions ---
  "distribution_flag", "flag_reason",
  "flat_flag", "bimodal_flag", "skewed_flag",
  "bimodality_coefficient", "skewness", "kurtosis",
  "p05", "p25", "p50", "p75", "p95",
  "YEAR_chr", "flag_counts", "lbl",

  # --- trend: score trend plots ---
  "mean_val", "sd_val", "lower", "upper",
  "p25_val", "p50_val", "p75_val",

  # --- trend: proficiency ---
  "PROFICIENT", "pct_proficient", "n_proficient",

  # --- trend: achievement transitions ---
  "from_level", "to_level", "prop", "avg_prop", "avg_n",
  "move_dir", "pct", "stable", "up", "down",
  "total_transition_rate", "net_upward_rate",
  "z_total", "z_net", "flag_type", "annotation",
  "divergence_flag", "dir_net", "dir_mean", "dir_prof",
  "delta_mean", "delta_pct_prof", "corr_mean", "corr_prof",
  "year_pair", "cell_id", "pt_label", "diverged", "flagged",
  "GRADE_1", "GRADE_2", "mean_y1", "mean_y2",

  # --- cohort analysis (1.3) ---
  "COHORT_YEAR", "COHORT_YR_CHR",
  "FROM_GRADE", "TO_GRADE", "GRADE_CHR",
  "FROM_LEVEL", "TO_LEVEL", "PCT_OF_FROM",
  "TRAJECTORY",
  "N_ORIGINAL", "N_ENTERING", "N_TOTAL", "PCT_RETAINED",
  "MOBILITY_STATUS",
  "DEMOGRAPHIC", "GROUP", "PCT_WITHIN_MOBILITY",
  "GRADE_X", "GRADE_Y", "YEAR_X", "YEAR_Y",
  "R", "P_VALUE",
  "Z_SCORE", "FLAGGED",
  "MEAN_SCORE", "SD_SCORE", "PCT_PROFICIENT",
  "MEDIAN_SCORE", "Q1_SCORE", "Q3_SCORE",
  "IQR_LO", "IQR_HI",
  "n_level", "n_mob", "n_total", "n_from",
  "lvl_from", "lvl_to",
  "mean_n", "sd_n",
  "SCORE_X", "SCORE_Y",

  # --- enrollment (1.2) ---
  # N, alluvium_id, subgroup_val used as bare names in ggplot2::aes() inside
  # plot_enrollment_sankey(); all other enrollment columns use .data[[]] pronoun
  "N", "alluvium_id", "subgroup_val",
  # Additional columns referenced in dplyr / ggplot expressions without pronoun
  "subgroup_var", "total_N", "N_change", "N_pct_change",
  "suppressed", "pct", "group", "N_sg", "pct_sg",
  "pct_change", "prior_year",
  # enrollment vs. score scatter plot
  "delta_mean", "year_pair"
))
