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
  "YEAR_chr", "flag_counts", "lbl"
))
