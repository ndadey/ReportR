# ==============================================================================
# 00_AR_Assessment_Spec.R — Arkansas ATLAS Assessment Specification
#
# Sources:
#   LOSS/HOSS    — Table 47, ATLAS 2023-2024 Annual Technical Report
#                  (Cambium Assessment, Inc., March 2025); confirmed
#                  against HOSS_LOSS.xlsx.
#   ELA/MATH cuts — Derived from Table 41 (theta cut scores) using
#                   grade-level scale score descriptives (Table 8).
#                   Rounded to nearest integer. Verify against DESE
#                   official score tables before public reporting.
#   SCIENCE cuts  — Table 44 (reported directly in scale score units).
#   EOC cuts      — Table 41 thetas (ALG, GEO) and Table 44 (BIO).
# ==============================================================================

assessment_spec <- list(

  # ================================================================
  # 1. Program Identity
  # ================================================================
  program = list(
    name         = "Arkansas Teaching and Learning Assessment System",
    short_name   = "ATLAS",
    state        = "AR",
    department   = "Arkansas Division of Elementary and Secondary Education",
    vendor       = "Cambium Assessment, Inc.",
    program_type = "summative"
  ),

  # ================================================================
  # 2. Subjects
  #    grades = integer vector for span-tested subjects
  #    grades = NULL for EOC subjects
  #    List names must match SUBJECT values after setup column mapping.
  #
  #    Note: READING and WRITING are components of the ELA composite.
  #    ELA is the composite score. All three are on the same scale.
  # ================================================================
  subjects = list(
    ELA     = list(label = "English Language Arts (Composite)", grades = 3:10),
    READING = list(label = "Reading",                           grades = 3:10),
    WRITING = list(label = "Writing",                           grades = 3:10),
    MATH    = list(label = "Mathematics",                       grades = 3:8),
    SCIENCE = list(label = "Science",                           grades = 3:8),
    ALG     = list(label = "Algebra I",                         grades = NULL),
    GEO     = list(label = "Geometry",                          grades = NULL),
    BIO     = list(label = "Biology",                           grades = NULL)
  ),

  # ================================================================
  # 3. Achievement Levels
  #    labels: ordered vector, lowest to highest.
  #    ATLAS reports ELA/Math as Level 1–4.
  #    Science PLDs use descriptive labels (kept in science_labels for
  #    reference) but the data column stores 1–4 for all subjects, so
  #    Level 1–4 is used uniformly in the pipeline.
  #    policy_benchmark: first level considered on-track / proficient.
  # ================================================================
  achievement_levels = list(
    labels           = c("Level 1", "Level 2", "Level 3", "Level 4"),
    science_labels   = c("Below Proficient", "Approaching Proficient",
                         "Proficient", "Highly Proficient"),
    policy_benchmark = "Level 3"
  ),

  # ================================================================
  # 4. Scale Score Structure
  #    loss/hoss: grade-specific (Table 47).
  #    cuts: upper bounds for Levels 1, 2, and 3 (length = 3).
  #    Span-tested: scale_scores$SUBJECT$GRADE (character grade key).
  #    EOC:         scale_scores$SUBJECT (no grade nesting).
  # ================================================================
  scale_scores = list(

    # -- ELA (Composite, grades 3–10) ------------------------------
    ELA = list(
      `3`  = list(loss = 1000, hoss = 1118, cuts = c(1041, 1055, 1067)),
      `4`  = list(loss = 1000, hoss = 1119, cuts = c(1039, 1054, 1070)),
      `5`  = list(loss = 1000, hoss = 1118, cuts = c(1043, 1057, 1069)),
      `6`  = list(loss = 1000, hoss = 1118, cuts = c(1040, 1054, 1069)),
      `7`  = list(loss = 1000, hoss = 1117, cuts = c(1038, 1055, 1068)),
      `8`  = list(loss = 1000, hoss = 1116, cuts = c(1041, 1055, 1070)),
      `9`  = list(loss = 1000, hoss = 1118, cuts = c(1039, 1053, 1068)),
      `10` = list(loss = 1000, hoss = 1117, cuts = c(1039, 1056, 1070))
    ),

    # -- READING (component of ELA, same scale) --------------------
    READING = list(
      `3`  = list(loss = 1000, hoss = 1118, cuts = c(1041, 1055, 1067)),
      `4`  = list(loss = 1000, hoss = 1119, cuts = c(1039, 1054, 1070)),
      `5`  = list(loss = 1000, hoss = 1118, cuts = c(1043, 1057, 1069)),
      `6`  = list(loss = 1000, hoss = 1118, cuts = c(1040, 1054, 1069)),
      `7`  = list(loss = 1000, hoss = 1117, cuts = c(1038, 1055, 1068)),
      `8`  = list(loss = 1000, hoss = 1116, cuts = c(1041, 1055, 1070)),
      `9`  = list(loss = 1000, hoss = 1118, cuts = c(1039, 1053, 1068)),
      `10` = list(loss = 1000, hoss = 1117, cuts = c(1039, 1056, 1070))
    ),

    # -- WRITING (component of ELA, same scale) --------------------
    WRITING = list(
      `3`  = list(loss = 1000, hoss = 1118, cuts = c(1041, 1055, 1067)),
      `4`  = list(loss = 1000, hoss = 1119, cuts = c(1039, 1054, 1070)),
      `5`  = list(loss = 1000, hoss = 1118, cuts = c(1043, 1057, 1069)),
      `6`  = list(loss = 1000, hoss = 1118, cuts = c(1040, 1054, 1069)),
      `7`  = list(loss = 1000, hoss = 1117, cuts = c(1038, 1055, 1068)),
      `8`  = list(loss = 1000, hoss = 1116, cuts = c(1041, 1055, 1070)),
      `9`  = list(loss = 1000, hoss = 1118, cuts = c(1039, 1053, 1068)),
      `10` = list(loss = 1000, hoss = 1117, cuts = c(1039, 1056, 1070))
    ),

    # -- MATHEMATICS (grades 3–8) ----------------------------------
    MATH = list(
      `3` = list(loss = 1000, hoss = 1118, cuts = c(1044, 1056, 1071)),
      `4` = list(loss = 1001, hoss = 1120, cuts = c(1035, 1052, 1073)),
      `5` = list(loss = 1002, hoss = 1120, cuts = c(1039, 1053, 1067)),
      `6` = list(loss = 1004, hoss = 1120, cuts = c(1037, 1049, 1065)),
      `7` = list(loss = 1001, hoss = 1120, cuts = c(1043, 1057, 1070)),
      `8` = list(loss = 1003, hoss = 1120, cuts = c(1035, 1046, 1057))
    ),

    # -- SCIENCE (grades 3–8) --------------------------------------
    SCIENCE = list(
      `3` = list(loss = 1000, hoss = 1120, cuts = c(1045, 1060, 1075)),
      `4` = list(loss = 1000, hoss = 1120, cuts = c(1044, 1060, 1074)),
      `5` = list(loss = 1000, hoss = 1120, cuts = c(1044, 1060, 1074)),
      `6` = list(loss = 1000, hoss = 1120, cuts = c(1043, 1060, 1074)),
      `7` = list(loss = 1000, hoss = 1120, cuts = c(1044, 1060, 1075)),
      `8` = list(loss = 1000, hoss = 1120, cuts = c(1044, 1060, 1073))
    ),

    # -- EOC (no grade nesting) ------------------------------------
    ALG = list(loss = 1000, hoss = 1115, cuts = c(1046, 1055, 1065)),
    GEO = list(loss = 1000, hoss = 1110, cuts = c(1046, 1059, 1066)),
    BIO = list(loss = 1000, hoss = 1120, cuts = c(1043, 1060, 1073))
  ),

  # ================================================================
  # 5. Years & Cohorts
  # ================================================================
  years = list(
    tested_years        = 2024:2025,
    cohort_anchor_grade = 3L
  ),

  # ================================================================
  # 6. Demographics
  #    Keys must match canonical column names produced by AR/00_AR_setup.R.
  # ================================================================
  demographics = list(
    RACE_ETHNICITY = list(type = "categorical"),
    GENDER         = list(type = "categorical"),
    FRL            = list(type = "binary"),
    EL             = list(type = "binary"),
    IEP            = list(type = "binary"),
    FORMER_EL      = list(type = "binary")
  ),

  # ================================================================
  # 7. Column map (raw → canonical; for documentation only)
  #    Actual renaming is performed in AR/00_AR_setup.R.
  # ================================================================
  column_map = list(
    STUDENT_ID     = "ID",
    SUBJECT        = "CONTENT_AREA",
    YEAR           = "YEAR",
    GRADE          = "GRADE",
    SCALE_SCORE    = "SCALE_SCORE",
    ACHIEVEMENT_LEVEL = "READINESS_LEVEL",
    DISTRICT_ID    = "district_lea",
    SCHOOL_ID      = "school_lea",
    GENDER         = "gender",
    RACE_ETHNICITY = "federal_race",
    IEP            = "sped",
    FRL            = "free_reduced_lunch_status",
    EL             = "english_language_learner",
    FORMER_EL      = "monitored_former_english_language_learner"
  ),

  # ================================================================
  # 8. Federal race/ethnicity code lookup (documentation only)
  # ================================================================
  race_codes = list(
    `1` = "American Indian or Alaska Native",
    `2` = "Asian",
    `3` = "Black or African American",
    `4` = "Hispanic/Latino",
    `5` = "Native Hawaiian or Other Pacific Islander",
    `6` = "White",
    `7` = "Two or More Races"
  ),

  # ================================================================
  # 9. Program notes (documentation only)
  # ================================================================
  notes = list(
    adaptive        = TRUE,
    irt_model       = "MIRT",
    baseline_year   = 2024,
    ela_components  = paste(
      "ELA composite = Reading + Writing.",
      "READING and WRITING are separate reportable scores on the same",
      "scale as ELA. Use ELA for overall performance;",
      "READING/WRITING for component-level analysis."
    ),
    eoc_note        = paste(
      "EOC subjects (ALG, GEO, BIO) are assigned to the grade in which",
      "the student is enrolled. Primary testing grades: ALG grades 8-10,",
      "GEO grades 9-11, BIO grade 10."
    ),
    cut_score_note  = paste(
      "ELA and Math cut scores derived from theta values in Table 41 of",
      "the 2023-2024 ATLAS Annual Technical Report using grade-level",
      "scale score descriptives (Table 8). Science cuts are directly from",
      "Table 44. Verify ELA/Math cuts against DESE official score tables",
      "before public reporting."
    )
  )

)
