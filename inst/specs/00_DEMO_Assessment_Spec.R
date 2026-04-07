assessment_spec <- list(

  # ================================================================
  # 1. Program Identity
  # ================================================================
  program = list(
    name         = "SGPdata Public Demo Assessment",
    short_name   = "DEMO",
    state        = "DEMO",
    department   = "SGPdata R Package (Demonstration Data)",
    program_type = "summative"
  ),

  # ================================================================
  # 2. Subjects
  #    Matches CONTENT_AREA values in sgpData_LONG after
  #    standardize_student_results() renames CONTENT_AREA -> SUBJECT
  # ================================================================
  subjects = list(
    ELA         = list(label = "English Language Arts", grades = 3:8),
    MATHEMATICS = list(label = "Mathematics",            grades = 3:8)
  ),

  # ================================================================
  # 3. Achievement Levels
  #    Matches ACHIEVEMENT_LEVEL values in sgpData_LONG exactly.
  #    4 levels — exercises the variable-level-count code paths
  #    that differ from the 5-level MS spec.
  #    policy_benchmark = "Proficient" (3rd of 4 levels)
  # ================================================================
  achievement_levels = list(
    labels           = c("Unsatisfactory", "Partially Proficient",
                         "Proficient", "Advanced"),
    policy_benchmark = "Proficient"
  ),

  # ================================================================
  # 4. Scale Score Structure
  #    Score ranges are approximate, derived from the known SGPdata
  #    distribution (roughly 200-800 across grades 3-8).
  #    cuts vectors are approximate and should be verified against
  #    the actual sgpData_LONG score distribution before use in
  #    any substantive analysis.
  #
  #    Grade-level ranges follow the convention of the SGPdata
  #    package: each grade's scale is roughly grade * 100 ± 100,
  #    with Grade 3 anchored around 300-500.
  #
  #    Keys: <SUBJECT>_<GRADE> (no EOC subjects in this dataset)
  # ================================================================
  scale_scores = list(

    # -- ELA --------------------------------------------------------
    ELA_3 = list(loss = 250, hoss = 600, cuts = c(350, 420, 500)),
    ELA_4 = list(loss = 300, hoss = 650, cuts = c(390, 460, 540)),
    ELA_5 = list(loss = 350, hoss = 700, cuts = c(430, 500, 580)),
    ELA_6 = list(loss = 380, hoss = 730, cuts = c(460, 530, 610)),
    ELA_7 = list(loss = 400, hoss = 760, cuts = c(490, 560, 640)),
    ELA_8 = list(loss = 420, hoss = 800, cuts = c(510, 590, 670)),

    # -- MATHEMATICS ------------------------------------------------
    MATHEMATICS_3 = list(loss = 250, hoss = 600, cuts = c(350, 420, 500)),
    MATHEMATICS_4 = list(loss = 300, hoss = 650, cuts = c(390, 460, 540)),
    MATHEMATICS_5 = list(loss = 350, hoss = 700, cuts = c(430, 500, 580)),
    MATHEMATICS_6 = list(loss = 380, hoss = 730, cuts = c(460, 530, 610)),
    MATHEMATICS_7 = list(loss = 400, hoss = 760, cuts = c(490, 560, 640)),
    MATHEMATICS_8 = list(loss = 420, hoss = 800, cuts = c(510, 590, 670))
  ),

  # ================================================================
  # 5. Years & Cohorts
  # ================================================================
  years = list(
    tested_years        = 2013:2019,
    cohort_anchor_grade = 3
  ),

  # ================================================================
  # 6. Demographics
  #    Matches column names in sgpData_LONG after
  #    standardize_student_results() canonicalizes them:
  #      ETHNICITY                  -> RACE_ETHNICITY
  #      FREE_REDUCED_LUNCH_STATUS  -> FRL
  #      ELL_STATUS                 -> EL
  #      IEP_STATUS                 -> IEP
  #      GENDER                     -> GENDER
  #
  #    Note: sgpData_LONG has no SCHOOL_ID or DISTRICT_ID —
  #    aggregate analyses (Line 3) cannot be run on this dataset.
  # ================================================================
  demographics = list(
    RACE_ETHNICITY = list(type = "categorical"),
    FRL            = list(type = "binary"),
    EL             = list(type = "binary"),
    IEP            = list(type = "binary"),
    GENDER         = list(type = "binary")
  ),

  # ================================================================
  # 7. Notes
  # ================================================================
  notes = list(
    data_source = "SGPdata R package — sgpData_LONG object",
    intended_use = paste(
      "Development and testing only. Scale score cutpoints are approximate",
      "and should not be used for substantive interpretation.",
      "Use 00_MS_Assessment_Spec.R for real-data analysis."
    ),
    limitations = paste(
      "No SCHOOL_ID or DISTRICT_ID — Line 3 (Aggregate Analysis) cannot",
      "be run on this dataset. VALID_CASE column should be filtered to",
      "'VALID_CASE' before passing to any analysis function."
    )
  )
)
