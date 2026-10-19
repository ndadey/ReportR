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
    READING     = list(label = "Reading",     grades = 3:10),
    MATHEMATICS = list(label = "Mathematics", grades = 3:10)
  ),

  # ================================================================
  # 3. Achievement Levels
  #    Matches ACHIEVEMENT_LEVEL values in sgpData_LONG exactly.
  #    4 levels — exercises the variable-level-count code paths
  #    that differ from the 5-level MS spec.
  #    sgpData_LONG also contains "No Score" records (no scale
  #    score); drop them before analysis.
  # ================================================================
  achievement_levels = list(
    labels           = c("Unsatisfactory", "Partially Proficient",
                         "Proficient", "Advanced"),
    policy_benchmark = "Proficient"
  ),

  # ================================================================
  # 4. Scale Score Structure
  #    LOSS/HOSS and cut scores are taken from the SGP package's
  #    SGPstateData[["DEMO"]] (Knots_Boundaries loss.hoss and
  #    Achievement Cutscores; SGP 2.2.0.0).
  #
  #    SGP cut scores are the LOWEST score of the next level. ReportR
  #    derives levels as sum(score > cuts) + 1, so each cut here is the
  #    HIGHEST score of the lower level: SGP cut - 1 (scores are
  #    integers). These reproduce sgpData_LONG's ACHIEVEMENT_LEVEL
  #    exactly.
  #
  #    Structure: scale_scores$SUBJECT$GRADE for span-tested subjects,
  #               scale_scores$SUBJECT       for EOC subjects.
  #    Grade keys are character strings matching as.character(GRADE).
  # ================================================================
  scale_scores = list(

    # -- READING ----------------------------------------------------
    READING = list(
      `3`  = list(loss = 150, hoss = 795, cuts = c(465, 525, 655)),
      `4`  = list(loss = 180, hoss = 940, cuts = c(516, 571, 670)),
      `5`  = list(loss = 220, hoss = 955, cuts = c(537, 587, 690)),
      `6`  = list(loss = 260, hoss = 970, cuts = c(542, 599, 695)),
      `7`  = list(loss = 300, hoss = 980, cuts = c(566, 619, 715)),
      `8`  = list(loss = 330, hoss = 990, cuts = c(577, 631, 723)),
      `9`  = list(loss = 350, hoss = 995, cuts = c(584, 641, 738)),
      `10` = list(loss = 370, hoss = 999, cuts = c(606, 662, 746))
    ),

    # -- MATHEMATICS ------------------------------------------------
    MATHEMATICS = list(
      `3`  = list(loss = 150, hoss = 700, cuts = c(334, 418, 509)),
      `4`  = list(loss = 180, hoss = 780, cuts = c(382, 454, 537)),
      `5`  = list(loss = 220, hoss = 800, cuts = c(421, 493, 561)),
      `6`  = list(loss = 240, hoss = 830, cuts = c(453, 519, 588)),
      `7`  = list(loss = 280, hoss = 860, cuts = c(486, 558, 613)),
      `8`  = list(loss = 310, hoss = 890, cuts = c(520, 576, 627)),
      `9`  = list(loss = 340, hoss = 920, cuts = c(547, 601, 651)),
      `10` = list(loss = 370, hoss = 950, cuts = c(561, 626, 691))
    )
  ),

  # ================================================================
  # 5. Years & Cohorts
  #    sgpData_LONG years run 2019_2020 to 2023_2024;
  #    standardize_student_results() converts them to end years.
  # ================================================================
  years = list(
    tested_years        = 2020:2024,
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
  #    sgpData_LONG codes values with a label prefix ("ELL: Yes",
  #    "Gender: Female"). Strip the prefix so binary variables read
  #    "Yes"/"No" — aggregate analyses count YES/Y/TRUE/1 as "yes".
  # ================================================================
  demographics = list(
    RACE_ETHNICITY = list(type = "categorical"),
    FRL            = list(type = "binary"),
    EL             = list(type = "binary"),
    IEP            = list(type = "binary"),
    GENDER         = list(type = "categorical")
  ),

  # ================================================================
  # 7. Notes
  # ================================================================
  notes = list(
    data_source = "SGPdata R package — sgpData_LONG object (SGPdata 28.0.0.0)",
    intended_use = paste(
      "Demonstration and testing only. sgpData_LONG is simulated data;",
      "results should not be interpreted substantively."
    ),
    data_prep = paste(
      "Keep VALID_CASE == 'VALID_CASE', run standardize_student_results(),",
      "drop records with no scale score ('No Score'), and strip the",
      "'Label: ' prefix from FRL, EL, IEP, and GENDER values."
    )
  )
)
