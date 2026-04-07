# ==============================================================================
# 00_TEMPLATE_Assessment_Spec.R
#
# Blank template for building a state assessment specification.
# Copy this file, rename it "00_<STATE>_Assessment_Spec.R", and fill in every
# field below. See inst/specs/00_MS_Assessment_Spec.R for a complete example.
# ==============================================================================

assessment_spec <- list(

  # ================================================================
  # 1. Program Identity
  # ================================================================
  program = list(
    name         = "",   # Full program name
    short_name   = "",   # Abbreviation used in output labels
    state        = "",   # Two-letter state code
    department   = "",   # Full department name
    program_type = "summative"
  ),

  # ================================================================
  # 2. Subjects
  #    grades = integer vector for span-tested subjects (e.g. 3:8)
  #    grades = NULL for EOC subjects
  # ================================================================
  subjects = list(
    # SUBJECT_CODE = list(label = "Human-readable label", grades = 3:8)
  ),

  # ================================================================
  # 3. Achievement Levels
  #    labels: ordered vector, lowest to highest
  #    policy_benchmark: label string of the "proficient" cut
  # ================================================================
  achievement_levels = list(
    labels           = c(),  # e.g. c("Level 1", "Level 2", "Level 3", "Level 4")
    policy_benchmark = ""    # must match one of the labels above
  ),

  # ================================================================
  # 4. Scale Score Structure
  #    Keys: <SUBJECT>_<GRADE> for span-tested, <SUBJECT> for EOC
  #    loss: lowest obtainable scale score
  #    hoss: highest obtainable scale score
  #    cuts: upper bounds of each level except the top, length = n_levels - 1
  # ================================================================
  scale_scores = list(
    # SUBJECT_3 = list(loss = , hoss = , cuts = c()),
    # SUBJECT_4 = list(loss = , hoss = , cuts = c()),
    # EOC_SUBJECT = list(loss = , hoss = , cuts = c())
  ),

  # ================================================================
  # 5. Years & Cohorts
  # ================================================================
  years = list(
    tested_years        = c(),   # e.g. 2018:2025
    cohort_anchor_grade = 3
  ),

  # ================================================================
  # 6. Demographics
  #    type: "categorical" or "binary"
  # ================================================================
  demographics = list(
    RACE_ETHNICITY = list(type = "categorical"),
    FRL            = list(type = "binary"),
    EL             = list(type = "binary"),
    IEP            = list(type = "binary"),
    GENDER         = list(type = "categorical")
  )
)
