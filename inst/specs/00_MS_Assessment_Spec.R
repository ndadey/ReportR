assessment_spec <- list(
  
  # ================================================================
  # 1. Program Identity
  # ================================================================
  program = list(
    name         = "Mississippi Academic Assessment Program",
    short_name   = "MAAP",
    state        = "MS",
    department   = "Mississippi Department of Education",
    program_type = "summative"
  ),
  
  # ================================================================
  # 2. Subjects
  #    grades = integer vector for span-tested subjects
  #    grades = NULL for EOC subjects (no grade key in scale_scores)
  #    list names should match those in the 
  # ================================================================
  subjects = list(
    ELA     = list(label = "English Language Arts",  grades = 3:8),
    MATH    = list(label = "Mathematics",            grades = 3:8),
    SCIENCE = list(label = "Science",                grades = c(5, 8)),
    ENG_II  = list(label = "English II",             grades = NULL),
    ALG_I   = list(label = "Algebra I",              grades = NULL),
    BIO_I   = list(label = "Biology I",              grades = NULL)
  ),
  
  # ================================================================
  # 3. Achievement Levels
  #    labels:           ordered vector, lowest to highest
  #    policy_benchmark: label of the "proficient" level
  # ================================================================
  achievement_levels = list(
    labels           = c("Minimal", "Basic", "Passing", "Proficient", "Advanced"),
    policy_benchmark = "Proficient"
  ),
  
  # ================================================================
  # 4. Scale Score Structure
  #    Keys: <SUBJECT>_<GRADE> for span-tested, <SUBJECT> for EOC
  #    loss: lowest obtainable scale score
  #    hoss: highest obtainable scale score
  #    cuts: upper bounds of each level except the top (whose upper
  #          bound is hoss). Length = n_levels - 1. Ordered low to high.
  #
  #    Source: https://mdek12.org/wp-content/uploads/sites/41/2025/09/
  #            maap_scale_score_tables-updated_9.11.2025.pdf
  # ================================================================
  scale_scores = list(
    
    # -- ELA --------------------------------------------------------
    ELA_3 = list(loss = 301, hoss = 399, cuts = c(334, 349, 364, 386)),
    ELA_4 = list(loss = 401, hoss = 499, cuts = c(428, 449, 464, 487)),
    ELA_5 = list(loss = 501, hoss = 599, cuts = c(538, 549, 564, 581)),
    ELA_6 = list(loss = 601, hoss = 699, cuts = c(635, 649, 664, 678)),
    ELA_7 = list(loss = 701, hoss = 799, cuts = c(737, 749, 764, 775)),
    ELA_8 = list(loss = 801, hoss = 899, cuts = c(841, 849, 864, 879)),
    
    # -- MATH -------------------------------------------------------
    MATH_3 = list(loss = 301, hoss = 399, cuts = c(332, 349, 364, 383)),
    MATH_4 = list(loss = 401, hoss = 499, cuts = c(435, 449, 464, 483)),
    MATH_5 = list(loss = 501, hoss = 599, cuts = c(539, 549, 564, 578)),
    MATH_6 = list(loss = 601, hoss = 699, cuts = c(635, 649, 664, 686)),
    MATH_7 = list(loss = 701, hoss = 799, cuts = c(735, 749, 764, 792)),
    MATH_8 = list(loss = 801, hoss = 899, cuts = c(837, 849, 864, 888)),
    
    # -- SCIENCE ----------------------------------------------------
    SCIENCE_5 = list(loss = 500, hoss = 650, cuts = c(540, 549, 564, 588)),
    SCIENCE_8 = list(loss = 800, hoss = 950, cuts = c(840, 849, 864, 888)),
    
    # -- EOC (no grade suffix) --------------------------------------
    ENG_II = list(loss = 1001, hoss = 1099, cuts = c(1036, 1049, 1064, 1080)),
    ALG_I  = list(loss = 1001, hoss = 1099, cuts = c(1038, 1049, 1064, 1087)),
    BIO_I  = list(loss = 1000, hoss = 1180, cuts = c(1037, 1049, 1064, 1094))
  ),
  
  # ================================================================
  # 5. Years & Cohorts
  # ================================================================
  years = list(
    tested_years        = 2018:2025,
    cohort_anchor_grade = 3
  ),
  
  # ================================================================
  # 6. Demographics
  # ================================================================
  demographics = list(
    ETHNICITY = list(type = "categorical"),
    IEP       = list(type = "binary"),
    LEP       = list(type = "binary"),
    FRL       = list(type = "binary"),
    GENDER    = list(type = "categorical")
  )
  
)