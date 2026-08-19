# =============================================================================
# states/_config_template.R — Multi-State Configuration Template
#
# HOW TO USE:
#   1. Copy this file to states/_config.R
#   2. Fill in PKG_ROOT and the data output paths for each state
#   3. states/_config.R is gitignored — never commit it
#
# TO ADD A NEW STATE:
#   1. Copy inst/specs/00_TEMPLATE_Spec.R to inst/specs/00_{STATE}_Assessment_Spec.R
#   2. Create states/{STATE}/_setup.R (see states/DEMO/_setup.R for the pattern)
#   3. Add an entry to state_configs below in states/_config.R
# =============================================================================

PKG_ROOT <- "FILL_IN_ABSOLUTE_PATH_TO_REPO"
# Example: "C:/Users/YourName/Dropbox/Git/ReportR"

state_configs <- list(

  DEMO = list(
    spec   = file.path(PKG_ROOT, "inst/specs/00_DEMO_Assessment_Spec.R"),
    setup  = file.path(PKG_ROOT, "states/DEMO/_setup.R"),
    output = file.path(PKG_ROOT, "output/DEMO")
  ),

  MS = list(
    spec   = file.path(PKG_ROOT, "inst/specs/00_MS_Assessment_Spec.R"),
    setup  = file.path(PKG_ROOT, "states/MS/_setup.R"),   # gitignored — create locally
    output = "FILL_IN_OUTPUT_PATH/MS"
    # Example: output = "C:/Users/YourName/Dropbox/ReportR_Outputs/MS"
  ),

  AR = list(
    spec   = file.path(PKG_ROOT, "inst/specs/00_AR_Assessment_Spec.R"),
    setup  = file.path(PKG_ROOT, "states/AR/_setup.R"),   # gitignored — create locally
    output = "FILL_IN_OUTPUT_PATH/AR"
  )

  # Add additional states here following the same pattern:
  # XX = list(
  #   spec   = file.path(PKG_ROOT, "inst/specs/00_XX_Assessment_Spec.R"),
  #   setup  = file.path(PKG_ROOT, "states/XX/_setup.R"),
  #   output = "FILL_IN_OUTPUT_PATH/XX"
  # )
)
