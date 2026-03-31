# ==============================================================================
# Check_Template.R  -  CoreGage check script scaffold
#
# HOW TO USE:
#   1. Copy this file into rules/trial/ (for trial checks) or
#      rules/study/ (for study-level checks)
#   2. Rename the file to match the Rule_Set column in rule_registry.xlsx
#      e.g. for Rule_Set = "AE"        save as AE.R
#           for Rule_Set = "AE_study"  save as AE_study.R
#   3. Rename the function below to check_<Rule_Set>
#      e.g. check_AE  or  check_AE_study
#   4. Implement your check logic inside each if() block
#
# FUNCTION CONVENTION:
#   check_<Rule_Set>(state, cfg) -> state
#   - state  : shared state object (do not modify directly - use collect_findings)
#   - cfg    : project config list (rarely needed inside checks)
#   - return : always return state at the end
# ==============================================================================

check_RULESET <- function(state, cfg) {    # <-- rename to check_<your_Rule_Set>

  domains      <- state$domains       # named list of domain data frames
  active_rules <- state$active_rules  # named logical: TRUE = check is ON

  # ============================================================================
  # Check ID: CHECKID    <-- replace with ID from rule_registry.xlsx
  # ============================================================================
  if (isTRUE(active_rules["CHECKID"])) {

    # Step 1 - Access the domain data frame
    #   domain name = lowercase file name e.g. inputs/AE.csv -> domains$ae
    df <- domains$your_domain

    # Step 2 - Apply check logic (filter, mutate etc.)
    # ...

    # Step 3 - Build the result data frame.
    #   Required columns:
    #     subj_id     (character) - subject identifier
    #     vis_id      (numeric)   - visit number, or NA_real_ if not applicable
    #     description (character) - plain English description of the issue,
    #                               max 200 characters
    CHECKID <- df |>
      dplyr::filter(/* your condition */) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = paste0("Meaningful description of the issue")
      ) |>
      dplyr::select(subj_id, vis_id, description)

    # Step 4 - Collect findings into the master issues table
    state <- collect_findings(state, CHECKID, id = "CHECKID")
  }

  # Add more checks following the same pattern ...

  state   # always return state
}
