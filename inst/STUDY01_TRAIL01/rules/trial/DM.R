# ==============================================================================
# DM.R  -  Demographics  -  Trial level checks
#
# DMCHK001 : Missing sex (SEX)
# DMCHK002 : Age outside inclusion criteria (18 to 80 years)
# DMCHK003 : Missing reference start date (RFSTDTC)
#
# Input    : state$domains$dm  - loaded from DM.csv
# Rule_Set  : DM
# ==============================================================================

check_DM <- function(state, cfg) {
  domains <- state$domains
  active_rules  <- state$active_rules

  if (is.null(domains$dm) || nrow(domains$dm) == 0) {
    message("  WARNING [DM]: domains$dm is empty - skipping all DM trial checks.")
    return(state)
  }

  dm <- domains$dm

  # ============================================================================
  # DMCHK001 - Missing sex (SEX)
  # ============================================================================
  if (isTRUE(active_rules["DMCHK001"])) {
    message("  Running DMCHK001 - Missing SEX ...")

    DMCHK001 <- dm |>
      dplyr::filter(is.na(SEX) | trimws(SEX) == "") |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = "Sex (SEX) is missing in the demographics record"
      ) |>
      dplyr::select(subj_id, vis_id, description)

    state <- collect_findings(state, DMCHK001, id = "DMCHK001")
  }

  # ============================================================================
  # DMCHK002 - Age outside inclusion criteria (18 to 80 years)
  # ============================================================================
  if (isTRUE(active_rules["DMCHK002"])) {
    message("  Running DMCHK002 - Age outside inclusion range (18-80) ...")

    DMCHK002 <- dm |>
      dplyr::filter(!is.na(AGE), trimws(AGE) != "") |>
      dplyr::mutate(age_num = suppressWarnings(as.numeric(AGE))) |>
      dplyr::filter(!is.na(age_num), age_num < 18 | age_num > 80) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = paste0(
          "Subject age (AGE=", AGE, " ", AGEU, ")",
          " is outside the inclusion criteria range of 18 to 80 years"
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    state <- collect_findings(state, DMCHK002, id = "DMCHK002")
  }

  # ============================================================================
  # DMCHK003 - Missing reference start date (RFSTDTC)
  # ============================================================================
  if (isTRUE(active_rules["DMCHK003"])) {
    message("  Running DMCHK003 - Missing reference start date (RFSTDTC) ...")

    DMCHK003 <- dm |>
      dplyr::filter(is.na(RFSTDTC) | trimws(RFSTDTC) == "") |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = "Reference start date (RFSTDTC) is missing for the subject"
      ) |>
      dplyr::select(subj_id, vis_id, description)

    state <- collect_findings(state, DMCHK003, id = "DMCHK003")
  }

  state
}
