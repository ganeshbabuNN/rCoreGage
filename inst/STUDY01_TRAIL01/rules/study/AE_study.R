# ==============================================================================
# AE_study.R  -  Adverse Events  -  Project level checks
# AEPRJ001 : Serious AE (AESER=Y) with missing action taken (AEACN)
# AEPRJ002 : Dictionary coded term (AEDECOD) missing
# AEPRJ003 : AE study day (AESTDY) missing
# Rule_Set  : AE_PRJ
# ==============================================================================
check_AE_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$ae) || nrow(domains$ae)==0) {
    message("  WARNING [AE_study]: domains$ae is empty - skipping.") ; return(state)
  }
  ae <- domains$ae

  # AEPRJ001 : Serious AE (AESER=Y) with missing action taken (AEACN)
  if (isTRUE(active_rules["AEPRJ001"])) {
    message("  Running AEPRJ001 - Serious AE missing action taken ...")
    AEPRJ001 <- ae |>
      dplyr::filter(!is.na(AESER), toupper(trimws(AESER))=="Y",
                    is.na(AEACN) | trimws(AEACN)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("Serious AE (AESER=Y) has no action taken (AEACN)",
          " for term: ",AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, AEPRJ001, id="AEPRJ001")
  }
  
  # AEPRJ002 : Dictionary coded term (AEDECOD) missing
  if (isTRUE(active_rules["AEPRJ002"])) {
    message("  Running AEPRJ002 - Missing AEDECOD ...")
    AEPRJ002 <- ae |>
      dplyr::filter(is.na(AEDECOD) | trimws(AEDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("Dictionary coded term (AEDECOD) is missing",
          " for verbatim: ",AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, AEPRJ002, id="AEPRJ002")
  }
  
  # AEPRJ003 : AE study day (AESTDY) missing
  if (isTRUE(active_rules["AEPRJ003"])) {
    message("  Running AEPRJ003 - Missing AESTDY ...")
    AEPRJ003 <- ae |>
      dplyr::filter(is.na(AESTDY) | trimws(as.character(AESTDY))=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("AE study day (AESTDY) is missing",
          " for term: ",AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, AEPRJ003, id="AEPRJ003")
  }
  state
}
