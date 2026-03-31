# ==============================================================================
# DD_study.R  -  Death Details  -  Project level checks
# DDPRJ001 : Missing coded cause of death (DDDECOD)
# DDPRJ002 : Unconfirmed death status (DDSTAT not CONFIRMED)
# DDPRJ003 : Missing site ID (SITEID)
# Rule_Set  : DD_PRJ
# ==============================================================================
check_DD_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$dd) || nrow(domains$dd)==0) {
    message("  WARNING [DD_study]: domains$dd is empty - skipping.") ; return(state) }
  dd <- domains$dd

  if (isTRUE(active_rules["DDPRJ001"])) {
    message("  Running DDPRJ001 - Missing DDDECOD ...")
    DDPRJ001 <- dd |>
      dplyr::filter(!is.na(DDTERM),trimws(DDTERM)!="",is.na(DDDECOD)|trimws(DDDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Coded cause of death (DDDECOD) is missing for term: ",DDTERM)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DDPRJ001,id="DDPRJ001") }

  if (isTRUE(active_rules["DDPRJ002"])) {
    message("  Running DDPRJ002 - Unconfirmed death status ...")
    DDPRJ002 <- dd |>
      dplyr::filter(!is.na(DDSTAT),trimws(DDSTAT)!="",
                    toupper(trimws(DDSTAT)) != "CONFIRMED") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Death status (DDSTAT='",DDSTAT,
          "') is not CONFIRMED for cause: ",
          ifelse(is.na(DDTERM),"[DDTERM missing]",DDTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DDPRJ002,id="DDPRJ002") }

  if (isTRUE(active_rules["DDPRJ003"])) {
    message("  Running DDPRJ003 - Missing SITEID ...")
    DDPRJ003 <- dd |>
      dplyr::filter(is.na(SITEID)|trimws(SITEID)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Site ID (SITEID) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DDPRJ003,id="DDPRJ003") }
  state
}
