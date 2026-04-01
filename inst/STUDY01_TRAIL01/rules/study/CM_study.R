# ==============================================================================
# CM_study.R  -  Concomitant Medications  -  Project level checks
# CMPRJ001 : Missing route of administration (CMROUTE)
# CMPRJ002 : Missing dictionary coded term (CMDECOD)
# CMPRJ003 : Ongoing medication missing end flag (CMENRTPT)
# Rule_Set  : CM_PRJ
# ==============================================================================
check_CM_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$cm) || nrow(domains$cm)==0) {
    message("  WARNING [CM_study]: domains$cm is empty - skipping.") ; return(state) }
  cm <- domains$cm

  # CMPRJ001 : Missing route of administration (CMROUTE)
  if (isTRUE(active_rules["CMPRJ001"])) {
    message("  Running CMPRJ001 - Missing CMROUTE ...")
    CMPRJ001 <- cm |>
      dplyr::filter(is.na(CMROUTE)|trimws(CMROUTE)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Route of administration (CMROUTE) is missing for medication: ",
          ifelse(is.na(CMTRT),"[CMTRT missing]",CMTRT))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,CMPRJ001,id="CMPRJ001") }

  # CMPRJ002 : Missing dictionary coded term (CMDECOD)
  if (isTRUE(active_rules["CMPRJ002"])) {
    message("  Running CMPRJ002 - Missing CMDECOD ...")
    CMPRJ002 <- cm |>
      dplyr::filter(!is.na(CMTRT),trimws(CMTRT)!="",is.na(CMDECOD)|trimws(CMDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Dictionary coded term (CMDECOD) is missing for medication: ",CMTRT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,CMPRJ002,id="CMPRJ002") }

  # CMPRJ003 : Ongoing medication missing end flag (CMENRTPT)
  if (isTRUE(active_rules["CMPRJ003"])) {
    message("  Running CMPRJ003 - Ongoing medication missing CMENRTPT ...")
    if (!"CMENRTPT" %in% names(cm)) {
      message("  NOTE: CMENRTPT column not found - skipping CMPRJ003.")
    } else {
      CMPRJ003 <- cm |>
        dplyr::filter(is.na(CMENDTC)|trimws(CMENDTC)=="",
                      is.na(CMENRTPT)|trimws(CMENRTPT)=="") |>
        dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
          description=paste0("Ongoing medication has no end date and CMENRTPT is also missing for: ",
            ifelse(is.na(CMTRT),"[CMTRT missing]",CMTRT))) |>
        dplyr::select(subj_id,vis_id,description)
      state <- collect_findings(state,CMPRJ003,id="CMPRJ003") } }
  state
}
