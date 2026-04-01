# ==============================================================================
# MH_study.R  -  Medical History  -  Project level checks
# MHPRJ001 : Missing dictionary coded term (MHDECOD)
# MHPRJ002 : Invalid medical history status (MHSTAT)
# MHPRJ003 : Missing body system (MHBODSYS)
# Rule_Set  : MH_PRJ
# ==============================================================================
check_MH_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  allowed_stat <- c("ONGOING","RESOLVED","UNKNOWN")
  if (is.null(domains$mh) || nrow(domains$mh)==0) {
    message("  WARNING [MH_study]: domains$mh is empty - skipping.") ; return(state) }
  mh <- domains$mh

  # MHPRJ001 : Missing dictionary coded term (MHDECOD)
  if (isTRUE(active_rules["MHPRJ001"])) {
    message("  Running MHPRJ001 - Missing MHDECOD ...")
    MHPRJ001 <- mh |>
      dplyr::filter(!is.na(MHTERM),trimws(MHTERM)!="",is.na(MHDECOD)|trimws(MHDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Dictionary coded term (MHDECOD) is missing for condition: ",MHTERM)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,MHPRJ001,id="MHPRJ001") }

  # MHPRJ002 : Invalid medical history status (MHSTAT)
  if (isTRUE(active_rules["MHPRJ002"])) {
    message("  Running MHPRJ002 - Invalid MHSTAT ...")
    MHPRJ002 <- mh |>
      dplyr::filter(!is.na(MHSTAT),trimws(MHSTAT)!="",
                    !toupper(trimws(MHSTAT)) %in% toupper(allowed_stat)) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Medical history status '",MHSTAT,
          "' is not in allowed list (",paste(allowed_stat,collapse="/"),
          ") for condition: ",ifelse(is.na(MHTERM),"[MHTERM missing]",MHTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,MHPRJ002,id="MHPRJ002") }

  # MHPRJ003 : Missing body system (MHBODSYS)
  if (isTRUE(active_rules["MHPRJ003"])) {
    message("  Running MHPRJ003 - Missing MHBODSYS ...")
    MHPRJ003 <- mh |>
      dplyr::filter(is.na(MHBODSYS)|trimws(MHBODSYS)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Body system (MHBODSYS) is missing for condition: ",
          ifelse(is.na(MHTERM),"[MHTERM missing]",MHTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,MHPRJ003,id="MHPRJ003") }
  state
}
