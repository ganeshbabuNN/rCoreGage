# ==============================================================================
# DV_study.R  -  Protocol Deviations  -  Project level checks
# DVPRJ001 : Missing reason for deviation (DVREASND)
# DVPRJ002 : Missing dictionary coded term (DVDECOD)
# DVPRJ003 : Missing site ID (SITEID)
# Rule_Set  : DV_PRJ
# ==============================================================================
check_DV_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$dv) || nrow(domains$dv)==0) {
    message("  WARNING [DV_study]: domains$dv is empty - skipping.") ; return(state) }
  dv <- domains$dv

  if (isTRUE(active_rules["DVPRJ001"])) {
    message("  Running DVPRJ001 - Missing DVREASND ...")
    DVPRJ001 <- dv |>
      dplyr::filter(is.na(DVREASND)|trimws(DVREASND)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Reason for deviation (DVREASND) is missing for: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DVPRJ001,id="DVPRJ001") }

  if (isTRUE(active_rules["DVPRJ002"])) {
    message("  Running DVPRJ002 - Missing DVDECOD ...")
    DVPRJ002 <- dv |>
      dplyr::filter(!is.na(DVTERM),trimws(DVTERM)!="",is.na(DVDECOD)|trimws(DVDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Dictionary coded term (DVDECOD) is missing for deviation: ",DVTERM)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DVPRJ002,id="DVPRJ002") }

  if (isTRUE(active_rules["DVPRJ003"])) {
    message("  Running DVPRJ003 - Missing SITEID ...")
    DVPRJ003 <- dv |>
      dplyr::filter(is.na(SITEID)|trimws(SITEID)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Site ID (SITEID) is missing for deviation: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DVPRJ003,id="DVPRJ003") }
  state
}
