# ==============================================================================
# DV.R  -  Protocol Deviations  -  Trial level checks
# DVCHK001 : Missing deviation date (DVSTDTC)
# DVCHK002 : Missing deviation term (DVTERM)
# DVCHK003 : Missing deviation category (DVCAT)
# Rule_Set  : DV
# ==============================================================================
check_DV <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$dv) || nrow(domains$dv)==0) {
    message("  WARNING [DV]: domains$dv is empty - skipping.") ; return(state) }
  dv <- domains$dv

  # DVCHK001 : Missing deviation date (DVSTDTC)
  if (isTRUE(active_rules["DVCHK001"])) {
    message("  Running DVCHK001 - Missing DVSTDTC ...")
    DVCHK001 <- dv |>
      dplyr::filter(is.na(DVSTDTC)|trimws(DVSTDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Deviation date (DVSTDTC) is missing for deviation: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DVCHK001,id="DVCHK001") }

  # DVCHK002 : Missing deviation term (DVTERM)
  if (isTRUE(active_rules["DVCHK002"])) {
    message("  Running DVCHK002 - Missing DVTERM ...")
    DVCHK002 <- dv |>
      dplyr::filter(is.na(DVTERM)|trimws(DVTERM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Protocol deviation term (DVTERM) is missing in the deviation record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DVCHK002,id="DVCHK002") }

  # DVCHK003 : Missing deviation category (DVCAT)
  if (isTRUE(active_rules["DVCHK003"])) {
    message("  Running DVCHK003 - Missing DVCAT ...")
    DVCHK003 <- dv |>
      dplyr::filter(is.na(DVCAT)|trimws(DVCAT)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Deviation category (DVCAT) is missing for deviation: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DVCHK003,id="DVCHK003") }
  state
}
