# ==============================================================================
# DS.R  -  Disposition  -  Trial level checks
# DSCHK001 : Missing disposition date (DSSTDTC)
# DSCHK002 : Missing disposition decision (DSDECOD)
# DSCHK003 : Missing disposition term (DSTERM)
# Rule_Set  : DS
# ==============================================================================
check_DS <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$ds) || nrow(domains$ds)==0) {
    message("  WARNING [DS]: domains$ds is empty - skipping.") ; return(state) }
  ds <- domains$ds

  if (isTRUE(active_rules["DSCHK001"])) {
    message("  Running DSCHK001 - Missing DSSTDTC ...")
    DSCHK001 <- ds |>
      dplyr::filter(is.na(DSSTDTC)|trimws(DSSTDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Disposition date (DSSTDTC) is missing for disposition: ",
          ifelse(is.na(DSDECOD),"[DSDECOD missing]",DSDECOD))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DSCHK001,id="DSCHK001") }

  if (isTRUE(active_rules["DSCHK002"])) {
    message("  Running DSCHK002 - Missing DSDECOD ...")
    DSCHK002 <- ds |>
      dplyr::filter(is.na(DSDECOD)|trimws(DSDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Disposition decision (DSDECOD) is missing in the disposition record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DSCHK002,id="DSCHK002") }

  if (isTRUE(active_rules["DSCHK003"])) {
    message("  Running DSCHK003 - Missing DSTERM ...")
    DSCHK003 <- ds |>
      dplyr::filter(is.na(DSTERM)|trimws(DSTERM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Disposition verbatim term (DSTERM) is missing in the disposition record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DSCHK003,id="DSCHK003") }
  state
}
