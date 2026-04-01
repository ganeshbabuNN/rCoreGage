# ==============================================================================
# DS_study.R  -  Disposition  -  Project level checks
# DSPRJ001 : Invalid epoch value (DSEPOCH)
# DSPRJ002 : Duplicate primary disposition records per subject
# DSPRJ003 : Missing site ID (SITEID)
# Rule_Set  : DS_PRJ
# ==============================================================================
check_DS_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  allowed_epochs <- c("SCREENING","TREATMENT","FOLLOW-UP")
  if (is.null(domains$ds) || nrow(domains$ds)==0) {
    message("  WARNING [DS_study]: domains$ds is empty - skipping.") ; return(state) }
  ds <- domains$ds

  # DSPRJ001 : Invalid epoch value (DSEPOCH)
  if (isTRUE(active_rules["DSPRJ001"])) {
    message("  Running DSPRJ001 - Invalid DSEPOCH ...")
    DSPRJ001 <- ds |>
      dplyr::filter(!is.na(DSEPOCH),trimws(DSEPOCH)!="",
                    !toupper(trimws(DSEPOCH)) %in% toupper(allowed_epochs)) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Epoch value '",DSEPOCH,"' is not in the allowed list (",
          paste(allowed_epochs,collapse="/"),")")) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DSPRJ001,id="DSPRJ001") }
  
  # DSPRJ002 : Duplicate primary disposition records per subject
  if (isTRUE(active_rules["DSPRJ002"])) {
    message("  Running DSPRJ002 - Duplicate primary disposition records ...")
    DSPRJ002 <- ds |>
      dplyr::filter(!is.na(DSSCAT),toupper(trimws(DSSCAT))=="PRIMARY") |>
      dplyr::group_by(USUBJID) |>
      dplyr::filter(dplyr::n()>1) |>
      dplyr::ungroup() |>
      dplyr::distinct(USUBJID,.keep_all=TRUE) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="More than one primary disposition (DSSCAT=PRIMARY) record found for subject") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DSPRJ002,id="DSPRJ002") }

  # DSPRJ003 : Missing site ID (SITEID)
  if (isTRUE(active_rules["DSPRJ003"])) {
    message("  Running DSPRJ003 - Missing SITEID ...")
    DSPRJ003 <- ds |>
      dplyr::filter(is.na(SITEID)|trimws(SITEID)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Site ID (SITEID) is missing for disposition: ",
          ifelse(is.na(DSDECOD),"[DSDECOD missing]",DSDECOD))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DSPRJ003,id="DSPRJ003") }
  state
}
