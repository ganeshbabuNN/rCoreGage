# ==============================================================================
# PC.R  -  Pharmacokinetics Concentrations  -  Trial level checks
# PCCHK001 : Missing concentration result (PCORRES)
# PCCHK002 : Negative concentration value (PCORRES < 0)
# PCCHK003 : Missing sample collection date (PCDTC)
# Rule_Set  : PC
# ==============================================================================
check_PC <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$pc) || nrow(domains$pc)==0) {
    message("  WARNING [PC]: domains$pc is empty - skipping.") ; return(state) }
  pc <- domains$pc

  # PCCHK001 : Missing concentration result (PCORRES)
  if (isTRUE(active_rules["PCCHK001"])) {
    message("  Running PCCHK001 - Missing PCORRES ...")
    PCCHK001 <- pc |>
      dplyr::filter(is.na(PCORRES)|trimws(PCORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Concentration result (PCORRES) is missing for test: ",
          PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PCCHK001,id="PCCHK001") }

  # PCCHK002 : Negative concentration value (PCORRES < 0)
  if (isTRUE(active_rules["PCCHK002"])) {
    message("  Running PCCHK002 - Negative concentration value ...")
    PCCHK002 <- pc |>
      dplyr::filter(!is.na(PCORRES),trimws(PCORRES)!="") |>
      dplyr::mutate(val=suppressWarnings(as.numeric(PCORRES))) |>
      dplyr::filter(!is.na(val),val<0) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Concentration value (PCORRES=",PCORRES,
          ") is negative for test: ",PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PCCHK002,id="PCCHK002") }

  # PCCHK003 : Missing sample collection date (PCDTC)
  if (isTRUE(active_rules["PCCHK003"])) {
    message("  Running PCCHK003 - Missing PCDTC ...")
    PCCHK003 <- pc |>
      dplyr::filter(is.na(PCDTC)|trimws(PCDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Sample collection date (PCDTC) is missing for test: ",
          PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PCCHK003,id="PCCHK003") }
  state
}
