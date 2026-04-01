# ==============================================================================
# PP.R  -  Pharmacokinetics Parameters  -  Trial level checks
# PPCHK001 : Missing PK parameter result (PPORRES)
# PPCHK002 : Negative PK parameter value
# PPCHK003 : Missing analysis date (PPDTC)
# Rule_Set  : PP
# ==============================================================================
check_PP <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$pp) || nrow(domains$pp)==0) {
    message("  WARNING [PP]: domains$pp is empty - skipping.") ; return(state) }
  pp <- domains$pp

  # PPCHK001 : Missing PK parameter result (PPORRES)
  if (isTRUE(active_rules["PPCHK001"])) {
    message("  Running PPCHK001 - Missing PPORRES ...")
    PPCHK001 <- pp |>
      dplyr::filter(is.na(PPORRES)|trimws(PPORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("PK parameter result (PPORRES) is missing for: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PPCHK001,id="PPCHK001") }

  # PPCHK002 : Negative PK parameter value
  if (isTRUE(active_rules["PPCHK002"])) {
    message("  Running PPCHK002 - Negative PK parameter value ...")
    PPCHK002 <- pp |>
      dplyr::filter(!is.na(PPORRES),trimws(PPORRES)!="") |>
      dplyr::mutate(val=suppressWarnings(as.numeric(PPORRES))) |>
      dplyr::filter(!is.na(val),val<0) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("PK parameter value (PPORRES=",PPORRES,
          ") is negative for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PPCHK002,id="PPCHK002") }

  # PPCHK003 : Missing analysis date (PPDTC)
  if (isTRUE(active_rules["PPCHK003"])) {
    message("  Running PPCHK003 - Missing PPDTC ...")
    PPCHK003 <- pp |>
      dplyr::filter(is.na(PPDTC)|trimws(PPDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Analysis date (PPDTC) is missing for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PPCHK003,id="PPCHK003") }
  state
}
