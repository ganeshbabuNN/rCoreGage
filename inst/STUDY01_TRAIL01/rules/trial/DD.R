# ==============================================================================
# DD.R  -  Death Details  -  Trial level checks
# DDCHK001 : Missing date of death (DDDTHDTC)
# DDCHK002 : Missing cause of death term (DDTERM)
# DDCHK003 : Missing death flag (DTHFL)
# Rule_Set  : DD
# ==============================================================================
check_DD <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$dd) || nrow(domains$dd)==0) {
    message("  WARNING [DD]: domains$dd is empty - skipping.") ; return(state) }
  dd <- domains$dd

  if (isTRUE(active_rules["DDCHK001"])) {
    message("  Running DDCHK001 - Missing DDDTHDTC ...")
    DDCHK001 <- dd |>
      dplyr::filter(is.na(DDDTHDTC)|trimws(DDDTHDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Date of death (DDDTHDTC) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DDCHK001,id="DDCHK001") }

  if (isTRUE(active_rules["DDCHK002"])) {
    message("  Running DDCHK002 - Missing DDTERM ...")
    DDCHK002 <- dd |>
      dplyr::filter(is.na(DDTERM)|trimws(DDTERM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Cause of death term (DDTERM) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DDCHK002,id="DDCHK002") }

  if (isTRUE(active_rules["DDCHK003"])) {
    message("  Running DDCHK003 - Missing DTHFL ...")
    DDCHK003 <- dd |>
      dplyr::filter(is.na(DTHFL)|trimws(DTHFL)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Death flag (DTHFL) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,DDCHK003,id="DDCHK003") }
  state
}
