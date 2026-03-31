# ==============================================================================
# FA.R  -  Findings About Events  -  Trial level checks
# FACHK001 : Missing finding result (FAORRES)
# FACHK002 : Missing finding date (FADTC)
# FACHK003 : Missing evaluator flag (FAEVAL)
# Rule_Set  : FA
# ==============================================================================
check_FA <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$fa) || nrow(domains$fa)==0) {
    message("  WARNING [FA]: domains$fa is empty - skipping.") ; return(state) }
  fa <- domains$fa

  if (isTRUE(active_rules["FACHK001"])) {
    message("  Running FACHK001 - Missing FAORRES ...")
    FACHK001 <- fa |>
      dplyr::filter(is.na(FAORRES)|trimws(FAORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Finding result (FAORRES) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,FACHK001,id="FACHK001") }

  if (isTRUE(active_rules["FACHK002"])) {
    message("  Running FACHK002 - Missing FADTC ...")
    FACHK002 <- fa |>
      dplyr::filter(is.na(FADTC)|trimws(FADTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Finding date (FADTC) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,FACHK002,id="FACHK002") }

  if (isTRUE(active_rules["FACHK003"])) {
    message("  Running FACHK003 - Missing FAEVAL ...")
    FACHK003 <- fa |>
      dplyr::filter(is.na(FAEVAL)|trimws(FAEVAL)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Evaluator flag (FAEVAL) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,FACHK003,id="FACHK003") }
  state
}
