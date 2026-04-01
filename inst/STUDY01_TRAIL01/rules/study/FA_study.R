# ==============================================================================
# FA_study.R  -  Findings About Events  -  Project level checks
# FAPRJ001 : Missing parent domain reference object (FAOBJ)
# FAPRJ002 : Missing test code (FATESTCD)
# FAPRJ003 : Missing finding status (FASTAT)
# Rule_Set  : FA_PRJ
# ==============================================================================
check_FA_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$fa) || nrow(domains$fa)==0) {
    message("  WARNING [FA_study]: domains$fa is empty - skipping.") ; return(state) }
  fa <- domains$fa

  # FAPRJ001 : Missing parent domain reference object (FAOBJ)
  if (isTRUE(active_rules["FAPRJ001"])) {
    message("  Running FAPRJ001 - Missing FAOBJ ...")
    FAPRJ001 <- fa |>
      dplyr::filter(is.na(FAOBJ)|trimws(FAOBJ)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Parent domain reference object (FAOBJ) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,FAPRJ001,id="FAPRJ001") }
  
  # FAPRJ002 : Missing test code (FATESTCD)
  if (isTRUE(active_rules["FAPRJ002"])) {
    message("  Running FAPRJ002 - Missing FATESTCD ...")
    FAPRJ002 <- fa |>
      dplyr::filter(is.na(FATESTCD)|trimws(FATESTCD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Finding test code (FATESTCD) is missing in the findings about record") |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,FAPRJ002,id="FAPRJ002") }

  # FAPRJ003 : Missing finding status (FASTAT)
  if (isTRUE(active_rules["FAPRJ003"])) {
    message("  Running FAPRJ003 - Missing FASTAT ...")
    FAPRJ003 <- fa |>
      dplyr::filter(is.na(FASTAT)|trimws(FASTAT)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Finding status (FASTAT) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,FAPRJ003,id="FAPRJ003") }
  state
}
