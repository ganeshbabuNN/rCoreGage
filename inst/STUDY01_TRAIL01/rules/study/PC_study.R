# ==============================================================================
# PC_study.R  -  Pharmacokinetics Concentrations  -  Project level checks
# PCPRJ001 : Missing specimen type (PCSPEC)
# PCPRJ002 : Missing concentration units (PCSTRESU)
# PCPRJ003 : Duplicate records for same subject, visit, and test
# Rule_Set  : PC_PRJ
# ==============================================================================
check_PC_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$pc) || nrow(domains$pc)==0) {
    message("  WARNING [PC_study]: domains$pc is empty - skipping.") ; return(state) }
  pc <- domains$pc

  # PCPRJ001 : Missing specimen type (PCSPEC)
  if (isTRUE(active_rules["PCPRJ001"])) {
    message("  Running PCPRJ001 - Missing PCSPEC ...")
    PCPRJ001 <- pc |>
      dplyr::filter(is.na(PCSPEC)|trimws(PCSPEC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Specimen type (PCSPEC) is missing for test: ",
          PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PCPRJ001,id="PCPRJ001") }

  # PCPRJ002 : Missing concentration units (PCSTRESU)
  if (isTRUE(active_rules["PCPRJ002"])) {
    message("  Running PCPRJ002 - Missing PCSTRESU ...")
    PCPRJ002 <- pc |>
      dplyr::filter(!is.na(PCORRES),trimws(PCORRES)!="",
                    is.na(PCSTRESU)|trimws(PCSTRESU)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Concentration units (PCSTRESU) are missing although",
          " result (PCORRES=",PCORRES,") is present for test: ",PCTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PCPRJ002,id="PCPRJ002") }

  # PCPRJ003 : Duplicate records for same subject, visit, and test
  if (isTRUE(active_rules["PCPRJ003"])) {
    message("  Running PCPRJ003 - Duplicate PC records ...")
    PCPRJ003 <- pc |>
      dplyr::group_by(USUBJID,VISITNUM,PCTESTCD) |>
      dplyr::filter(dplyr::n()>1) |>
      dplyr::ungroup() |>
      dplyr::distinct(USUBJID,VISITNUM,PCTESTCD,.keep_all=TRUE) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Duplicate records found for test: ",PCTESTCD,
          " at visit ",VISIT," - more than one result recorded")) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PCPRJ003,id="PCPRJ003") }
  state
}
