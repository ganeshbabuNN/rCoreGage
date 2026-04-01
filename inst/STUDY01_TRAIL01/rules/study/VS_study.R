# ==============================================================================
# VS_study.R  -  Vital Signs  -  Project level checks
# VSPRJ001 : Missing vital sign collection date (VSDTC)
# VSPRJ002 : Duplicate vital sign records same subject/visit/test
# VSPRJ003 : Invalid position (VSPOS) value
# Rule_Set  : VS_PRJ
# ==============================================================================
check_VS_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  allowed_pos <- c("STANDING","SITTING","SUPINE","PRONE","ORIGINAL")
  if (is.null(domains$vs) || nrow(domains$vs)==0) {
    message("  WARNING [VS_study]: domains$vs is empty - skipping.") ; return(state) }
  vs <- domains$vs

  # VSPRJ001 : Missing vital sign collection date (VSDTC)
  if (isTRUE(active_rules["VSPRJ001"])) {
    message("  Running VSPRJ001 - Missing VSDTC ...")
    VSPRJ001 <- vs |>
      dplyr::filter(is.na(VSDTC)|trimws(VSDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Collection date (VSDTC) is missing for test: ",
          VSTEST," (",VSTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,VSPRJ001,id="VSPRJ001") }

  # VSPRJ002 : Duplicate vital sign records same subject/visit/test
  if (isTRUE(active_rules["VSPRJ002"])) {
    message("  Running VSPRJ002 - Duplicate VS records ...")
    VSPRJ002 <- vs |>
      dplyr::group_by(USUBJID,VISITNUM,VSTESTCD) |>
      dplyr::filter(dplyr::n()>1) |>
      dplyr::ungroup() |>
      dplyr::distinct(USUBJID,VISITNUM,VSTESTCD,.keep_all=TRUE) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Duplicate vital sign records for ",VSTEST,
          " (",VSTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,VSPRJ002,id="VSPRJ002") }

  # VSPRJ003 : Invalid position (VSPOS) value
  if (isTRUE(active_rules["VSPRJ003"])) {
    message("  Running VSPRJ003 - Invalid VSPOS ...")
    if (!"VSPOS" %in% names(vs)) {
      message("  NOTE: VSPOS column not found - skipping VSPRJ003.")
    } else {
      VSPRJ003 <- vs |>
        dplyr::filter(!is.na(VSPOS),trimws(VSPOS)!="",
                      !toupper(trimws(VSPOS)) %in% toupper(allowed_pos)) |>
        dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
          description=paste0("Position value '",VSPOS,
            "' is not in allowed list (",paste(allowed_pos,collapse="/"),
            ") for test: ",VSTEST," at visit ",VISIT)) |>
        dplyr::select(subj_id,vis_id,description)
      state <- collect_findings(state,VSPRJ003,id="VSPRJ003") } }
  state
}
