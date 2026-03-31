# ==============================================================================
# EX_study.R  -  Exposure  -  Project level checks
# EXPRJ001 : Invalid dose form - not in allowed list (TABLET/CAPSULE)
# EXPRJ002 : Administered dose exceeds protocol maximum (40 mg)
# EXPRJ003 : Missing administration status (EXSTAT)
# Rule_Set  : EX_PRJ
# ==============================================================================
check_EX_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  allowed_forms <- c("TABLET","CAPSULE")
  max_dose_mg   <- 40
  if (is.null(domains$ex) || nrow(domains$ex)==0) {
    message("  WARNING [EX_study]: domains$ex is empty - skipping.") ; return(state) }
  ex <- domains$ex

  if (isTRUE(active_rules["EXPRJ001"])) {
    message("  Running EXPRJ001 - Invalid dose form ...")
    EXPRJ001 <- ex |>
      dplyr::filter(!is.na(EXDOSFRM),trimws(EXDOSFRM)!="",
                    !toupper(trimws(EXDOSFRM)) %in% toupper(allowed_forms)) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Dose form '",EXDOSFRM,"' not in allowed list (",
          paste(allowed_forms,collapse="/"),") for treatment: ",EXTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,EXPRJ001,id="EXPRJ001") }

  if (isTRUE(active_rules["EXPRJ002"])) {
    message("  Running EXPRJ002 - Dose exceeds protocol maximum (",max_dose_mg,"mg) ...")
    EXPRJ002 <- ex |>
      dplyr::filter(!is.na(EXDOSE),trimws(EXDOSE)!="") |>
      dplyr::mutate(dose_num=suppressWarnings(as.numeric(EXDOSE))) |>
      dplyr::filter(!is.na(dose_num),dose_num>max_dose_mg) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Dose (EXDOSE=",EXDOSE," ",EXDOSU,") exceeds protocol maximum of ",
          max_dose_mg," mg for treatment: ",EXTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,EXPRJ002,id="EXPRJ002") }

  if (isTRUE(active_rules["EXPRJ003"])) {
    message("  Running EXPRJ003 - Missing EXSTAT ...")
    EXPRJ003 <- ex |>
      dplyr::filter(is.na(EXSTAT)|trimws(EXSTAT)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Administration status (EXSTAT) is missing for treatment: ",
          EXTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,EXPRJ003,id="EXPRJ003") }
  state
}
