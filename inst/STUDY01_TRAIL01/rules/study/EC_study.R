# ==============================================================================
# EC_study.R  -  Exposure as Collected  -  Project level checks
# ECPRJ001 : ECOCCUR occurrence flag missing
# ECPRJ002 : Dose exceeds protocol maximum (40 mg)
# ECPRJ003 : Reason not provided when ECOCCUR = N
# Rule_Set  : EC_PRJ
# ==============================================================================
check_EC_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  max_dose_mg <- 40
  if (is.null(domains$ec) || nrow(domains$ec)==0) {
    message("  WARNING [EC_study]: domains$ec is empty - skipping.") ; return(state)
  }
  ec <- domains$ec

  if (isTRUE(active_rules["ECPRJ001"])) {
    message("  Running ECPRJ001 - ECOCCUR flag missing ...")
    if (!"ECOCCUR" %in% names(ec)) {
      message("  NOTE: ECOCCUR column not found - skipping ECPRJ001.")
    } else {
      ECPRJ001 <- ec |>
        dplyr::filter(is.na(ECOCCUR) | trimws(ECOCCUR)=="") |>
        dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
          description=paste0("Occurrence flag (ECOCCUR) is missing",
            " for treatment: ",ECTRT," at visit ",VISIT)) |>
        dplyr::select(subj_id, vis_id, description)
      state <- collect_findings(state, ECPRJ001, id="ECPRJ001")
    }
  }
  if (isTRUE(active_rules["ECPRJ002"])) {
    message("  Running ECPRJ002 - Dose exceeds protocol maximum (",max_dose_mg,"mg) ...")
    ECPRJ002 <- ec |>
      dplyr::filter(!is.na(ECDOSE), trimws(ECDOSE)!="") |>
      dplyr::mutate(dose_num=suppressWarnings(as.numeric(ECDOSE))) |>
      dplyr::filter(!is.na(dose_num), dose_num > max_dose_mg) |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Dose (ECDOSE=",ECDOSE," ",ECDOSU,")",
          " exceeds protocol maximum of ",max_dose_mg," mg",
          " for treatment: ",ECTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, ECPRJ002, id="ECPRJ002")
  }
  if (isTRUE(active_rules["ECPRJ003"])) {
    message("  Running ECPRJ003 - Reason missing when ECOCCUR = N ...")
    if (!all(c("ECOCCUR","ECREASND") %in% names(ec))) {
      message("  NOTE: ECOCCUR or ECREASND column not found - skipping ECPRJ003.")
    } else {
      ECPRJ003 <- ec |>
        dplyr::filter(!is.na(ECOCCUR), toupper(trimws(ECOCCUR))=="N",
                      is.na(ECREASND) | trimws(ECREASND)=="") |>
        dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
          description=paste0("Treatment not given (ECOCCUR=N)",
            " but reason (ECREASND) is missing",
            " for treatment: ",ECTRT," at visit ",VISIT)) |>
        dplyr::select(subj_id, vis_id, description)
      state <- collect_findings(state, ECPRJ003, id="ECPRJ003")
    }
  }
  state
}
