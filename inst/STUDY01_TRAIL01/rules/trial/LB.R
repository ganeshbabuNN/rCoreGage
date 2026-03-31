# ==============================================================================
# LB.R  -  Laboratory Results  -  Trial level checks
# LBCHK001 : Lab value outside normal reference range
# LBCHK002 : Missing lab result value (LBORRES)
# LBCHK003 : Duplicate records for same subject/visit/test
# Rule_Set  : LB
# ==============================================================================
check_LB <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$lb) || nrow(domains$lb)==0) {
    message("  WARNING [LB]: domains$lb is empty - skipping.") ; return(state)
  }
  lb <- domains$lb

  # LBCHK001 : Lab value outside normal reference range
  if (isTRUE(active_rules["LBCHK001"])) {
    message("  Running LBCHK001 - Lab value outside normal range ...")
    LBCHK001 <- lb |>
      dplyr::filter(!is.na(LBORRES), trimws(LBORRES)!="",
                    !is.na(LBNRLO), trimws(LBNRLO)!="",
                    !is.na(LBNRHI), trimws(LBNRHI)!="") |>
      dplyr::mutate(val=suppressWarnings(as.numeric(LBORRES)),
                    lo=suppressWarnings(as.numeric(LBNRLO)),
                    hi=suppressWarnings(as.numeric(LBNRHI))) |>
      dplyr::filter(!is.na(val), !is.na(lo), !is.na(hi), val<lo | val>hi) |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0(LBTEST," (",LBTESTCD,") = ",val," ",LBORRESU,
          " outside normal range [",lo," - ",hi,"] at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, LBCHK001, id="LBCHK001")
  }
  
  # LBCHK002 : Missing lab result value (LBORRES)
  if (isTRUE(active_rules["LBCHK002"])) {
    message("  Running LBCHK002 - Missing lab result value ...")
    LBCHK002 <- lb |>
      dplyr::filter(is.na(LBORRES) | trimws(LBORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Lab result (LBORRES) is missing for test ",
          LBTEST," (",LBTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, LBCHK002, id="LBCHK002")
  }
  
  # LBCHK003 : Duplicate records for same subject/visit/test
  if (isTRUE(active_rules["LBCHK003"])) {
    message("  Running LBCHK003 - Duplicate lab records ...")
    LBCHK003 <- lb |>
      dplyr::group_by(USUBJID, VISITNUM, LBTESTCD) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup() |>
      dplyr::distinct(USUBJID, VISITNUM, LBTESTCD, .keep_all=TRUE) |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Duplicate records for test ",
          LBTEST," (",LBTESTCD,") at visit ",VISIT,
          " - more than one result recorded")) |>
      dplyr::select(subj_id, vis_id, description)
    state <- collect_findings(state, LBCHK003, id="LBCHK003")
  }
  state
}
