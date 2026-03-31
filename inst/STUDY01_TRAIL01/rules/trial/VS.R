# ==============================================================================
# VS.R  -  Vital Signs  -  Trial level checks
# VSCHK001 : Systolic BP not greater than diastolic BP
# VSCHK002 : Missing vital sign result (VSORRES)
# VSCHK003 : Vital sign value outside normal reference range
# Rule_Set  : VS
# ==============================================================================
check_VS <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$vs) || nrow(domains$vs)==0) {
    message("  WARNING [VS]: domains$vs is empty - skipping.") ; return(state) }
  vs <- domains$vs

  if (isTRUE(active_rules["VSCHK001"])) {
    message("  Running VSCHK001 - Systolic BP not greater than diastolic BP ...")
    sysbp <- vs |> dplyr::filter(VSTESTCD=="SYSBP",!is.na(VSORRES),trimws(VSORRES)!="") |>
      dplyr::select(USUBJID,VISITNUM,VISIT,VSDTC,sysbp=VSORRES)
    diabp <- vs |> dplyr::filter(VSTESTCD=="DIABP",!is.na(VSORRES),trimws(VSORRES)!="") |>
      dplyr::select(USUBJID,VISITNUM,VISIT,VSDTC,diabp=VSORRES)
    VSCHK001 <- sysbp |>
      dplyr::inner_join(diabp,by=c("USUBJID","VISITNUM","VISIT","VSDTC")) |>
      dplyr::mutate(s=suppressWarnings(as.numeric(sysbp)),d=suppressWarnings(as.numeric(diabp))) |>
      dplyr::filter(!is.na(s),!is.na(d),s<=d) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Systolic BP (",sysbp," mmHg) is not greater than",
          " diastolic BP (",diabp," mmHg) at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,VSCHK001,id="VSCHK001") }

  if (isTRUE(active_rules["VSCHK002"])) {
    message("  Running VSCHK002 - Missing VSORRES ...")
    VSCHK002 <- vs |>
      dplyr::filter(is.na(VSORRES)|trimws(VSORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Vital sign result (VSORRES) is missing for test: ",
          VSTEST," (",VSTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,VSCHK002,id="VSCHK002") }

  if (isTRUE(active_rules["VSCHK003"])) {
    message("  Running VSCHK003 - Value outside normal range ...")
    VSCHK003 <- vs |>
      dplyr::filter(!is.na(VSORRES),trimws(VSORRES)!="",
                    !is.na(VSSTNRLO),trimws(VSSTNRLO)!="",
                    !is.na(VSSTNRHI),trimws(VSSTNRHI)!="") |>
      dplyr::mutate(val=suppressWarnings(as.numeric(VSORRES)),
                    lo=suppressWarnings(as.numeric(VSSTNRLO)),
                    hi=suppressWarnings(as.numeric(VSSTNRHI))) |>
      dplyr::filter(!is.na(val),!is.na(lo),!is.na(hi),val<lo|val>hi) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0(VSTEST," (",VSTESTCD,") = ",val," ",VSORRESU,
          " outside normal range [",lo," - ",hi,"] at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,VSCHK003,id="VSCHK003") }
  state
}
