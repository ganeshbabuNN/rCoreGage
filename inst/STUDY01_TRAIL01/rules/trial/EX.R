# ==============================================================================
# EX.R  -  Exposure  -  Trial level checks
# EXCHK001 : Exposure end date before start date
# EXCHK002 : Missing dose amount (EXDOSE)
# EXCHK003 : Missing route of administration (EXROUTE)
# Rule_Set  : EX
# ==============================================================================
check_EX <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$ex) || nrow(domains$ex)==0) {
    message("  WARNING [EX]: domains$ex is empty - skipping.") ; return(state) }
  ex <- domains$ex

  # EXCHK001 : Exposure end date before start date
  if (isTRUE(active_rules["EXCHK001"])) {
    message("  Running EXCHK001 - EX end date before start date ...")
    EXCHK001 <- ex |>
      dplyr::filter(!is.na(EXSTDTC),trimws(EXSTDTC)!="",!is.na(EXENDTC),trimws(EXENDTC)!="") |>
      dplyr::mutate(st=as.Date(EXSTDTC),en=as.Date(EXENDTC)) |>
      dplyr::filter(!is.na(st),!is.na(en),en<st) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Exposure end date (",format(en,"%d%b%Y"),
          ") is before start date (",format(st,"%d%b%Y"),
          ") for treatment: ",EXTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,EXCHK001,id="EXCHK001") }

  # EXCHK002 : Missing dose amount (EXDOSE)
  if (isTRUE(active_rules["EXCHK002"])) {
    message("  Running EXCHK002 - Missing EXDOSE ...")
    EXCHK002 <- ex |>
      dplyr::filter(is.na(EXDOSE)|trimws(EXDOSE)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Dose amount (EXDOSE) is missing for treatment: ",EXTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,EXCHK002,id="EXCHK002") }

  # EXCHK003 : Missing route of administration (EXROUTE)
  if (isTRUE(active_rules["EXCHK003"])) {
    message("  Running EXCHK003 - Missing EXROUTE ...")
    EXCHK003 <- ex |>
      dplyr::filter(is.na(EXROUTE)|trimws(EXROUTE)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Route of administration (EXROUTE) is missing for treatment: ",
          EXTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,EXCHK003,id="EXCHK003") }
  state
}
