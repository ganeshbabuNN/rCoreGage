# ==============================================================================
# PC.R  -  Pharmacokinetics Concentrations  -  Trial level checks
# PCCHK001 : Missing concentration result (PCORRES)
# PCCHK002 : Negative concentration value (PCORRES < 0)
# PCCHK003 : Missing sample collection date (PCDTC)
# Batch_ID : PC
# ==============================================================================
run_PC <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$pc) || nrow(data_list$pc)==0) {
    message("  WARNING [PC]: data_list$pc is empty - skipping.") ; return(ctx) }
  pc <- data_list$pc

  if (isTRUE(switches["PCCHK001"])) {
    message("  Running PCCHK001 - Missing PCORRES ...")
    PCCHK001 <- pc |>
      dplyr::filter(is.na(PCORRES)|trimws(PCORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Concentration result (PCORRES) is missing for test: ",
          PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PCCHK001,id="PCCHK001") }

  if (isTRUE(switches["PCCHK002"])) {
    message("  Running PCCHK002 - Negative concentration value ...")
    PCCHK002 <- pc |>
      dplyr::filter(!is.na(PCORRES),trimws(PCORRES)!="") |>
      dplyr::mutate(val=suppressWarnings(as.numeric(PCORRES))) |>
      dplyr::filter(!is.na(val),val<0) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Concentration value (PCORRES=",PCORRES,
          ") is negative for test: ",PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PCCHK002,id="PCCHK002") }

  if (isTRUE(switches["PCCHK003"])) {
    message("  Running PCCHK003 - Missing PCDTC ...")
    PCCHK003 <- pc |>
      dplyr::filter(is.na(PCDTC)|trimws(PCDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Sample collection date (PCDTC) is missing for test: ",
          PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PCCHK003,id="PCCHK003") }
  ctx
}
