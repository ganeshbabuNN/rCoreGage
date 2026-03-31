# ==============================================================================
# MH.R  -  Medical History  -  Trial level checks
# MHCHK001 : Missing medical history term (MHTERM)
# MHCHK002 : Medical history end date before start date
# MHCHK003 : Missing onset date (MHSTDTC)
# Batch_ID : MH
# ==============================================================================
run_MH <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$mh) || nrow(data_list$mh)==0) {
    message("  WARNING [MH]: data_list$mh is empty - skipping.") ; return(ctx) }
  mh <- data_list$mh

  if (isTRUE(switches["MHCHK001"])) {
    message("  Running MHCHK001 - Missing MHTERM ...")
    MHCHK001 <- mh |>
      dplyr::filter(is.na(MHTERM)|trimws(MHTERM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Medical history term (MHTERM) is missing in the medical history record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,MHCHK001,id="MHCHK001") }

  if (isTRUE(switches["MHCHK002"])) {
    message("  Running MHCHK002 - MH end date before start date ...")
    MHCHK002 <- mh |>
      dplyr::filter(!is.na(MHSTDTC),trimws(MHSTDTC)!="",!is.na(MHENDTC),trimws(MHENDTC)!="") |>
      dplyr::mutate(st=as.Date(MHSTDTC),en=as.Date(MHENDTC)) |>
      dplyr::filter(!is.na(st),!is.na(en),en<st) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Medical history end date (",format(en,"%d%b%Y"),
          ") is before onset date (",format(st,"%d%b%Y"),") for condition: ",
          ifelse(is.na(MHTERM),"[MHTERM missing]",MHTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,MHCHK002,id="MHCHK002") }

  if (isTRUE(switches["MHCHK003"])) {
    message("  Running MHCHK003 - Missing MHSTDTC ...")
    MHCHK003 <- mh |>
      dplyr::filter(is.na(MHSTDTC)|trimws(MHSTDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Onset date (MHSTDTC) is missing for condition: ",
          ifelse(is.na(MHTERM),"[MHTERM missing]",MHTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,MHCHK003,id="MHCHK003") }
  ctx
}
