# ==============================================================================
# CM.R  -  Concomitant Medications  -  Trial level checks
# CMCHK001 : CM end date before start date
# CMCHK002 : Missing medication name (CMTRT)
# CMCHK003 : Missing indication (CMINDC)
# Batch_ID : CM
# ==============================================================================
run_CM <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$cm) || nrow(data_list$cm)==0) {
    message("  WARNING [CM]: data_list$cm is empty - skipping.") ; return(ctx) }
  cm <- data_list$cm

  if (isTRUE(switches["CMCHK001"])) {
    message("  Running CMCHK001 - CM end date before start date ...")
    CMCHK001 <- cm |>
      dplyr::filter(!is.na(CMSTDTC),trimws(CMSTDTC)!="",!is.na(CMENDTC),trimws(CMENDTC)!="") |>
      dplyr::mutate(st=as.Date(CMSTDTC),en=as.Date(CMENDTC)) |>
      dplyr::filter(!is.na(st),!is.na(en),en<st) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("CM end date (",format(en,"%d%b%Y"),") is before start date (",
          format(st,"%d%b%Y"),") for medication: ",CMTRT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,CMCHK001,id="CMCHK001") }

  if (isTRUE(switches["CMCHK002"])) {
    message("  Running CMCHK002 - Missing CMTRT ...")
    CMCHK002 <- cm |>
      dplyr::filter(is.na(CMTRT)|trimws(CMTRT)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Medication name (CMTRT) is missing in the concomitant medication record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,CMCHK002,id="CMCHK002") }

  if (isTRUE(switches["CMCHK003"])) {
    message("  Running CMCHK003 - Missing CMINDC ...")
    CMCHK003 <- cm |>
      dplyr::filter(is.na(CMINDC)|trimws(CMINDC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Indication (CMINDC) is missing for medication: ",
          ifelse(is.na(CMTRT),"[CMTRT missing]",CMTRT))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,CMCHK003,id="CMCHK003") }
  ctx
}
