# ==============================================================================
# CM_PRJ.R  -  Concomitant Medications  -  Project level checks
# CMPRJ001 : Missing route of administration (CMROUTE)
# CMPRJ002 : Missing dictionary coded term (CMDECOD)
# CMPRJ003 : Ongoing medication missing end flag (CMENRTPT)
# Batch_ID : CM_PRJ
# ==============================================================================
run_CM_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$cm) || nrow(data_list$cm)==0) {
    message("  WARNING [CM_PRJ]: data_list$cm is empty - skipping.") ; return(ctx) }
  cm <- data_list$cm

  if (isTRUE(switches["CMPRJ001"])) {
    message("  Running CMPRJ001 - Missing CMROUTE ...")
    CMPRJ001 <- cm |>
      dplyr::filter(is.na(CMROUTE)|trimws(CMROUTE)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Route of administration (CMROUTE) is missing for medication: ",
          ifelse(is.na(CMTRT),"[CMTRT missing]",CMTRT))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,CMPRJ001,id="CMPRJ001") }

  if (isTRUE(switches["CMPRJ002"])) {
    message("  Running CMPRJ002 - Missing CMDECOD ...")
    CMPRJ002 <- cm |>
      dplyr::filter(!is.na(CMTRT),trimws(CMTRT)!="",is.na(CMDECOD)|trimws(CMDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Dictionary coded term (CMDECOD) is missing for medication: ",CMTRT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,CMPRJ002,id="CMPRJ002") }

  if (isTRUE(switches["CMPRJ003"])) {
    message("  Running CMPRJ003 - Ongoing medication missing CMENRTPT ...")
    if (!"CMENRTPT" %in% names(cm)) {
      message("  NOTE: CMENRTPT column not found - skipping CMPRJ003.")
    } else {
      CMPRJ003 <- cm |>
        dplyr::filter(is.na(CMENDTC)|trimws(CMENDTC)=="",
                      is.na(CMENRTPT)|trimws(CMENRTPT)=="") |>
        dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
          description=paste0("Ongoing medication has no end date and CMENRTPT is also missing for: ",
            ifelse(is.na(CMTRT),"[CMTRT missing]",CMTRT))) |>
        dplyr::select(subj_id,vis_id,description)
      ctx <- prepare(ctx,CMPRJ003,id="CMPRJ003") } }
  ctx
}
