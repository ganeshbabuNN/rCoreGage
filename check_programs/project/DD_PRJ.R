# ==============================================================================
# DD_PRJ.R  -  Death Details  -  Project level checks
# DDPRJ001 : Missing coded cause of death (DDDECOD)
# DDPRJ002 : Unconfirmed death status (DDSTAT not CONFIRMED)
# DDPRJ003 : Missing site ID (SITEID)
# Batch_ID : DD_PRJ
# ==============================================================================
run_DD_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$dd) || nrow(data_list$dd)==0) {
    message("  WARNING [DD_PRJ]: data_list$dd is empty - skipping.") ; return(ctx) }
  dd <- data_list$dd

  if (isTRUE(switches["DDPRJ001"])) {
    message("  Running DDPRJ001 - Missing DDDECOD ...")
    DDPRJ001 <- dd |>
      dplyr::filter(!is.na(DDTERM),trimws(DDTERM)!="",is.na(DDDECOD)|trimws(DDDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Coded cause of death (DDDECOD) is missing for term: ",DDTERM)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DDPRJ001,id="DDPRJ001") }

  if (isTRUE(switches["DDPRJ002"])) {
    message("  Running DDPRJ002 - Unconfirmed death status ...")
    DDPRJ002 <- dd |>
      dplyr::filter(!is.na(DDSTAT),trimws(DDSTAT)!="",
                    toupper(trimws(DDSTAT)) != "CONFIRMED") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Death status (DDSTAT='",DDSTAT,
          "') is not CONFIRMED for cause: ",
          ifelse(is.na(DDTERM),"[DDTERM missing]",DDTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DDPRJ002,id="DDPRJ002") }

  if (isTRUE(switches["DDPRJ003"])) {
    message("  Running DDPRJ003 - Missing SITEID ...")
    DDPRJ003 <- dd |>
      dplyr::filter(is.na(SITEID)|trimws(SITEID)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Site ID (SITEID) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DDPRJ003,id="DDPRJ003") }
  ctx
}
