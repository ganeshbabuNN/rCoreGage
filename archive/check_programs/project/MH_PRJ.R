# ==============================================================================
# MH_PRJ.R  -  Medical History  -  Project level checks
# MHPRJ001 : Missing dictionary coded term (MHDECOD)
# MHPRJ002 : Invalid medical history status (MHSTAT)
# MHPRJ003 : Missing body system (MHBODSYS)
# Batch_ID : MH_PRJ
# ==============================================================================
run_MH_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  allowed_stat <- c("ONGOING","RESOLVED","UNKNOWN")
  if (is.null(data_list$mh) || nrow(data_list$mh)==0) {
    message("  WARNING [MH_PRJ]: data_list$mh is empty - skipping.") ; return(ctx) }
  mh <- data_list$mh

  if (isTRUE(switches["MHPRJ001"])) {
    message("  Running MHPRJ001 - Missing MHDECOD ...")
    MHPRJ001 <- mh |>
      dplyr::filter(!is.na(MHTERM),trimws(MHTERM)!="",is.na(MHDECOD)|trimws(MHDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Dictionary coded term (MHDECOD) is missing for condition: ",MHTERM)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,MHPRJ001,id="MHPRJ001") }

  if (isTRUE(switches["MHPRJ002"])) {
    message("  Running MHPRJ002 - Invalid MHSTAT ...")
    MHPRJ002 <- mh |>
      dplyr::filter(!is.na(MHSTAT),trimws(MHSTAT)!="",
                    !toupper(trimws(MHSTAT)) %in% toupper(allowed_stat)) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Medical history status '",MHSTAT,
          "' is not in allowed list (",paste(allowed_stat,collapse="/"),
          ") for condition: ",ifelse(is.na(MHTERM),"[MHTERM missing]",MHTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,MHPRJ002,id="MHPRJ002") }

  if (isTRUE(switches["MHPRJ003"])) {
    message("  Running MHPRJ003 - Missing MHBODSYS ...")
    MHPRJ003 <- mh |>
      dplyr::filter(is.na(MHBODSYS)|trimws(MHBODSYS)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Body system (MHBODSYS) is missing for condition: ",
          ifelse(is.na(MHTERM),"[MHTERM missing]",MHTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,MHPRJ003,id="MHPRJ003") }
  ctx
}
