# ==============================================================================
# DV_PRJ.R  -  Protocol Deviations  -  Project level checks
# DVPRJ001 : Missing reason for deviation (DVREASND)
# DVPRJ002 : Missing dictionary coded term (DVDECOD)
# DVPRJ003 : Missing site ID (SITEID)
# Batch_ID : DV_PRJ
# ==============================================================================
run_DV_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$dv) || nrow(data_list$dv)==0) {
    message("  WARNING [DV_PRJ]: data_list$dv is empty - skipping.") ; return(ctx) }
  dv <- data_list$dv

  if (isTRUE(switches["DVPRJ001"])) {
    message("  Running DVPRJ001 - Missing DVREASND ...")
    DVPRJ001 <- dv |>
      dplyr::filter(is.na(DVREASND)|trimws(DVREASND)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Reason for deviation (DVREASND) is missing for: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DVPRJ001,id="DVPRJ001") }

  if (isTRUE(switches["DVPRJ002"])) {
    message("  Running DVPRJ002 - Missing DVDECOD ...")
    DVPRJ002 <- dv |>
      dplyr::filter(!is.na(DVTERM),trimws(DVTERM)!="",is.na(DVDECOD)|trimws(DVDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Dictionary coded term (DVDECOD) is missing for deviation: ",DVTERM)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DVPRJ002,id="DVPRJ002") }

  if (isTRUE(switches["DVPRJ003"])) {
    message("  Running DVPRJ003 - Missing SITEID ...")
    DVPRJ003 <- dv |>
      dplyr::filter(is.na(SITEID)|trimws(SITEID)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Site ID (SITEID) is missing for deviation: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DVPRJ003,id="DVPRJ003") }
  ctx
}
