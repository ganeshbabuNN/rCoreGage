# ==============================================================================
# DS.R  -  Disposition  -  Trial level checks
# DSCHK001 : Missing disposition date (DSSTDTC)
# DSCHK002 : Missing disposition decision (DSDECOD)
# DSCHK003 : Missing disposition term (DSTERM)
# Batch_ID : DS
# ==============================================================================
run_DS <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$ds) || nrow(data_list$ds)==0) {
    message("  WARNING [DS]: data_list$ds is empty - skipping.") ; return(ctx) }
  ds <- data_list$ds

  if (isTRUE(switches["DSCHK001"])) {
    message("  Running DSCHK001 - Missing DSSTDTC ...")
    DSCHK001 <- ds |>
      dplyr::filter(is.na(DSSTDTC)|trimws(DSSTDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Disposition date (DSSTDTC) is missing for disposition: ",
          ifelse(is.na(DSDECOD),"[DSDECOD missing]",DSDECOD))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DSCHK001,id="DSCHK001") }

  if (isTRUE(switches["DSCHK002"])) {
    message("  Running DSCHK002 - Missing DSDECOD ...")
    DSCHK002 <- ds |>
      dplyr::filter(is.na(DSDECOD)|trimws(DSDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Disposition decision (DSDECOD) is missing in the disposition record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DSCHK002,id="DSCHK002") }

  if (isTRUE(switches["DSCHK003"])) {
    message("  Running DSCHK003 - Missing DSTERM ...")
    DSCHK003 <- ds |>
      dplyr::filter(is.na(DSTERM)|trimws(DSTERM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Disposition verbatim term (DSTERM) is missing in the disposition record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DSCHK003,id="DSCHK003") }
  ctx
}
