# ==============================================================================
# PP.R  -  Pharmacokinetics Parameters  -  Trial level checks
# PPCHK001 : Missing PK parameter result (PPORRES)
# PPCHK002 : Negative PK parameter value
# PPCHK003 : Missing analysis date (PPDTC)
# Batch_ID : PP
# ==============================================================================
run_PP <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$pp) || nrow(data_list$pp)==0) {
    message("  WARNING [PP]: data_list$pp is empty - skipping.") ; return(ctx) }
  pp <- data_list$pp

  if (isTRUE(switches["PPCHK001"])) {
    message("  Running PPCHK001 - Missing PPORRES ...")
    PPCHK001 <- pp |>
      dplyr::filter(is.na(PPORRES)|trimws(PPORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("PK parameter result (PPORRES) is missing for: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PPCHK001,id="PPCHK001") }

  if (isTRUE(switches["PPCHK002"])) {
    message("  Running PPCHK002 - Negative PK parameter value ...")
    PPCHK002 <- pp |>
      dplyr::filter(!is.na(PPORRES),trimws(PPORRES)!="") |>
      dplyr::mutate(val=suppressWarnings(as.numeric(PPORRES))) |>
      dplyr::filter(!is.na(val),val<0) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("PK parameter value (PPORRES=",PPORRES,
          ") is negative for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PPCHK002,id="PPCHK002") }

  if (isTRUE(switches["PPCHK003"])) {
    message("  Running PPCHK003 - Missing PPDTC ...")
    PPCHK003 <- pp |>
      dplyr::filter(is.na(PPDTC)|trimws(PPDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Analysis date (PPDTC) is missing for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PPCHK003,id="PPCHK003") }
  ctx
}
