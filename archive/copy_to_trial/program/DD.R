# ==============================================================================
# DD.R  -  Death Details  -  Trial level checks
# DDCHK001 : Missing date of death (DDDTHDTC)
# DDCHK002 : Missing cause of death term (DDTERM)
# DDCHK003 : Missing death flag (DTHFL)
# Batch_ID : DD
# ==============================================================================
run_DD <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$dd) || nrow(data_list$dd)==0) {
    message("  WARNING [DD]: data_list$dd is empty - skipping.") ; return(ctx) }
  dd <- data_list$dd

  if (isTRUE(switches["DDCHK001"])) {
    message("  Running DDCHK001 - Missing DDDTHDTC ...")
    DDCHK001 <- dd |>
      dplyr::filter(is.na(DDDTHDTC)|trimws(DDDTHDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Date of death (DDDTHDTC) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DDCHK001,id="DDCHK001") }

  if (isTRUE(switches["DDCHK002"])) {
    message("  Running DDCHK002 - Missing DDTERM ...")
    DDCHK002 <- dd |>
      dplyr::filter(is.na(DDTERM)|trimws(DDTERM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Cause of death term (DDTERM) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DDCHK002,id="DDCHK002") }

  if (isTRUE(switches["DDCHK003"])) {
    message("  Running DDCHK003 - Missing DTHFL ...")
    DDCHK003 <- dd |>
      dplyr::filter(is.na(DTHFL)|trimws(DTHFL)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Death flag (DTHFL) is missing in the death details record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DDCHK003,id="DDCHK003") }
  ctx
}
