# ==============================================================================
# SUPPDM.R  -  Supplemental Demographics  -  Trial level checks
# SUPPDMCHK001 : Missing qualifier value (QVAL)
# SUPPDMCHK002 : Missing qualifier name (QNAM)
# SUPPDMCHK003 : Missing qualifier label (QLABEL)
# Batch_ID : SUPPDM
# ==============================================================================
run_SUPPDM <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$suppdm) || nrow(data_list$suppdm)==0) {
    message("  WARNING [SUPPDM]: data_list$suppdm is empty - skipping.") ; return(ctx) }
  suppdm <- data_list$suppdm

  if (isTRUE(switches["SUPPDMCHK001"])) {
    message("  Running SUPPDMCHK001 - Missing QVAL ...")
    SUPPDMCHK001 <- suppdm |>
      dplyr::filter(is.na(QVAL)|trimws(QVAL)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Qualifier value (QVAL) is missing for qualifier: ",
          ifelse(is.na(QNAM),"[QNAM missing]",QNAM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,SUPPDMCHK001,id="SUPPDMCHK001") }

  if (isTRUE(switches["SUPPDMCHK002"])) {
    message("  Running SUPPDMCHK002 - Missing QNAM ...")
    SUPPDMCHK002 <- suppdm |>
      dplyr::filter(is.na(QNAM)|trimws(QNAM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Qualifier name (QNAM) is missing in the supplemental demographics record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,SUPPDMCHK002,id="SUPPDMCHK002") }

  if (isTRUE(switches["SUPPDMCHK003"])) {
    message("  Running SUPPDMCHK003 - Missing QLABEL ...")
    SUPPDMCHK003 <- suppdm |>
      dplyr::filter(is.na(QLABEL)|trimws(QLABEL)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Qualifier label (QLABEL) is missing for qualifier: ",
          ifelse(is.na(QNAM),"[QNAM missing]",QNAM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,SUPPDMCHK003,id="SUPPDMCHK003") }
  ctx
}
