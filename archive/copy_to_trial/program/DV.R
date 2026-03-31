# ==============================================================================
# DV.R  -  Protocol Deviations  -  Trial level checks
# DVCHK001 : Missing deviation date (DVSTDTC)
# DVCHK002 : Missing deviation term (DVTERM)
# DVCHK003 : Missing deviation category (DVCAT)
# Batch_ID : DV
# ==============================================================================
run_DV <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$dv) || nrow(data_list$dv)==0) {
    message("  WARNING [DV]: data_list$dv is empty - skipping.") ; return(ctx) }
  dv <- data_list$dv

  if (isTRUE(switches["DVCHK001"])) {
    message("  Running DVCHK001 - Missing DVSTDTC ...")
    DVCHK001 <- dv |>
      dplyr::filter(is.na(DVSTDTC)|trimws(DVSTDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Deviation date (DVSTDTC) is missing for deviation: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DVCHK001,id="DVCHK001") }

  if (isTRUE(switches["DVCHK002"])) {
    message("  Running DVCHK002 - Missing DVTERM ...")
    DVCHK002 <- dv |>
      dplyr::filter(is.na(DVTERM)|trimws(DVTERM)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Protocol deviation term (DVTERM) is missing in the deviation record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DVCHK002,id="DVCHK002") }

  if (isTRUE(switches["DVCHK003"])) {
    message("  Running DVCHK003 - Missing DVCAT ...")
    DVCHK003 <- dv |>
      dplyr::filter(is.na(DVCAT)|trimws(DVCAT)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Deviation category (DVCAT) is missing for deviation: ",
          ifelse(is.na(DVTERM),"[DVTERM missing]",DVTERM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,DVCHK003,id="DVCHK003") }
  ctx
}
