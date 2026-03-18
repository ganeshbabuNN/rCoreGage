# ==============================================================================
# AE.R  -  Adverse Events  -  Trial level checks
# AECHK001 : AE end date before start date
# AECHK002 : Missing AE severity (AESEV)
# AECHK003 : Missing AE outcome (AEOUT)
# Batch_ID : AE
# ==============================================================================
run_AE <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$ae) || nrow(data_list$ae)==0) {
    message("  WARNING [AE]: data_list$ae is empty - skipping.") ; return(ctx)
  }
  ae <- data_list$ae
  # AECHK001 : AE end date before start date
  if (isTRUE(switches["AECHK001"])) {
    message("  Running AECHK001 - AE end date before start date ...")
    AECHK001 <- ae |>
      dplyr::filter(!is.na(AESTDTC), trimws(AESTDTC)!="",
                    !is.na(AEENDTC), trimws(AEENDTC)!="") |>
      dplyr::mutate(st=as.Date(AESTDTC), en=as.Date(AEENDTC)) |>
      dplyr::filter(!is.na(st), !is.na(en), en < st) |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("AE end date (",format(en,"%d%b%Y"),
          ") is before start date (",format(st,"%d%b%Y"),
          ") for term: ",AETERM," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, AECHK001, id="AECHK001")
  }

  # AECHK002 : Missing AE severity (AESEV)
  if (isTRUE(switches["AECHK002"])) {
    message("  Running AECHK002 - Missing AE severity ...")
    AECHK002 <- ae |>
      dplyr::filter(is.na(AESEV) | trimws(AESEV)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("AE severity (AESEV) is missing for term: ",
          AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, AECHK002, id="AECHK002")
  }

  # AECHK003 : Missing AE outcome (AEOUT)
  if (isTRUE(switches["AECHK003"])) {
    message("  Running AECHK003 - Missing AE outcome ...")
    AECHK003 <- ae |>
      dplyr::filter(is.na(AEOUT) | trimws(AEOUT)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("AE outcome (AEOUT) is missing for term: ",
          AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, AECHK003, id="AECHK003")
  }
  ctx
}
