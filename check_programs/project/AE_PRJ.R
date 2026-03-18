# ==============================================================================
# AE_PRJ.R  -  Adverse Events  -  Project level checks
# AEPRJ001 : Serious AE (AESER=Y) with missing action taken (AEACN)
# AEPRJ002 : Dictionary coded term (AEDECOD) missing
# AEPRJ003 : AE study day (AESTDY) missing
# Batch_ID : AE_PRJ
# ==============================================================================
run_AE_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$ae) || nrow(data_list$ae)==0) {
    message("  WARNING [AE_PRJ]: data_list$ae is empty - skipping.") ; return(ctx)
  }
  ae <- data_list$ae

  # AEPRJ001 : Serious AE (AESER=Y) with missing action taken (AEACN)
  if (isTRUE(switches["AEPRJ001"])) {
    message("  Running AEPRJ001 - Serious AE missing action taken ...")
    AEPRJ001 <- ae |>
      dplyr::filter(!is.na(AESER), toupper(trimws(AESER))=="Y",
                    is.na(AEACN) | trimws(AEACN)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("Serious AE (AESER=Y) has no action taken (AEACN)",
          " for term: ",AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, AEPRJ001, id="AEPRJ001")
  }
  
  # AEPRJ002 : Dictionary coded term (AEDECOD) missing
  if (isTRUE(switches["AEPRJ002"])) {
    message("  Running AEPRJ002 - Missing AEDECOD ...")
    AEPRJ002 <- ae |>
      dplyr::filter(is.na(AEDECOD) | trimws(AEDECOD)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("Dictionary coded term (AEDECOD) is missing",
          " for verbatim: ",AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, AEPRJ002, id="AEPRJ002")
  }
  
  # AEPRJ003 : AE study day (AESTDY) missing
  if (isTRUE(switches["AEPRJ003"])) {
    message("  Running AEPRJ003 - Missing AESTDY ...")
    AEPRJ003 <- ae |>
      dplyr::filter(is.na(AESTDY) | trimws(as.character(AESTDY))=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=NA_real_,
        description=paste0("AE study day (AESTDY) is missing",
          " for term: ",AETERM," starting ",AESTDTC," (AESEQ=",AESEQ,")")) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, AEPRJ003, id="AEPRJ003")
  }
  ctx
}
