# ==============================================================================
# PP_PRJ.R  -  Pharmacokinetics Parameters  -  Project level checks
# PPPRJ001 : Missing standardised units (PPSTRESU)
# PPPRJ002 : Missing specimen type (PPSPEC)
# PPPRJ003 : Invalid parameter category (PPCAT not DERIVED)
# Batch_ID : PP_PRJ
# ==============================================================================
run_PP_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$pp) || nrow(data_list$pp)==0) {
    message("  WARNING [PP_PRJ]: data_list$pp is empty - skipping.") ; return(ctx) }
  pp <- data_list$pp

  if (isTRUE(switches["PPPRJ001"])) {
    message("  Running PPPRJ001 - Missing PPSTRESU ...")
    PPPRJ001 <- pp |>
      dplyr::filter(is.na(PPSTRESU)|trimws(PPSTRESU)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Standardised units (PPSTRESU) are missing for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PPPRJ001,id="PPPRJ001") }

  if (isTRUE(switches["PPPRJ002"])) {
    message("  Running PPPRJ002 - Missing PPSPEC ...")
    PPPRJ002 <- pp |>
      dplyr::filter(is.na(PPSPEC)|trimws(PPSPEC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Specimen type (PPSPEC) is missing for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PPPRJ002,id="PPPRJ002") }

  if (isTRUE(switches["PPPRJ003"])) {
    message("  Running PPPRJ003 - Invalid PPCAT (expected DERIVED) ...")
    PPPRJ003 <- pp |>
      dplyr::filter(!is.na(PPCAT),trimws(PPCAT)!="",
                    toupper(trimws(PPCAT)) != "DERIVED") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Parameter category (PPCAT='",PPCAT,
          "') should be DERIVED for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PPPRJ003,id="PPPRJ003") }
  ctx
}
