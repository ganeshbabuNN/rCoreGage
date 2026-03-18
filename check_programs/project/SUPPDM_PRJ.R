# ==============================================================================
# SUPPDM_PRJ.R  -  Supplemental Demographics  -  Project level checks
# SUPPDMPRJ001 : Missing ID variable value (IDVARVAL)
# SUPPDMPRJ002 : Invalid reference domain (RDOMAIN not DM)
# SUPPDMPRJ003 : Missing site ID (SITEID)
# Batch_ID : SUPPDM_PRJ
# ==============================================================================
run_SUPPDM_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$suppdm) || nrow(data_list$suppdm)==0) {
    message("  WARNING [SUPPDM_PRJ]: data_list$suppdm is empty - skipping.") ; return(ctx) }
  suppdm <- data_list$suppdm

  if (isTRUE(switches["SUPPDMPRJ001"])) {
    message("  Running SUPPDMPRJ001 - Missing IDVARVAL ...")
    SUPPDMPRJ001 <- suppdm |>
      dplyr::filter(is.na(IDVARVAL)|trimws(IDVARVAL)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("ID variable value (IDVARVAL) is missing for qualifier: ",
          ifelse(is.na(QNAM),"[QNAM missing]",QNAM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,SUPPDMPRJ001,id="SUPPDMPRJ001") }

  if (isTRUE(switches["SUPPDMPRJ002"])) {
    message("  Running SUPPDMPRJ002 - Invalid RDOMAIN ...")
    SUPPDMPRJ002 <- suppdm |>
      dplyr::filter(!is.na(RDOMAIN),trimws(RDOMAIN)!="",
                    toupper(trimws(RDOMAIN)) != "DM") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Reference domain (RDOMAIN='",RDOMAIN,
          "') should be DM for supplemental demographics record")) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,SUPPDMPRJ002,id="SUPPDMPRJ002") }

  if (isTRUE(switches["SUPPDMPRJ003"])) {
    message("  Running SUPPDMPRJ003 - Missing SITEID ...")
    SUPPDMPRJ003 <- suppdm |>
      dplyr::filter(is.na(SITEID)|trimws(SITEID)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Site ID (SITEID) is missing for qualifier: ",
          ifelse(is.na(QNAM),"[QNAM missing]",QNAM))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,SUPPDMPRJ003,id="SUPPDMPRJ003") }
  ctx
}
