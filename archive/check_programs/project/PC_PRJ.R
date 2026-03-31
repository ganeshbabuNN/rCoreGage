# ==============================================================================
# PC_PRJ.R  -  Pharmacokinetics Concentrations  -  Project level checks
# PCPRJ001 : Missing specimen type (PCSPEC)
# PCPRJ002 : Missing concentration units (PCSTRESU)
# PCPRJ003 : Duplicate records for same subject, visit, and test
# Batch_ID : PC_PRJ
# ==============================================================================
run_PC_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$pc) || nrow(data_list$pc)==0) {
    message("  WARNING [PC_PRJ]: data_list$pc is empty - skipping.") ; return(ctx) }
  pc <- data_list$pc

  if (isTRUE(switches["PCPRJ001"])) {
    message("  Running PCPRJ001 - Missing PCSPEC ...")
    PCPRJ001 <- pc |>
      dplyr::filter(is.na(PCSPEC)|trimws(PCSPEC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Specimen type (PCSPEC) is missing for test: ",
          PCTESTCD," at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PCPRJ001,id="PCPRJ001") }

  if (isTRUE(switches["PCPRJ002"])) {
    message("  Running PCPRJ002 - Missing PCSTRESU ...")
    PCPRJ002 <- pc |>
      dplyr::filter(!is.na(PCORRES),trimws(PCORRES)!="",
                    is.na(PCSTRESU)|trimws(PCSTRESU)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Concentration units (PCSTRESU) are missing although",
          " result (PCORRES=",PCORRES,") is present for test: ",PCTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PCPRJ002,id="PCPRJ002") }

  if (isTRUE(switches["PCPRJ003"])) {
    message("  Running PCPRJ003 - Duplicate PC records ...")
    PCPRJ003 <- pc |>
      dplyr::group_by(USUBJID,VISITNUM,PCTESTCD) |>
      dplyr::filter(dplyr::n()>1) |>
      dplyr::ungroup() |>
      dplyr::distinct(USUBJID,VISITNUM,PCTESTCD,.keep_all=TRUE) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Duplicate records found for test: ",PCTESTCD,
          " at visit ",VISIT," - more than one result recorded")) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,PCPRJ003,id="PCPRJ003") }
  ctx
}
