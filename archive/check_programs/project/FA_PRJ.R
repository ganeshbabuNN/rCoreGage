# ==============================================================================
# FA_PRJ.R  -  Findings About Events  -  Project level checks
# FAPRJ001 : Missing parent domain reference object (FAOBJ)
# FAPRJ002 : Missing test code (FATESTCD)
# FAPRJ003 : Missing finding status (FASTAT)
# Batch_ID : FA_PRJ
# ==============================================================================
run_FA_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$fa) || nrow(data_list$fa)==0) {
    message("  WARNING [FA_PRJ]: data_list$fa is empty - skipping.") ; return(ctx) }
  fa <- data_list$fa

  if (isTRUE(switches["FAPRJ001"])) {
    message("  Running FAPRJ001 - Missing FAOBJ ...")
    FAPRJ001 <- fa |>
      dplyr::filter(is.na(FAOBJ)|trimws(FAOBJ)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Parent domain reference object (FAOBJ) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,FAPRJ001,id="FAPRJ001") }

  if (isTRUE(switches["FAPRJ002"])) {
    message("  Running FAPRJ002 - Missing FATESTCD ...")
    FAPRJ002 <- fa |>
      dplyr::filter(is.na(FATESTCD)|trimws(FATESTCD)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description="Finding test code (FATESTCD) is missing in the findings about record") |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,FAPRJ002,id="FAPRJ002") }

  if (isTRUE(switches["FAPRJ003"])) {
    message("  Running FAPRJ003 - Missing FASTAT ...")
    FAPRJ003 <- fa |>
      dplyr::filter(is.na(FASTAT)|trimws(FASTAT)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Finding status (FASTAT) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,FAPRJ003,id="FAPRJ003") }
  ctx
}
