# ==============================================================================
# FA.R  -  Findings About Events  -  Trial level checks
# FACHK001 : Missing finding result (FAORRES)
# FACHK002 : Missing finding date (FADTC)
# FACHK003 : Missing evaluator flag (FAEVAL)
# Batch_ID : FA
# ==============================================================================
run_FA <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  if (is.null(data_list$fa) || nrow(data_list$fa)==0) {
    message("  WARNING [FA]: data_list$fa is empty - skipping.") ; return(ctx) }
  fa <- data_list$fa

  if (isTRUE(switches["FACHK001"])) {
    message("  Running FACHK001 - Missing FAORRES ...")
    FACHK001 <- fa |>
      dplyr::filter(is.na(FAORRES)|trimws(FAORRES)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Finding result (FAORRES) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,FACHK001,id="FACHK001") }

  if (isTRUE(switches["FACHK002"])) {
    message("  Running FACHK002 - Missing FADTC ...")
    FACHK002 <- fa |>
      dplyr::filter(is.na(FADTC)|trimws(FADTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Finding date (FADTC) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,FACHK002,id="FACHK002") }

  if (isTRUE(switches["FACHK003"])) {
    message("  Running FACHK003 - Missing FAEVAL ...")
    FACHK003 <- fa |>
      dplyr::filter(is.na(FAEVAL)|trimws(FAEVAL)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Evaluator flag (FAEVAL) is missing for test: ",
          ifelse(is.na(FATESTCD),"[FATESTCD missing]",FATESTCD))) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,FACHK003,id="FACHK003") }
  ctx
}
