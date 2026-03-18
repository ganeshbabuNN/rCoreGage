# ==============================================================================
# VS_PRJ.R  -  Vital Signs  -  Project level checks
# VSPRJ001 : Missing vital sign collection date (VSDTC)
# VSPRJ002 : Duplicate vital sign records same subject/visit/test
# VSPRJ003 : Invalid position (VSPOS) value
# Batch_ID : VS_PRJ
# ==============================================================================
run_VS_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  allowed_pos <- c("STANDING","SITTING","SUPINE","PRONE","ORIGINAL")
  if (is.null(data_list$vs) || nrow(data_list$vs)==0) {
    message("  WARNING [VS_PRJ]: data_list$vs is empty - skipping.") ; return(ctx) }
  vs <- data_list$vs

  if (isTRUE(switches["VSPRJ001"])) {
    message("  Running VSPRJ001 - Missing VSDTC ...")
    VSPRJ001 <- vs |>
      dplyr::filter(is.na(VSDTC)|trimws(VSDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Collection date (VSDTC) is missing for test: ",
          VSTEST," (",VSTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,VSPRJ001,id="VSPRJ001") }

  if (isTRUE(switches["VSPRJ002"])) {
    message("  Running VSPRJ002 - Duplicate VS records ...")
    VSPRJ002 <- vs |>
      dplyr::group_by(USUBJID,VISITNUM,VSTESTCD) |>
      dplyr::filter(dplyr::n()>1) |>
      dplyr::ungroup() |>
      dplyr::distinct(USUBJID,VISITNUM,VSTESTCD,.keep_all=TRUE) |>
      dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
        description=paste0("Duplicate vital sign records for ",VSTEST,
          " (",VSTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id,vis_id,description)
    ctx <- prepare(ctx,VSPRJ002,id="VSPRJ002") }

  if (isTRUE(switches["VSPRJ003"])) {
    message("  Running VSPRJ003 - Invalid VSPOS ...")
    if (!"VSPOS" %in% names(vs)) {
      message("  NOTE: VSPOS column not found - skipping VSPRJ003.")
    } else {
      VSPRJ003 <- vs |>
        dplyr::filter(!is.na(VSPOS),trimws(VSPOS)!="",
                      !toupper(trimws(VSPOS)) %in% toupper(allowed_pos)) |>
        dplyr::mutate(subj_id=USUBJID,vis_id=as.numeric(VISITNUM),
          description=paste0("Position value '",VSPOS,
            "' is not in allowed list (",paste(allowed_pos,collapse="/"),
            ") for test: ",VSTEST," at visit ",VISIT)) |>
        dplyr::select(subj_id,vis_id,description)
      ctx <- prepare(ctx,VSPRJ003,id="VSPRJ003") } }
  ctx
}
