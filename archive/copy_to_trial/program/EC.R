# ==============================================================================
# EC.R  -  Exposure as Collected  -  Trial level checks
# ECCHK001 : EC end date before start date
# ECCHK002 : Missing dose amount (ECDOSE)
# ECCHK003 : Dose unit missing when dose is present
# ECCHK004 : Invalid dose form - not in allowed list
# ECCHK005 : Missing administration status (ECSTAT)
# Batch_ID : EC
# ==============================================================================
run_EC <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  allowed_forms <- c("TABLET","CAPSULE")
  if (is.null(data_list$ec) || nrow(data_list$ec)==0) {
    message("  WARNING [EC]: data_list$ec is empty - skipping.") ; return(ctx)
  }
  ec <- data_list$ec

  # ECCHK001 : EC end date before start date
  if (isTRUE(switches["ECCHK001"])) {
    message("  Running ECCHK001 - EC end date before start date ...")
    ECCHK001 <- ec |>
      dplyr::filter(!is.na(ECSTDTC), trimws(ECSTDTC)!="",
                    !is.na(ECENDTC), trimws(ECENDTC)!="") |>
      dplyr::mutate(st=as.Date(ECSTDTC), en=as.Date(ECENDTC)) |>
      dplyr::filter(!is.na(st), !is.na(en), en < st) |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("EC end date (",format(en,"%d%b%Y"),
          ") is before start date (",format(st,"%d%b%Y"),
          ") for treatment: ",ECTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, ECCHK001, id="ECCHK001")
  }
  
  # ECCHK002 : Missing dose amount (ECDOSE)
  if (isTRUE(switches["ECCHK002"])) {
    message("  Running ECCHK002 - Missing dose amount ...")
    ECCHK002 <- ec |>
      dplyr::filter(is.na(ECDOSE) | trimws(ECDOSE)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Dose amount (ECDOSE) is missing",
          " for treatment: ",ECTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, ECCHK002, id="ECCHK002")
  }
  
  # ECCHK003 : Dose unit missing when dose is present
  if (isTRUE(switches["ECCHK003"])) {
    message("  Running ECCHK003 - Dose unit missing when dose present ...")
    ECCHK003 <- ec |>
      dplyr::filter(!is.na(ECDOSE), trimws(ECDOSE)!="",
                    is.na(ECDOSU) | trimws(ECDOSU)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Dose unit (ECDOSU) is missing although",
          " dose (ECDOSE=",ECDOSE,") is present",
          " for treatment: ",ECTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, ECCHK003, id="ECCHK003")
  }
  
  # ECCHK004 : Invalid dose form - not in allowed list
  if (isTRUE(switches["ECCHK004"])) {
    message("  Running ECCHK004 - Invalid dose form ...")
    ECCHK004 <- ec |>
      dplyr::filter(!is.na(ECDOSFRM), trimws(ECDOSFRM)!="",
                    !toupper(trimws(ECDOSFRM)) %in% toupper(allowed_forms)) |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Dose form '",ECDOSFRM,
          "' not in allowed list (",paste(allowed_forms,collapse="/"),")",
          " for treatment: ",ECTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, ECCHK004, id="ECCHK004")
  }
  
  # ECCHK005 : Missing administration status (ECSTAT)
  if (isTRUE(switches["ECCHK005"])) {
    message("  Running ECCHK005 - Missing administration status ...")
    ECCHK005 <- ec |>
      dplyr::filter(is.na(ECSTAT) | trimws(ECSTAT)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Administration status (ECSTAT) is missing",
          " for treatment: ",ECTRT," at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, ECCHK005, id="ECCHK005")
  }
  ctx
}
