# ==============================================================================
# LB_PRJ.R  -  Laboratory Results  -  Project level checks
# LBPRJ001 : Missing lab collection date (LBDTC)
# LBPRJ002 : Inconsistent analysis method across sites for same test
# LBPRJ003 : Specimen condition not in allowed list
# Batch_ID : LB_PRJ
# ==============================================================================
run_LB_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list ; switches <- ctx$switches
  allowed_cond <- c("ORIGINAL","RECOLLECT")
  if (is.null(data_list$lb) || nrow(data_list$lb)==0) {
    message("  WARNING [LB_PRJ]: data_list$lb is empty - skipping.") ; return(ctx)
  }
  lb <- data_list$lb

  # LBPRJ001 : Missing lab collection date (LBDTC)
  if (isTRUE(switches["LBPRJ001"])) {
    message("  Running LBPRJ001 - Missing lab collection date ...")
    LBPRJ001 <- lb |>
      dplyr::filter(is.na(LBDTC) | trimws(LBDTC)=="") |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Lab collection date (LBDTC) is missing for test ",
          LBTEST," (",LBTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, LBPRJ001, id="LBPRJ001")
  }
  
  # LBPRJ002 : Inconsistent analysis method across sites for same test
  if (isTRUE(switches["LBPRJ002"])) {
    message("  Running LBPRJ002 - Inconsistent lab method across sites ...")
    method_check <- lb |>
      dplyr::filter(!is.na(LBMETHOD), trimws(LBMETHOD)!="") |>
      dplyr::group_by(VISITNUM, LBTESTCD) |>
      dplyr::summarise(n_methods=dplyr::n_distinct(LBMETHOD),
                       methods=paste(sort(unique(LBMETHOD)),collapse=" / "),
                       .groups="drop") |>
      dplyr::filter(n_methods > 1)
    if (nrow(method_check) > 0) {
      LBPRJ002 <- lb |>
        dplyr::inner_join(method_check |> dplyr::select(VISITNUM,LBTESTCD,methods),
                          by=c("VISITNUM","LBTESTCD")) |>
        dplyr::distinct(USUBJID, VISITNUM, LBTESTCD, .keep_all=TRUE) |>
        dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
          description=paste0("Inconsistent analysis method for ",
            LBTEST," (",LBTESTCD,") at visit ",VISIT,
            " - methods used: ",methods)) |>
        dplyr::select(subj_id, vis_id, description)
    } else {
      LBPRJ002 <- data.frame(subj_id=character(0), vis_id=numeric(0),
                              description=character(0), stringsAsFactors=FALSE)
    }
    ctx <- prepare(ctx, LBPRJ002, id="LBPRJ002")
  }
  
  # LBPRJ003 : Specimen condition not in allowed list
  if (isTRUE(switches["LBPRJ003"])) {
    message("  Running LBPRJ003 - Specimen condition not in allowed list ...")
    LBPRJ003 <- lb |>
      dplyr::filter(!is.na(LBSPCCND), trimws(LBSPCCND)!="",
                    !toupper(trimws(LBSPCCND)) %in% toupper(allowed_cond)) |>
      dplyr::mutate(subj_id=USUBJID, vis_id=as.numeric(VISITNUM),
        description=paste0("Specimen condition '",LBSPCCND,
          "' not in allowed list (",paste(allowed_cond,collapse="/"),")",
          " for test ",LBTEST," (",LBTESTCD,") at visit ",VISIT)) |>
      dplyr::select(subj_id, vis_id, description)
    ctx <- prepare(ctx, LBPRJ003, id="LBPRJ003")
  }
  ctx
}

