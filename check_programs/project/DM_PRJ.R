# ==============================================================================
# DM_PRJ.R  -  Demographics  -  Project level checks
#
# DMPRJ001 : Missing country (COUNTRY)
# DMPRJ002 : Reference end date before reference start date
# DMPRJ003 : Missing birth date (BRTHDTC)
#
# Input    : ctx$data_list$dm  - loaded from DM.csv
# Batch_ID : DM_PRJ
# ==============================================================================

run_DM_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list
  switches  <- ctx$switches

  if (is.null(data_list$dm) || nrow(data_list$dm) == 0) {
    message("  WARNING [DM_PRJ]: data_list$dm is empty - skipping all DM project checks.")
    return(ctx)
  }

  dm <- data_list$dm

  # ============================================================================
  # DMPRJ001 - Missing country (COUNTRY)
  # ============================================================================
  if (isTRUE(switches["DMPRJ001"])) {
    message("  Running DMPRJ001 - Missing COUNTRY ...")

    DMPRJ001 <- dm |>
      dplyr::filter(is.na(COUNTRY) | trimws(COUNTRY) == "") |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = "Country (COUNTRY) is missing in the demographics record"
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, DMPRJ001, id = "DMPRJ001")
  }

  # ============================================================================
  # DMPRJ002 - Reference end date (RFENDTC) before reference start date (RFSTDTC)
  # ============================================================================
  if (isTRUE(switches["DMPRJ002"])) {
    message("  Running DMPRJ002 - RFENDTC before RFSTDTC ...")

    DMPRJ002 <- dm |>
      dplyr::filter(
        !is.na(RFSTDTC), trimws(RFSTDTC) != "",
        !is.na(RFENDTC), trimws(RFENDTC) != ""
      ) |>
      dplyr::mutate(
        rf_st = as.Date(RFSTDTC),
        rf_en = as.Date(RFENDTC)
      ) |>
      dplyr::filter(!is.na(rf_st), !is.na(rf_en), rf_en < rf_st) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = paste0(
          "Reference end date (RFENDTC=", format(rf_en, "%d%b%Y"), ")",
          " is before reference start date (RFSTDTC=", format(rf_st, "%d%b%Y"), ")"
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, DMPRJ002, id = "DMPRJ002")
  }

  # ============================================================================
  # DMPRJ003 - Missing birth date (BRTHDTC)
  # ============================================================================
  if (isTRUE(switches["DMPRJ003"])) {
    message("  Running DMPRJ003 - Missing birth date (BRTHDTC) ...")

    DMPRJ003 <- dm |>
      dplyr::filter(is.na(BRTHDTC) | trimws(BRTHDTC) == "") |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = "Birth date (BRTHDTC) is missing in the demographics record"
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, DMPRJ003, id = "DMPRJ003")
  }

  ctx
}
