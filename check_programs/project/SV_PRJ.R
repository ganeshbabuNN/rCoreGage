# ==============================================================================
# SV_PRJ.R  -  Subject Visits  -  Project level checks
#
# SVPRJ001 : Missing visit description (SVUPDES) for unscheduled visits
# SVPRJ002 : Duplicate visit records for same subject and visit number
# SVPRJ003 : Visit date outside expected study window (after study end 2026)
#
# Input    : ctx$data_list$sv  - loaded from SV.csv
# Batch_ID : SV_PRJ
#
# Note     : Update study_end_date below to match your trial's planned end date
# ==============================================================================

run_SV_PRJ <- function(ctx, config) {
  data_list <- ctx$data_list
  switches  <- ctx$switches

  # Update this date to the trial's planned study end date
  study_end_date <- as.Date("2026-01-01")

  if (is.null(data_list$sv) || nrow(data_list$sv) == 0) {
    message("  WARNING [SV_PRJ]: data_list$sv is empty - skipping all SV project checks.")
    return(ctx)
  }

  sv <- data_list$sv

  # ============================================================================
  # SVPRJ001 - Missing visit description (SVUPDES)
  # ============================================================================
  if (isTRUE(switches["SVPRJ001"])) {
    message("  Running SVPRJ001 - Missing visit description (SVUPDES) ...")

    if (!"SVUPDES" %in% names(sv)) {
      message("  NOTE: SVUPDES column not found - skipping SVPRJ001.")
    } else {
      SVPRJ001 <- sv |>
        dplyr::filter(is.na(SVUPDES) | trimws(SVUPDES) == "") |>
        dplyr::mutate(
          subj_id     = USUBJID,
          vis_id      = as.numeric(VISITNUM),
          description = paste0(
            "Visit description (SVUPDES) is missing",
            " for visit: ", VISIT, " (VISITNUM=", VISITNUM, ")"
          )
        ) |>
        dplyr::select(subj_id, vis_id, description)

      ctx <- prepare(ctx, SVPRJ001, id = "SVPRJ001")
    }
  }

  # ============================================================================
  # SVPRJ002 - Duplicate visit records (same subject and visit number)
  # ============================================================================
  if (isTRUE(switches["SVPRJ002"])) {
    message("  Running SVPRJ002 - Duplicate visit records ...")

    SVPRJ002 <- sv |>
      dplyr::group_by(USUBJID, VISITNUM) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup() |>
      dplyr::distinct(USUBJID, VISITNUM, .keep_all = TRUE) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = as.numeric(VISITNUM),
        description = paste0(
          "Duplicate visit records found for visit: ",
          VISIT, " (VISITNUM=", VISITNUM, ")",
          " - more than one record exists for this subject and visit"
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, SVPRJ002, id = "SVPRJ002")
  }

  # ============================================================================
  # SVPRJ003 - Visit date outside expected study window
  # ============================================================================
  if (isTRUE(switches["SVPRJ003"])) {
    message("  Running SVPRJ003 - Visit date outside expected study window ...")

    SVPRJ003 <- sv |>
      dplyr::filter(!is.na(SVSTDTC), trimws(SVSTDTC) != "") |>
      dplyr::mutate(sv_dt = suppressWarnings(as.Date(SVSTDTC))) |>
      dplyr::filter(!is.na(sv_dt), sv_dt > study_end_date) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = as.numeric(VISITNUM),
        description = paste0(
          "Visit date (SVSTDTC=", format(sv_dt, "%d%b%Y"), ")",
          " is after the expected study end date (",
          format(study_end_date, "%d%b%Y"), ")",
          " for visit: ", VISIT
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, SVPRJ003, id = "SVPRJ003")
  }

  ctx
}
