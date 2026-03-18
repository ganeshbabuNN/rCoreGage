# ==============================================================================
# SV.R  -  Subject Visits  -  Trial level checks
#
# SVCHK001 : Missing visit start date (SVSTDTC)
# SVCHK002 : Visit end date before visit start date
# SVCHK003 : Visit dates not in chronological order within subject
#
# Input    : ctx$data_list$sv  - loaded from SV.csv
# Batch_ID : SV
# ==============================================================================

run_SV <- function(ctx, config) {
  data_list <- ctx$data_list
  switches  <- ctx$switches

  if (is.null(data_list$sv) || nrow(data_list$sv) == 0) {
    message("  WARNING [SV]: data_list$sv is empty - skipping all SV trial checks.")
    return(ctx)
  }

  sv <- data_list$sv

  # ============================================================================
  # SVCHK001 - Missing visit start date (SVSTDTC)
  # ============================================================================
  if (isTRUE(switches["SVCHK001"])) {
    message("  Running SVCHK001 - Missing visit start date (SVSTDTC) ...")

    SVCHK001 <- sv |>
      dplyr::filter(is.na(SVSTDTC) | trimws(SVSTDTC) == "") |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = as.numeric(VISITNUM),
        description = paste0(
          "Visit start date (SVSTDTC) is missing",
          " for visit: ", VISIT, " (VISITNUM=", VISITNUM, ")"
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, SVCHK001, id = "SVCHK001")
  }

  # ============================================================================
  # SVCHK002 - Visit end date (SVENDTC) before visit start date (SVSTDTC)
  # ============================================================================
  if (isTRUE(switches["SVCHK002"])) {
    message("  Running SVCHK002 - Visit end date before start date ...")

    SVCHK002 <- sv |>
      dplyr::filter(
        !is.na(SVSTDTC), trimws(SVSTDTC) != "",
        !is.na(SVENDTC), trimws(SVENDTC) != ""
      ) |>
      dplyr::mutate(
        sv_st = as.Date(SVSTDTC),
        sv_en = as.Date(SVENDTC)
      ) |>
      dplyr::filter(!is.na(sv_st), !is.na(sv_en), sv_en < sv_st) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = as.numeric(VISITNUM),
        description = paste0(
          "Visit end date (SVENDTC=", format(sv_en, "%d%b%Y"), ")",
          " is before start date (SVSTDTC=", format(sv_st, "%d%b%Y"), ")",
          " for visit: ", VISIT
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, SVCHK002, id = "SVCHK002")
  }

  # ============================================================================
  # SVCHK003 - Visit dates not in chronological order within a subject
  # ============================================================================
  if (isTRUE(switches["SVCHK003"])) {
    message("  Running SVCHK003 - Visit dates not in chronological order ...")

    SVCHK003 <- sv |>
      dplyr::filter(!is.na(SVSTDTC), trimws(SVSTDTC) != "") |>
      dplyr::mutate(sv_dt = as.Date(SVSTDTC)) |>
      dplyr::filter(!is.na(sv_dt)) |>
      dplyr::arrange(USUBJID, as.numeric(VISITNUM)) |>
      dplyr::group_by(USUBJID) |>
      dplyr::mutate(
        prev_dt  = dplyr::lag(sv_dt),
        prev_vis = dplyr::lag(VISIT)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(prev_dt), sv_dt < prev_dt) |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = as.numeric(VISITNUM),
        description = paste0(
          "Visit date (", VISIT, " = ", format(sv_dt, "%d%b%Y"), ")",
          " is before the previous visit (", prev_vis, " = ",
          format(prev_dt, "%d%b%Y"), ")",
          " - visits are not in chronological order"
        )
      ) |>
      dplyr::select(subj_id, vis_id, description)

    ctx <- prepare(ctx, SVCHK003, id = "SVCHK003")
  }

  ctx
}
