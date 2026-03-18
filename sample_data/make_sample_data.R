# ==============================================================================
# sample_data/make_sample_data.R
# Description : Generates sample SDTM-like ADM datasets for testing CFrame.
#               Run this once to create the sample data, then load it in
#               master_run.R via:  source("sample_data/make_sample_data.R")
# ==============================================================================

make_sample_data <- function() {

  set.seed(42)

  n_subj <- 20
  subj_ids <- paste0("SUBJ-", sprintf("%03d", 1:n_subj))

  # ---------------------------------------------------------------------------
  # dm - Demographics (one row per subject)
  # ---------------------------------------------------------------------------
  dm <- data.frame(
    subj_id   = subj_ids,
    sex       = sample(c("M", "F"), n_subj, replace = TRUE),
    race      = sample(c("WHITE", "BLACK", "ASIAN", "OTHER"),
                       n_subj, replace = TRUE),
    age       = sample(18:75, n_subj, replace = TRUE),
    rfstdtc   = sample(seq(as.Date("2022-01-01"),
                           as.Date("2023-06-01"), by = "day"),
                       n_subj, replace = TRUE),
    country   = sample(c("USA", "GBR", "DEU", "FRA"),
                       n_subj, replace = TRUE),
    armcd     = sample(c("TRT", "PBO"), n_subj, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Introduce issues for DM checks
  # DMCHK001 - Missing sex for 3 subjects
  dm$sex[c(2, 7, 14)] <- NA

  # DMCHK002 - Age out of range (< 18 or > 80) for 2 subjects
  dm$age[c(5, 18)] <- c(15, 85)

  # ---------------------------------------------------------------------------
  # ae - Adverse Events
  # ---------------------------------------------------------------------------
  ae_rows <- lapply(subj_ids, function(sid) {
    n_ae <- sample(0:4, 1)
    if (n_ae == 0) return(NULL)
    data.frame(
      subj_id  = sid,
      aeterm   = sample(c("HEADACHE", "NAUSEA", "FATIGUE",
                           "DIZZINESS", "RASH"), n_ae, replace = TRUE),
      aestdtc  = sample(seq(as.Date("2022-01-01"),
                            as.Date("2023-12-01"), by = "day"),
                        n_ae, replace = TRUE),
      aeendtc  = sample(seq(as.Date("2022-06-01"),
                            as.Date("2024-01-01"), by = "day"),
                        n_ae, replace = TRUE),
      aesev    = sample(c("MILD", "MODERATE", "SEVERE"),
                        n_ae, replace = TRUE),
      aerel    = sample(c("RELATED", "NOT RELATED", "POSSIBLY RELATED"),
                        n_ae, replace = TRUE),
      aeser    = sample(c("Y", "N"), n_ae, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
  ae <- do.call(rbind, ae_rows[!sapply(ae_rows, is.null)])

  # AECHK001 - End date before start date for 3 rows
  ae$aeendtc[c(1, 5, 9)] <- ae$aestdtc[c(1, 5, 9)] - sample(1:10, 3)

  # AECHK002 - Missing severity for 2 rows
  ae$aesev[c(3, 8)] <- NA

  # ---------------------------------------------------------------------------
  # vs - Vital Signs
  # ---------------------------------------------------------------------------
  vis_ids  <- c(100, 200, 300, 400, 500)  # screening, baseline, w4, w8, eot
  vis_type <- c("SCREENING", "BASELINE", "WEEK 4", "WEEK 8", "END OF TREATMENT")

  vs_rows <- lapply(subj_ids, function(sid) {
    do.call(rbind, lapply(seq_along(vis_ids), function(vi) {
      data.frame(
        subj_id   = sid,
        vis_id    = vis_ids[vi],
        vstestcd  = rep(c("SYSBP", "DIABP", "PULSE", "TEMP", "WEIGHT"),
                        each = 1),
        vsorres   = c(
          round(rnorm(1, 120, 15)),   # SYSBP
          round(rnorm(1,  80, 10)),   # DIABP
          round(rnorm(1,  72,  8)),   # PULSE
          round(rnorm(1,  37,  0.5), 1),  # TEMP
          round(rnorm(1,  75, 12), 1)     # WEIGHT
        ),
        vsorresu  = c("mmHg", "mmHg", "beats/min", "C", "kg"),
        vsdtc     = sample(seq(as.Date("2022-01-01"),
                               as.Date("2024-01-01"), by = "day"), 5,
                           replace = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  })
  vs <- do.call(rbind, vs_rows)

  # VSCHK001 - Systolic BP below diastolic (impossible values) for 4 rows
  vs_sysbp_idx <- which(vs$vstestcd == "SYSBP")[c(1, 10, 20, 30)]
  vs_diabp_idx <- which(vs$vstestcd == "DIABP")[c(1, 10, 20, 30)]
  vs$vsorres[vs_sysbp_idx] <- vs$vsorres[vs_diabp_idx] - 5

  # VSCHK002 - Missing weight at baseline for 3 subjects
  weight_baseline <- which(vs$vstestcd == "WEIGHT" & vs$vis_id == 200)
  vs$vsorres[weight_baseline[c(1, 3, 5)]] <- NA

  # ---------------------------------------------------------------------------
  # lb - Laboratory Results
  # ---------------------------------------------------------------------------
  lab_tests <- data.frame(
    lbtestcd = c("HBA1C", "GLUC", "CREAT", "ALT", "AST",
                 "BILI", "WBC", "RBC", "HGB", "PLT"),
    lbtest   = c("Hemoglobin A1C", "Glucose", "Creatinine", "ALT",
                 "AST", "Bilirubin", "White Blood Cells",
                 "Red Blood Cells", "Hemoglobin", "Platelets"),
    lborresu = c("%", "mmol/L", "umol/L", "U/L", "U/L",
                 "umol/L", "10^9/L", "10^12/L", "g/dL", "10^9/L"),
    stringsAsFactors = FALSE
  )

  lb_rows <- lapply(subj_ids, function(sid) {
    do.call(rbind, lapply(vis_ids, function(vi) {
      data.frame(
        subj_id  = sid,
        vis_id   = vi,
        lbtestcd = lab_tests$lbtestcd,
        lbtest   = lab_tests$lbtest,
        lborres  = round(runif(nrow(lab_tests), 0.5, 10), 2),
        lborresu = lab_tests$lborresu,
        lbdtc    = sample(seq(as.Date("2022-01-01"),
                              as.Date("2024-01-01"), by = "day"),
                          nrow(lab_tests), replace = TRUE),
        lbnrlo   = 0.5,
        lbnrhi   = 9.0,
        stringsAsFactors = FALSE
      )
    }))
  })
  lb <- do.call(rbind, lb_rows)

  # LBCHK001 - Values outside normal range for some rows
  lb$lborres[c(5, 25, 50, 80)] <- c(12.5, 0.1, 15.0, 0.2)

  # ---------------------------------------------------------------------------
  # ds - Disposition
  # ---------------------------------------------------------------------------
  ds <- data.frame(
    subj_id  = subj_ids,
    dsdecod  = sample(c("COMPLETED", "ADVERSE EVENT", "WITHDRAWAL BY SUBJECT",
                         "LOST TO FOLLOW-UP"), n_subj, replace = TRUE,
                      prob = c(0.7, 0.1, 0.1, 0.1)),
    dsstdtc  = sample(seq(as.Date("2023-01-01"),
                          as.Date("2024-06-01"), by = "day"),
                      n_subj, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # DSCHK001 - Missing disposition date for 2 subjects
  ds$dsstdtc[c(4, 11)] <- NA

  # ---------------------------------------------------------------------------
  # cm - Concomitant Medications
  # ---------------------------------------------------------------------------
  cm_rows <- lapply(subj_ids, function(sid) {
    n_cm <- sample(1:5, 1)
    data.frame(
      subj_id  = sid,
      cmtrt    = sample(c("METFORMIN", "INSULIN", "ASPIRIN",
                           "LISINOPRIL", "ATORVASTATIN"),
                        n_cm, replace = TRUE),
      cmstdtc  = sample(seq(as.Date("2021-01-01"),
                            as.Date("2023-01-01"), by = "day"),
                        n_cm, replace = TRUE),
      cmendtc  = sample(seq(as.Date("2023-01-01"),
                            as.Date("2024-06-01"), by = "day"),
                        n_cm, replace = TRUE),
      cmindc   = sample(c("DIABETES", "HYPERTENSION", "PAIN",
                           "HYPERLIPIDEMIA"), n_cm, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
  cm <- do.call(rbind, cm_rows)

  # CMCHK001 - End date before start date for 4 rows
  cm$cmendtc[c(2, 6, 12, 18)] <- cm$cmstdtc[c(2, 6, 12, 18)] - sample(1:5, 4)

  # ---------------------------------------------------------------------------
  # sv - Subject Visits (for project level checks)
  # ---------------------------------------------------------------------------
  sv_rows <- lapply(subj_ids, function(sid) {
    data.frame(
      subj_id = sid,
      vis_id  = vis_ids,
      svstdtc = sort(sample(seq(as.Date("2022-01-01"),
                               as.Date("2024-01-01"), by = "day"),
                            length(vis_ids), replace = FALSE)),
      stringsAsFactors = FALSE
    )
  })
  sv <- do.call(rbind, sv_rows)

  # SVCHK001 - Visits out of order for 3 subjects (swap two visit dates)
  swap_subj <- c("SUBJ-003", "SUBJ-009", "SUBJ-015")
  for (s in swap_subj) {
    idx <- which(sv$subj_id == s & sv$vis_id %in% c(200, 300))
    if (length(idx) == 2) {
      tmp <- sv$svstdtc[idx[1]]
      sv$svstdtc[idx[1]] <- sv$svstdtc[idx[2]]
      sv$svstdtc[idx[2]] <- tmp
    }
  }

  # ---------------------------------------------------------------------------
  # plnd_trl_vis - Planned trial visits lookup
  # ---------------------------------------------------------------------------
  plnd_trl_vis <- data.frame(
    vis_id   = vis_ids,
    vis_type = vis_type,
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # subj_scrn_fail - Screening failures
  # ---------------------------------------------------------------------------
  subj_scrn_fail <- data.frame(
    subj_id = c("SUBJ-016", "SUBJ-017"),
    reason  = c("INCLUSION CRITERIA NOT MET", "WITHDRAWAL"),
    stringsAsFactors = FALSE
  )

  list(
    dm             = dm,
    ae             = ae,
    vs             = vs,
    lb             = lb,
    ds             = ds,
    cm             = cm,
    sv             = sv,
    plnd_trl_vis   = plnd_trl_vis,
    subj_scrn_fail = subj_scrn_fail
  )
}
