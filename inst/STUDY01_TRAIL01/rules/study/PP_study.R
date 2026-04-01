# ==============================================================================
# PP_study.R  -  Pharmacokinetics Parameters  -  Project level checks
# PPPRJ001 : Missing standardised units (PPSTRESU)
# PPPRJ002 : Missing specimen type (PPSPEC)
# PPPRJ003 : Invalid parameter category (PPCAT not DERIVED)
# Rule_Set  : PP_PRJ
# ==============================================================================
check_PP_study <- function(state, cfg) {
  domains <- state$domains ; active_rules <- state$active_rules
  if (is.null(domains$pp) || nrow(domains$pp)==0) {
    message("  WARNING [PP_study]: domains$pp is empty - skipping.") ; return(state) }
  pp <- domains$pp

  # PPPRJ001 : Missing standardised units (PPSTRESU)
  if (isTRUE(active_rules["PPPRJ001"])) {
    message("  Running PPPRJ001 - Missing PPSTRESU ...")
    PPPRJ001 <- pp |>
      dplyr::filter(is.na(PPSTRESU)|trimws(PPSTRESU)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Standardised units (PPSTRESU) are missing for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PPPRJ001,id="PPPRJ001") }

  # PPPRJ002 : Missing specimen type (PPSPEC)
  if (isTRUE(active_rules["PPPRJ002"])) {
    message("  Running PPPRJ002 - Missing PPSPEC ...")
    PPPRJ002 <- pp |>
      dplyr::filter(is.na(PPSPEC)|trimws(PPSPEC)=="") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Specimen type (PPSPEC) is missing for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PPPRJ002,id="PPPRJ002") }
  
  # PPPRJ003 : Invalid parameter category (PPCAT not DERIVED)
  if (isTRUE(active_rules["PPPRJ003"])) {
    message("  Running PPPRJ003 - Invalid PPCAT (expected DERIVED) ...")
    PPPRJ003 <- pp |>
      dplyr::filter(!is.na(PPCAT),trimws(PPCAT)!="",
                    toupper(trimws(PPCAT)) != "DERIVED") |>
      dplyr::mutate(subj_id=USUBJID,vis_id=NA_real_,
        description=paste0("Parameter category (PPCAT='",PPCAT,
          "') should be DERIVED for parameter: ",PPTESTCD)) |>
      dplyr::select(subj_id,vis_id,description)
    state <- collect_findings(state,PPPRJ003,id="PPPRJ003") }
  state
}
