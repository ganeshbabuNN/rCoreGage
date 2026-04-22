#' Execute all active check programs
#'
#' Loops through all active rule sets defined in the rule registry,
#' sources each check script from the appropriate folder
#' (trial checks from \code{cfg$trial_checks}, study checks from
#' \code{cfg$study_checks}), and calls \code{check_RULESET(state, cfg)},
#' where \code{RULESET} is the value from the \code{Rule_Set} column in
#' \code{rule_registry.xlsx}.
#'
#' @param cfg A named list of paths from \code{project_config.R}.
#' @param state The state object returned by \code{\link{setup_coregage}}.
#'
#' @return Updated \code{state} object with \code{issues} and
#'   \code{summary_log} populated.
#'
#' @export
#' @examples
#' \donttest{
#' tmp_rep <- tempdir()
#' cfg <- list(
#'   rule_registry = system.file("extdata", "rule_registry.xlsx",
#'                               package = "rCoreGage"),
#'   trial_checks  = tmp_rep,
#'   study_checks  = tmp_rep,
#'   inputs        = tmp_rep,
#'   reports       = tmp_rep,
#'   feedback      = tmp_rep
#' )
#' state          <- setup_coregage(cfg)
#' state$domains  <- load_inputs(cfg)
#' state          <- run_checks(cfg, state)
#' }
run_checks <- function(cfg, state) {

  message(">> [runner] Starting check execution ...")

  sources <- unique(state$rule_registry$sheet)

  for (src in sources) {

    check_dir <- if (src == "Trial") cfg$trial_checks else cfg$study_checks

    active_sets <- state$rule_registry |>
      dplyr::filter(.data$sheet == src,
                    startsWith(.data$active, "Y")) |>
      dplyr::pull(.data$rule_set) |>
      unique() |>
      sort()

    if (length(active_sets) == 0) {
      message(">> [runner] No active rule sets for sheet: ", src)
      next
    }

    for (rs in active_sets) {
      check_file <- file.path(check_dir, paste0(rs, ".R"))
      message(strrep(">", 20), " Executing: ", rs, " ", strrep("<", 20))

      if (!file.exists(check_file)) {
        message("  WARNING: Check script not found: ", check_file,
                " -- skipping.")
        next
      }

      tryCatch({
        check_env <- new.env(parent = baseenv())
        sys.source(check_file, envir = check_env, keep.source = FALSE)
        fn_name <- paste0("check_", rs)
        if (!exists(fn_name, envir = check_env, inherits = FALSE)) {
          stop("Function '", fn_name, "' not found in ", check_file,
               ". Ensure the function is named check_", rs, "(state, cfg).")
        }
        check_fn <- get(fn_name, envir = check_env, inherits = FALSE)
        state    <- check_fn(state, cfg)
      }, error = function(e) {
        message("  ERROR in rule set ", rs, ": ", conditionMessage(e))
      })

      message(strrep(">", 20), " Finished:  ", rs, " ", strrep("<", 20))
    }
  }

  n_issues <- nrow(state$issues)
  message(">> [runner] All checks executed. Total findings: ", n_issues)
  state
}
