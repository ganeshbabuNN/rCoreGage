# ==============================================================================
# modules/exec_coregage.R
# Description : Execution module for CoreGage.
#   - Determines check sources (Trial and/or Project)
#   - Loops through all active batch IDs
#   - Sources each check program and calls run_<batch_id>(ctx, config)
# ==============================================================================

exec_coregage <- function(config, ctx) {

  message(">> [exec_coregage] Starting check execution ...")

  sources <- unique(ctx$master_checks$sheet)

  for (src in sources) {
    checklib <- if (src == "Trial") config$tchecklib else config$pchecklib

    active_batches <- ctx$master_checks |>
      dplyr::filter(sheet == src, startsWith(switch_on, "Y")) |>
      dplyr::pull(batch_id) |>
      unique() |>
      sort()

    if (length(active_batches) == 0) {
      message(">> [exec_coregage] No active checks for source: ", src)
      next
    }

    for (batch in active_batches) {
      check_file <- file.path(checklib, paste0(batch, ".R"))
      message(strrep(">", 20), " Executing: ", batch, " ", strrep("<", 20))

      if (!file.exists(check_file)) {
        message("WARNING: Check file not found: ", check_file, " -- skipping.")
        next
      }

      tryCatch(
        local({
          source(check_file, local = TRUE)
          run_fn <- get(paste0("run_", batch))
          ctx <<- run_fn(ctx, config)
        }),
        error = function(e) {
          message("ERROR in check ", batch, ": ", conditionMessage(e))
        }
      )

      message(strrep(">", 20), " Finished:  ", batch, " ", strrep("<", 20))
    }
  }

  message(">> [exec_coregage] All checks executed.")
  ctx
}
