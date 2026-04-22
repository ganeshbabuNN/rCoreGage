#' Collect findings from a single check into the master issues table
#'
#' Called at the end of every individual check block. Validates the
#' findings data frame, counts valid observations via
#' \code{\link{count_valid}}, and appends to \code{state$issues} and
#' \code{state$summary_log}.
#'
#' @param state The current state object.
#' @param df A data frame produced by the check. Must contain columns:
#'   \code{subj_id} (character), \code{vis_id} (numeric),
#'   \code{description} (character, max 200 chars).
#' @param id Character. The check ID matching the ID column in
#'   \code{rule_registry.xlsx}.
#' @param desc_col Character. Name of the description column in \code{df}.
#'   Defaults to \code{"description"}.
#' @param sobs Logical. Whether to limit output to 20 observations.
#'   Defaults to \code{TRUE}.
#' @param unblind_codes Character vector. Topic codes that could unblind
#'   the study. Rows with negative subject IDs where these codes are
#'   absent from the description are excluded. Defaults to \code{character(0)}.
#'
#' @return Updated \code{state} object.
#'
#' @export
#' @examples
#' \donttest{
#' # Build a minimal state object
#' state <- list(
#'   issues = data.frame(id = character(0), subj_id = character(0),
#'                       vis_id = numeric(0), description = character(0),
#'                       review = character(0), stringsAsFactors = FALSE),
#'   summary_log = data.frame(headlink = character(0), nu = integer(0),
#'                            rule_set = character(0), sobs = character(0),
#'                            stringsAsFactors = FALSE)
#' )
#'
#' # A findings data frame produced by a check
#' MY_CHECK <- data.frame(
#'   subj_id     = c("SUBJ-001", "SUBJ-002"),
#'   vis_id      = NA_real_,
#'   description = c("AESEV missing for RASH", "AESEV missing for HEADACHE"),
#'   stringsAsFactors = FALSE
#' )
#'
#' state <- collect_findings(state, MY_CHECK, id = "AECHK001")
#' }
collect_findings <- function(state, df, id,
                             desc_col       = "description",
                             sobs           = TRUE,
                             unblind_codes  = character(0)) {

  if (is.null(df) || !is.data.frame(df)) {
    message("  WARNING [collector]: dataset for ", id,
            " is NULL or not a data frame. Skipping.")
    return(state)
  }
  if (!"subj_id" %in% names(df)) {
    message("  WARNING [collector]: subj_id column missing for ", id,
            ". Skipping.")
    return(state)
  }
  if (!desc_col %in% names(df)) {
    message("  WARNING [collector]: description column '", desc_col,
            "' missing for ", id, ". Skipping.")
    return(state)
  }

  missing_desc <- df[is.na(df[[desc_col]]) | trimws(df[[desc_col]]) == "", ]
  if (nrow(missing_desc) > 0) {
    message("  WARNING [collector]: ", nrow(missing_desc),
            " rows have empty descriptions for ", id, ". Skipping.")
    return(state)
  }

  n <- count_valid(df, unblind_codes)
  message("  >> [collector] Appending ", n, " finding(s) for: ", id)

  new_issues <- df
  names(new_issues)[names(new_issues) == desc_col] <- "description"

  keep_cols <- intersect(c("subj_id", "vis_id", "description"),
                         names(new_issues))
  new_issues <- new_issues[, keep_cols, drop = FALSE]

  # Ensure vis_id column exists before column operations
  if (!"vis_id" %in% names(new_issues)) {
    new_issues$vis_id <- NA_real_
  } else {
    new_issues$vis_id <- suppressWarnings(as.numeric(new_issues$vis_id))
  }

  # Only perform row-level assignments when rows exist
  if (nrow(new_issues) > 0) {
    new_issues$id          <- id
    new_issues$review      <- "ANALYST"
    new_issues$subj_id     <- as.character(new_issues$subj_id)
    new_issues$description <- substr(as.character(new_issues$description),
                                     1, 200)
  } else {
    # 0-row df: add columns with correct types but no rows
    new_issues$id     <- character(0)
    new_issues$review <- character(0)
  }

  new_issues <- unique(new_issues[, c("id", "subj_id", "vis_id",
                                      "description", "review")])
  state$issues <- unique(rbind(state$issues, new_issues))

  log_row <- data.frame(
    headlink = id,
    nu       = as.integer(n),
    rule_set = id,
    sobs     = if (sobs) "Y" else "N",
    stringsAsFactors = FALSE
  )
  state$summary_log <- rbind(state$summary_log, log_row)

  state
}
