#' Count valid observations in a findings data frame
#'
#' Returns the number of rows in \code{df}, optionally excluding rows
#' that could unblind the study (negative subject IDs where none of
#' the unblinding topic codes appear in the description).
#'
#' @param df A data frame with at least \code{subj_id} and
#'   \code{description} columns.
#' @param unblind_codes Character vector of topic codes that flag
#'   potential unblinding. Defaults to \code{character(0)} (no exclusion).
#'
#' @return Integer. Number of valid rows.
#'
#' @export
#' @examples
#' df <- data.frame(subj_id = c("001", "-002"),
#'                  description = c("Issue A", "Issue B"),
#'                  stringsAsFactors = FALSE)
#' count_valid(df)
count_valid <- function(df, unblind_codes = character(0)) {

  if (nrow(df) == 0) return(0L)

  if (length(unblind_codes) > 0 && "subj_id" %in% names(df) &&
      "description" %in% names(df)) {
    is_negative  <- grepl("^-", as.character(df$subj_id))
    has_code     <- vapply(df$description, function(desc) {
      any(grepl(unblind_codes, desc, fixed = TRUE))
    }, logical(1))
    df <- df[!(is_negative & !has_code), ]
  }

  nrow(df)
}
