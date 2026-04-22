#' Initialise a CoreGage run session
#'
#' Reads the rule registry (rule_registry.xlsx), builds the active rule
#' switch vector, and initialises all empty master tables required for
#' a check run. This is the first function called in every run.
#'
#' @param cfg A named list of paths produced by sourcing project_config.R.
#'   Must contain: \code{rule_registry}, \code{reports}, \code{feedback}.
#'
#' @return A named list (the \emph{state} object) containing:
#'   \code{rule_registry}, \code{active_rules}, \code{session},
#'   \code{issues}, \code{summary_log}, \code{review_log}.
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
#' state <- setup_coregage(cfg)
#' }
setup_coregage <- function(cfg) {

  message(">> [setup] Starting CoreGage initialisation ...")

  if (!file.exists(cfg$rule_registry)) {
    stop("rule_registry.xlsx not found at: ", cfg$rule_registry)
  }

  #  Read rule registry 
  read_sheet <- function(sheet_name) {

    available <- readxl::excel_sheets(cfg$rule_registry)
    if (!sheet_name %in% available) {
      message("  NOTE: sheet '", sheet_name, "' not found - skipping.")
      return(NULL)
    }

    df <- readxl::read_xlsx(cfg$rule_registry, sheet = sheet_name,
                            col_types = "text")
    message("  Sheet '", sheet_name, "' columns : ",
            paste(names(df), collapse = ", "))
    message("  Sheet '", sheet_name, "' rows    : ", nrow(df))

    # Normalise column names
    names(df)[names(df) == "Category"]    <- "category"
    names(df)[names(df) == "Subcategory"] <- "subcategory"
    names(df)[names(df) == "ID"]          <- "id"
    names(df)[names(df) == "Active"]      <- "active"
    names(df)[names(df) == "DM_Report"]   <- "dm_report"
    names(df)[names(df) == "MW_Report"]   <- "mw_report"
    names(df)[names(df) == "SDTM_Report"] <- "sdtm_report"
    names(df)[names(df) == "ADAM_Report"] <- "adam_report"
    names(df)[names(df) == "Rule_Set"]    <- "rule_set"
    names(df)[names(df) == "Description"] <- "description"
    names(df)[names(df) == "Notes"]       <- "notes"

    required <- c("category", "subcategory", "id", "active",
                  "dm_report", "mw_report", "sdtm_report", "adam_report",
                  "rule_set", "description", "notes")
    for (col in required) {
      if (!col %in% names(df)) df[[col]] <- NA_character_
    }
    df <- df[, required]

    # Debug: first 10 IDs
    message("  First 10 IDs:")
    for (i in seq_len(min(10, nrow(df)))) {
      val <- df$id[i]
      message("    [", i, "] '", val, "'  active='", df$active[i], "'")
    }

    # Filter blanks and placeholders
    df <- df[!is.na(df$id) & trimws(df$id) != "" &
               toupper(trimws(df$id)) != "YOURID", ]

    if (nrow(df) == 0) {
      message("  NOTE: sheet '", sheet_name, "' has no valid rows.")
      return(NULL)
    }

    df$sheet          <- sheet_name
    df$id             <- toupper(trimws(df$id))
    df$active         <- toupper(trimws(df$active))
    df$dm_report      <- toupper(trimws(df$dm_report))
    df$mw_report      <- toupper(trimws(df$mw_report))
    df$sdtm_report    <- toupper(trimws(df$sdtm_report))
    df$adam_report    <- toupper(trimws(df$adam_report))
    df$rule_set       <- trimws(df$rule_set)
    df$category       <- trimws(df$category)
    df$subcategory    <- trimws(df$subcategory)
    df$description    <- trimws(df$description)

    message("  Valid rows from '", sheet_name, "': ", nrow(df))
    df
  }

  trial_df <- read_sheet("Trial")
  study_df <- read_sheet("Study")

  sheets_read <- Filter(Negate(is.null), list(trial_df, study_df))
  if (length(sheets_read) == 0) {
    stop("No valid check definitions found in rule_registry.xlsx. ",
         "Ensure sheets are named 'Trial' and/or 'Study'.")
  }

  rule_registry <- do.call(rbind, sheets_read)
  rule_registry <- rule_registry[order(rule_registry$rule_set,
                                       rule_registry$id), ]

  message(">> [setup] Imported ", nrow(rule_registry),
          " rules from: ",
          paste(unique(rule_registry$sheet), collapse = " + "))

  n_on  <- sum(startsWith(rule_registry$active, "Y"))
  n_off <- nrow(rule_registry) - n_on
  message("  Active: ", n_on, " ON  /  ", n_off, " OFF")

  #  Active rules switch vector 
  active_rules <- structure(
    startsWith(rule_registry$active, "Y"),
    names = rule_registry$id
  )

  session <- list(
    sdate = format(Sys.Date(), "%d%b%Y"),
    stime = format(Sys.time(), "%H:%M")
  )

  #  Empty master tables 
  issues <- data.frame(
    id          = character(0), subj_id     = character(0),
    vis_id      = numeric(0),   description = character(0),
    review      = character(0), stringsAsFactors = FALSE
  )

  summary_log <- data.frame(
    headlink    = character(0), nu          = integer(0),
    rule_set    = character(0), sobs        = character(0),
    stringsAsFactors = FALSE
  )

  review_log <- data.frame(
    id            = character(0), subj_id       = character(0),
    vis_id        = numeric(0),   desrp         = character(0),
    find_dt       = as.Date(character(0)),
    status        = character(0), analyst_note  = character(0),
    analyst_id    = character(0), review_note   = character(0),
    reviewer_id   = character(0), last_mod      = as.POSIXct(character(0)),
    report_type   = character(0), stringsAsFactors = FALSE
  )

  message(">> [setup] Initialisation complete.")

  list(
    rule_registry = rule_registry,
    active_rules  = active_rules,
    session       = session,
    issues        = issues,
    summary_log   = summary_log,
    review_log    = review_log
  )
}
