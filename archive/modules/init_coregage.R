# ==============================================================================
# modules/init_coregage.R
# Description : Initialisation module for CoreGage.
#   - Imports master_checks.xlsx (Project + Trial sheets)
#   - Builds named logical switch vector per check ID
#   - Initialises empty master tables for findings and reviewer comments
#   - Creates empty open / closed issue tables on first run
# ==============================================================================

init_coregage <- function(config) {

  message(">> [init_coregage] Starting initialisation ...")

  master_file <- file.path(config$masterlib, "master_checks.xlsx")
  if (!file.exists(master_file)) {
    stop("master_checks.xlsx not found at: ", master_file)
  }

  read_sheet <- function(sheet_name) {
    available <- readxl::excel_sheets(master_file)
    if (!sheet_name %in% available) {
      message("  NOTE: sheet '", sheet_name, "' not found - skipping.")
      return(NULL)
    }
    df <- readxl::read_xlsx(master_file, sheet = sheet_name, col_types = "text")
    message("  Sheet '", sheet_name, "' columns : ", paste(names(df), collapse = ", "))
    message("  Sheet '", sheet_name, "' total rows : ", nrow(df))

    names(df)[names(df) == "Category"]    <- "category"
    names(df)[names(df) == "Subcategory"] <- "subcategory"
    names(df)[names(df) == "ID"]          <- "id"
    names(df)[names(df) == "Switch_on"]   <- "switch_on"
    names(df)[names(df) == "DM"]          <- "dm"
    names(df)[names(df) == "TVP"]         <- "tvp"
    names(df)[names(df) == "Batch_ID"]    <- "batch_id"
    names(df)[names(df) == "Headtxt"]     <- "headtxt"
    names(df)[names(df) == "Parameters"]  <- "parameters"
    names(df)[names(df) == "Comment"]     <- "comment"
    names(df)[names(df) == "Comments"]    <- "comment"

    required <- c("category","subcategory","id","switch_on",
                  "dm","tvp","batch_id","headtxt","parameters","comment")
    for (col in required) {
      if (!col %in% names(df)) df[[col]] <- NA_character_
    }
    df <- df[, required]

    message("  First 10 raw ID values:")
    for (i in seq_len(min(10, nrow(df)))) {
      val <- df$id[i]
      message("    [", i, "] value='", val, "'  is.na=", is.na(val),
              "  nchar=", ifelse(is.na(val),"NA",nchar(trimws(val))),
              "  trimmed='", ifelse(is.na(val),"NA",trimws(val)), "'")
    }
    message("  First 10 raw Switch_on values:")
    for (i in seq_len(min(10, nrow(df)))) {
      message("    [", i, "] '", df$switch_on[i], "'")
    }

    before <- nrow(df)
    df <- df[!is.na(df$id), ]
    message("  After removing NA ids       : ", nrow(df), " rows (removed ", before - nrow(df), ")")
    before <- nrow(df)
    df <- df[trimws(df$id) != "", ]
    message("  After removing blank ids    : ", nrow(df), " rows (removed ", before - nrow(df), ")")
    before <- nrow(df)
    df <- df[toupper(trimws(df$id)) != "YOURID", ]
    message("  After removing YOURID rows  : ", nrow(df), " rows (removed ", before - nrow(df), ")")

    if (nrow(df) == 0) {
      message("  NOTE: sheet '", sheet_name, "' has no valid rows after filtering.")
      return(NULL)
    }
    df$sheet       <- sheet_name
    df$id          <- toupper(trimws(df$id))
    df$switch_on   <- toupper(trimws(df$switch_on))
    df$dm          <- toupper(trimws(df$dm))
    df$tvp         <- toupper(trimws(df$tvp))
    df$batch_id    <- trimws(df$batch_id)
    df$category    <- trimws(df$category)
    df$subcategory <- trimws(df$subcategory)
    df$headtxt     <- trimws(df$headtxt)
    message("  Final rows imported from '", sheet_name, "': ", nrow(df))
    df
  }

  project_df  <- read_sheet("Project")
  trial_df    <- read_sheet("Trial")
  sheets_read <- Filter(Negate(is.null), list(project_df, trial_df))

  if (length(sheets_read) == 0) stop("No valid check definitions found in master_checks.xlsx.")

  master_checks <- do.call(rbind, sheets_read)
  master_checks <- master_checks[order(master_checks$batch_id, master_checks$id), ]

  message(">> [init_coregage] Imported ", nrow(master_checks), " check definitions from: ",
          paste(unique(master_checks$sheet), collapse = " + "))
  message("  Check IDs : ", paste(head(master_checks$id, 10), collapse = ", "),
          if (nrow(master_checks) > 10) " ..." else "")

  n_on  <- sum(startsWith(master_checks$switch_on, "Y"))
  message("  Switches  : ", n_on, " ON  /  ", nrow(master_checks) - n_on, " OFF")

  switches <- setNames(startsWith(master_checks$switch_on, "Y"), master_checks$id)
  run_meta <- list(sdate = format(Sys.Date(), "%d%b%Y"), stime = format(Sys.time(), "%H:%M"))

  headlines <- data.frame(status=character(0), headlink=character(0), nu=integer(0),
                          cmv=character(0), sobs=character(0), info=character(0),
                          stringsAsFactors=FALSE)

  findings <- data.frame(id=character(0), subj_id=character(0), vis_id=numeric(0),
                         description=character(0), review=character(0),
                         stringsAsFactors=FALSE)

  reviewer_comments_all <- data.frame(
    id=character(0), subj_id=character(0), vis_id=numeric(0),
    desrp=character(0), find_dt=as.Date(character(0)), status=character(0),
    prog_comment=character(0), prog_initials=character(0),
    reviewer_comment=character(0), reviewer_initials=character(0),
    last_mod=as.POSIXct(character(0)), stringsAsFactors=FALSE)

  empty_issue_table <- function() {
    data.frame(sheet=character(0), category=character(0), subcategory=character(0),
               id=character(0), subj_id=character(0), vis_id=numeric(0),
               desrp=character(0), find_dt=as.Date(character(0)), status=character(0),
               prog_comment=character(0), prog_initials=character(0),
               reviewer_comment=character(0), reviewer_initials=character(0),
               stringsAsFactors=FALSE)
  }

  message(">> [init_coregage] Initialisation complete.")

  list(master_checks=master_checks, switches=switches, run_meta=run_meta,
       headlines=headlines, findings=findings,
       reviewer_comments_all=reviewer_comments_all,
       open_issues=empty_issue_table(), closed_issues=empty_issue_table())
}
