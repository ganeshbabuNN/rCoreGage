#' Consolidate findings and write role-based Excel reports
#'
#' Merges new findings with previously saved issues, incorporates reviewer
#' feedback from role-separated feedback folders, and writes four
#' role-specific reports plus \code{all_open.xlsx} and
#' \code{all_closed.xlsx} to \code{cfg$reports}.
#'
#' @details
#' \strong{Feedback loop:}
#' After distributing reports to reviewers, they place updated files in
#' the appropriate subfolder under \code{cfg$feedback}:
#' \itemize{
#'   \item \code{feedback/DM/}   - Data Management reviewer
#'   \item \code{feedback/MW/}   - Medical Writing reviewer
#'   \item \code{feedback/SDTM/} - SDTM programmer
#'   \item \code{feedback/ADAM/} - ADaM programmer
#' }
#' On the next run, \code{build_reports()} reads the most recent file
#' from each feedback folder, merges \code{analyst_note},
#' \code{review_note} and status updates back into the findings, and
#' overwrites the reports with the merged version.
#'
#' @param cfg A named list of paths from \code{project_config.R}.
#' @param state The state object returned by \code{\link{run_checks}}.
#'
#' @return Invisibly \code{NULL}. Reports are written to \code{cfg$reports}.
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
#' state         <- setup_coregage(cfg)
#' state$domains <- load_inputs(cfg)
#' state         <- run_checks(cfg, state)
#' build_reports(cfg, state)
#' }
build_reports <- function(cfg, state) {

  message(">> [reporter] Starting consolidation ...")

  sdate <- state$session$sdate
  stime <- state$session$stime

  clean <- .cleanup_text

  # Parse a date column that may contain Excel serial numbers (numeric strings),
  # ISO date strings "2024-01-15", or formatted strings "15Jan2024".
  # Returns a Date vector; NAs are preserved rather than crashing.
  .parse_date_col <- function(x) {
    if (inherits(x, "Date")) return(x)
    x <- as.character(x)
    result <- suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30"))
    # Where numeric conversion failed, try ISO string
    na_idx <- is.na(result)
    if (any(na_idx)) {
      result[na_idx] <- suppressWarnings(as.Date(x[na_idx]))
    }
    # Where ISO also failed, try ddMMMYYYY format e.g. "15Jan2024"
    na_idx <- is.na(result)
    if (any(na_idx)) {
      result[na_idx] <- suppressWarnings(
        as.Date(x[na_idx], format = "%d%b%Y"))
    }
    result
  }


  #  Import previously saved issues 
  import_saved <- function(fname) {
    path <- file.path(cfg$reports, fname)
    if (!file.exists(path)) return(NULL)
    message("  Importing saved issues: ", fname)
    df <- tryCatch(
      readxl::read_xlsx(path, sheet = "Details",
                        skip = 2, col_types = "text"),
      error = function(e) {
        message("  WARNING: cannot read ", basename(path),
                " -- ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)

    col_map <- c(
      "CHECK ID"    = "id",      "SUBJECT ID"   = "subj_id",
      "VISIT ID"    = "vis_id",  "DESCRIPTION"  = "desrp",
      "FIND DATE"   = "find_dt", "STATUS"       = "status",
      "ANALYST NOTE"= "analyst_note", "ANALYST ID"= "analyst_id"
    )
    for (old in names(col_map)) {
      if (old %in% names(df)) names(df)[names(df) == old] <- col_map[[old]]
    }
    needed <- c("id","subj_id","vis_id","desrp","find_dt","status",
                "analyst_note","analyst_id")
    for (col in needed) if (!col %in% names(df)) df[[col]] <- NA_character_

    df <- df[!is.na(df$find_dt), ]
    if ("analyst_note" %in% names(df)) {
      is_placeholder <- !is.na(df$analyst_note) &
                        toupper(trimws(df$analyst_note)) == "THIS IS EMPTY"
      df <- df[!is_placeholder, ]
    }
    if (nrow(df) == 0) return(NULL)

    df$vis_id  <- suppressWarnings(as.numeric(df$vis_id))
    df$find_dt <- .parse_date_col(df$find_dt)
    df$desrp       <- clean(df$desrp)
    df$analyst_note<- clean(df$analyst_note)
    df$dup_id      <- gsub("\\s", "", df$desrp)
    out <- df[, c("id","subj_id","vis_id","desrp","dup_id","find_dt",
                  "status","analyst_note","analyst_id")]
    message("    -> ", nrow(out), " saved issue(s) loaded from ", fname)
    out
  }

  # Replace NA vis_id with sentinel so merge() treats them as equal.
  # merge() in R does NOT match NA==NA, so we use -1 as a sentinel key.
  .add_vis_sentinel  <- function(df) {
    if ("vis_id" %in% names(df))
      df$vis_id <- ifelse(is.na(df$vis_id), -1, df$vis_id)
    df
  }
  .drop_vis_sentinel <- function(df) {
    if ("vis_id" %in% names(df))
      df$vis_id <- ifelse(df$vis_id == -1, NA_real_, df$vis_id)
    df
  }

  old_open   <- import_saved("all_open.xlsx")
  old_closed <- import_saved("all_closed.xlsx")

  empty_old <- function() {
    data.frame(id=character(0),subj_id=character(0),vis_id=numeric(0),
               desrp=character(0),dup_id=character(0),
               find_dt=as.Date(character(0)),status=character(0),
               analyst_note=character(0),analyst_id=character(0),
               stringsAsFactors=FALSE)
  }

  olddata <- if (!is.null(old_open) || !is.null(old_closed))
    dplyr::bind_rows(
      if (!is.null(old_open))   old_open   else empty_old(),
      if (!is.null(old_closed)) old_closed else empty_old()
    )
  else empty_old()

  has_new <- nrow(state$issues) > 0
  has_old <- nrow(olddata) > 0

  #  Merge new findings with old 
  if (!has_new && !has_old) {
    message("  No findings to report. Writing empty reports.")
    issuelist <- data.frame(
      id=character(0), subj_id=character(0), vis_id=numeric(0),
      desrp=character(0), dup_id=character(0),
      find_dt=as.Date(character(0)), status=character(0),
      analyst_note=character(0), analyst_id=character(0),
      review_note=character(0), reviewer_id=character(0),
      category=character(0), subcategory=character(0),
      description=character(0), dm_report=character(0),
      mw_report=character(0), sdtm_report=character(0),
      adam_report=character(0), stringsAsFactors=FALSE)
  } else {

    if (has_new) {
      newf <- state$issues
      names(newf)[names(newf) == "description"] <- "desrp"
      newf <- unique(newf[, c("id","subj_id","vis_id","desrp")])
      newf$dup_id <- gsub("\\s", "", newf$desrp)
      newf$.new   <- TRUE
      newf        <- .add_vis_sentinel(newf)
    } else {
      newf <- data.frame(id=character(0), subj_id=character(0),
                         vis_id=numeric(0), desrp=character(0),
                         dup_id=character(0), .new=logical(0),
                         stringsAsFactors=FALSE)
    }

    if (nrow(olddata) > 0) {
      olddata$.old <- TRUE
      olddata <- .add_vis_sentinel(olddata)
    }

    if (nrow(newf) > 0 && nrow(olddata) > 0) {
      merged <- merge(newf, olddata,
                      by = c("id","subj_id","vis_id","dup_id"),
                      all.x = TRUE, all.y = TRUE)
      merged$.new <- ifelse(is.na(merged$.new), FALSE, merged$.new)
      merged$.old <- ifelse(is.na(merged$.old), FALSE, merged$.old)
    } else if (nrow(newf) > 0) {
      merged <- newf
      merged$.old         <- FALSE
      merged$find_dt      <- as.Date(NA)
      merged$status       <- NA_character_
      merged$analyst_note <- NA_character_
      merged$analyst_id   <- NA_character_
      merged$desrp.x      <- merged$desrp
      merged$desrp.y      <- NA_character_
    } else {
      merged <- olddata
      merged$.new    <- FALSE
      merged$desrp.x <- NA_character_
      merged$desrp.y <- merged$desrp
    }

    if ("desrp.x" %in% names(merged) && "desrp.y" %in% names(merged)) {
      merged$desrp <- ifelse(!is.na(merged$desrp.x),
                             merged$desrp.x, merged$desrp.y)
      merged$desrp.x <- merged$desrp.y <- NULL
    }

    merged <- .drop_vis_sentinel(merged)

    for (col in c("status","analyst_note","analyst_id","find_dt")) {
      if (!col %in% names(merged)) {
        merged[[col]] <- if (col == "find_dt") as.Date(NA) else NA_character_
      }
    }

    # Preserve original old status before reassignment
    merged$old_status <- ifelse(is.na(merged$status), "", merged$status)

    # Assign status (pre-feedback merge pass  uses analyst note/id only)
    # Reviewer-based closure is handled after feedback merge in Part 3.
    merged$status <- mapply(function(is_new, is_old, st, cmt, init) {
      st_l  <- tolower(trimws(ifelse(is.na(st),"",st)))
      cmt_l <- tolower(trimws(ifelse(is.na(cmt),"",cmt)))
      hi    <- nchar(trimws(ifelse(is.na(init),"",init))) > 0
      by_an <- grepl("closed by analyst", cmt_l)
      if  (is_new && !is_old)                                      return("Open")
      if  (is_new &&  is_old && st_l != "closed")                  return(st)
      if  (is_new &&  is_old && st_l == "closed" && by_an && hi)   return("Closed")
      if  (is_new &&  is_old && st_l == "closed" && by_an && !hi)  return("Open")
      if  (is_new &&  is_old && st_l == "closed" && !by_an)        return("Open")
      if (!is_new &&  is_old)                                       return("Closed")
      return(ifelse(is.na(st), "Open", st))
    }, merged$.new, merged$.old,
    ifelse(is.na(merged$status),"",merged$status),
    ifelse(is.na(merged$analyst_note),"",merged$analyst_note),
    ifelse(is.na(merged$analyst_id),"",merged$analyst_id),
    SIMPLIFY = TRUE)

    # Auto-update analyst_note
    # old_status = status from previous run (before this run's reassignment)
    merged$analyst_note <- mapply(function(is_new, is_old, st, old_st, cmt) {
      ac     <- paste0("[Not in data anymore (", sdate, ").]")
      ro     <- paste0("[Was closed but re-appeared (", sdate, ").]")
      cout   <- trimws(ifelse(is.na(cmt),"",cmt))
      old_stl <- tolower(trimws(ifelse(is.na(old_st),"",old_st)))
      # Finding dropped out of data
      if (!is_new && is_old && !grepl("not in data anymore", tolower(cout)))
        return(trimws(paste(gsub("\\[Not in data.*?\\]", "", cout), ac)))
      # Finding was previously CLOSED but is now back in data
      # Only fire "re-appeared" tag if finding was closed by ANALYST
      # (not by reviewer) and has now re-appeared in the data.
      by_reviewer <- grepl("closed by reviewer", tolower(cout))
      if (is_new && is_old && old_stl == "closed" &&
          !by_reviewer &&
          !grepl("closed by analyst", tolower(cout)) &&
          !grepl("was closed but re-appeared", tolower(cout)))
        return(trimws(paste(gsub("\\[Was closed.*?\\]", "", cout), ro)))
      # If closed by reviewer and still in data, keep Closed (no tag needed)
      if (is_new && is_old && old_stl == "closed" && by_reviewer)
        return(cout)
      return(cout)
    }, merged$.new, merged$.old, merged$status, merged$old_status,
    ifelse(is.na(merged$analyst_note),"",merged$analyst_note),
    SIMPLIFY=TRUE)

    # Remove helper column
    merged$old_status <- NULL

    merged$find_dt <- as.Date(
      ifelse(is.na(merged$find_dt), as.numeric(Sys.Date()),
             as.numeric(merged$find_dt)),
      origin = "1970-01-01")

    merged$.new <- merged$.old <- NULL
    issuelist0  <- unique(merged)

    # Attach rule registry metadata
    reg_sub <- state$rule_registry[, c("id","category","subcategory",
                                        "description","dm_report",
                                        "mw_report","sdtm_report",
                                        "adam_report","sheet")]
    issuelist1 <- merge(reg_sub, issuelist0, by = "id", all.y = TRUE)

    #  Incorporate role-based feedback 
    issuelist <- issuelist1
    issuelist$review_note  <- ""
    issuelist$reviewer_id  <- ""

    roles <- c("DM", "MW", "SDTM", "ADAM")
    for (role in roles) {
      fb_dir   <- file.path(cfg$feedback, role)
      if (!dir.exists(fb_dir)) next
      fb_files <- list.files(fb_dir, pattern = "\\.xlsx$",
                             full.names = TRUE, ignore.case = TRUE)
      if (length(fb_files) == 0) next

      message("  Importing feedback from: feedback/", role, "/ (",
              length(fb_files), " file(s))")

      fb_list <- lapply(fb_files, function(fpath) {
        tryCatch({
          df <- readxl::read_xlsx(fpath, sheet = "Details",
                                  skip = 2, col_types = "text")
          col_map2 <- c(
            "CHECK ID"="id","SUBJECT ID"="subj_id","VISIT ID"="vis_id",
            "DESCRIPTION"="desrp","FIND DATE"="find_dt","STATUS"="status",
            "ANALYST NOTE"="analyst_note","ANALYST ID"="analyst_id",
            "REVIEW NOTE"="review_note","REVIEWER ID"="reviewer_id"
          )
          for (o in names(col_map2)) {
            if (o %in% names(df)) names(df)[names(df)==o] <- col_map2[[o]]
          }
          df <- df[!is.na(df$find_dt), ]
          if (nrow(df) == 0) return(NULL)
          df$vis_id  <- suppressWarnings(as.numeric(df$vis_id))
          df$find_dt <- .parse_date_col(df$find_dt)
          df$last_mod   <- file.mtime(fpath)
          df$dup_id     <- gsub("\\s","",clean(df$desrp))
          df$desrp      <- clean(df$desrp)
          df$role       <- role
          if ("review_note" %in% names(df))
            df$review_note <- clean(df$review_note)
          df
        }, error=function(e){NULL})
      })

      fb_all <- do.call(dplyr::bind_rows, Filter(Negate(is.null), fb_list))
      if (is.null(fb_all) || nrow(fb_all) == 0) {
        message("    -> 0 rows could be read from feedback/", role,
                "/ files - check file is not open in Excel")
        next
      }

      fb_all  <- fb_all[order(fb_all$id, fb_all$subj_id,
                               fb_all$vis_id, fb_all$dup_id,
                               fb_all$last_mod, decreasing=TRUE), ]
      holder  <- fb_all[!duplicated(fb_all[, c("id","subj_id",
                                                "vis_id","dup_id")]), ]
      keep    <- intersect(c("id","subj_id","vis_id","dup_id",
                             "analyst_note","analyst_id",
                             "review_note","reviewer_id","status"),
                           names(holder))
      holder  <- holder[, keep]
      names(holder)[names(holder)=="status"]       <- "fb_status"
      names(holder)[names(holder)=="analyst_note"] <- "fb_analyst_note"
      names(holder)[names(holder)=="analyst_id"]   <- "fb_analyst_id"

      # Count how many rows have an actual reviewer note or status change
      n_review_noted  <- sum(!is.na(holder$review_note) &
                             trimws(holder$review_note) != "", na.rm = TRUE)
      n_analyst_noted <- sum(!is.na(holder$fb_analyst_note) &
                             trimws(holder$fb_analyst_note) != "", na.rm = TRUE)
      n_status <- sum(!is.na(holder$fb_status) &
                       trimws(holder$fb_status) != "", na.rm = TRUE)
      message("    -> ", nrow(holder), " finding(s) matched from feedback/", role, "/")
      message("         status updates: ", n_status,
              "  |  analyst notes: ", n_analyst_noted,
              "  |  review notes: ",  n_review_noted)

      if (!"dup_id" %in% names(issuelist)) {
        issuelist$dup_id <- gsub("\\s", "", ifelse(is.na(issuelist$desrp),
                                                    "", issuelist$desrp))
      }
      issuelist <- .add_vis_sentinel(issuelist)
      holder    <- .add_vis_sentinel(holder)
      issuelist <- merge(issuelist, holder,
                         by = c("id","subj_id","vis_id","dup_id"),
                         all.x = TRUE, suffixes = c("",".fb"))

      upd <- !is.na(issuelist$fb_status) &
               trimws(issuelist$fb_status) != ""
      issuelist$status[upd] <- issuelist$fb_status[upd]

      # For reviewer-closed findings: mark as reviewer-closed so auto_note
      # on the next run does not treat them as "re-appeared".
      # A reviewer closure is identified by: status=Closed AND reviewer_id present.
      is_reviewer_closed <- !is.na(issuelist$status) &
        tolower(trimws(issuelist$status)) == "closed" &
        ( (!is.na(issuelist$reviewer_id) & trimws(issuelist$reviewer_id) != "") |
          (!is.na(issuelist$review_note) & trimws(issuelist$review_note) != "") )
      # Tag the analyst_note so the next run knows this was reviewer-closed
      if (any(is_reviewer_closed, na.rm=TRUE)) {
        tag <- "[closed by reviewer]"
        issuelist$analyst_note[is_reviewer_closed] <- trimws(
          paste(
            ifelse(is.na(issuelist$analyst_note[is_reviewer_closed]), "",
                   issuelist$analyst_note[is_reviewer_closed]),
            tag))
        # Deduplicate the tag if already present
        issuelist$analyst_note <- gsub(
          "(\\[closed by reviewer\\]\\s*)+",
          "[closed by reviewer] ",
          issuelist$analyst_note)
        issuelist$analyst_note <- trimws(issuelist$analyst_note)
      }

      # Merge review notes
      if ("review_note.fb" %in% names(issuelist)) {
        issuelist$review_note <- ifelse(
          !is.na(issuelist$review_note.fb) & issuelist$review_note.fb != "",
          issuelist$review_note.fb, issuelist$review_note)
        issuelist$review_note.fb <- NULL
      }
      if ("reviewer_id.fb" %in% names(issuelist)) {
        issuelist$reviewer_id <- ifelse(
          !is.na(issuelist$reviewer_id.fb) & issuelist$reviewer_id.fb != "",
          issuelist$reviewer_id.fb, issuelist$reviewer_id)
        issuelist$reviewer_id.fb <- NULL
      }
      issuelist$fb_status <- NULL

      # Merge analyst_note from feedback (reviewer may have corrected it)
      if ("fb_analyst_note" %in% names(issuelist)) {
        issuelist$analyst_note <- ifelse(
          !is.na(issuelist$fb_analyst_note) &
            trimws(issuelist$fb_analyst_note) != "",
          issuelist$fb_analyst_note,
          ifelse(is.na(issuelist$analyst_note), "",
                 issuelist$analyst_note))
        issuelist$fb_analyst_note <- NULL
      }
      if ("fb_analyst_id" %in% names(issuelist)) {
        issuelist$analyst_id <- ifelse(
          !is.na(issuelist$fb_analyst_id) &
            trimws(issuelist$fb_analyst_id) != "",
          issuelist$fb_analyst_id,
          ifelse(is.na(issuelist$analyst_id), "",
                 issuelist$analyst_id))
        issuelist$fb_analyst_id <- NULL
      }

      issuelist <- .drop_vis_sentinel(issuelist)
    }

    # Clean dup_id column from final output
    if ("dup_id" %in% names(issuelist)) issuelist$dup_id <- NULL

    n_with_review  <- sum(
      !is.na(issuelist$review_note) & trimws(issuelist$review_note) != "",
      na.rm = TRUE)
    n_with_analyst <- sum(
      !is.na(issuelist$analyst_note) & trimws(issuelist$analyst_note) != "" &
      !grepl("^\\[", trimws(issuelist$analyst_note)),
      na.rm = TRUE)
    n_closed <- sum(tolower(trimws(issuelist$status)) == "closed", na.rm = TRUE)
    n_queried<- sum(tolower(trimws(issuelist$status)) == "queried", na.rm = TRUE)
    n_open   <- sum(tolower(trimws(issuelist$status)) == "open",    na.rm = TRUE)
    message("  -------------------------------------------------------")
    message("  Feedback summary:")
    message("    Notes  : analyst notes: ", n_with_analyst,
            "  |  reviewer notes: ", n_with_review)
    message("    Status : open: ", n_open,
            "  |  queried: ", n_queried,
            "  |  closed: ", n_closed)
    message("  -------------------------------------------------------")
  }

  #  Split open / closed 
  open_iss   <- issuelist[tolower(trimws(issuelist$status)) != "closed", ]
  closed_iss <- issuelist[tolower(trimws(issuelist$status)) == "closed", ]

  #  Build summary 
  count_by <- function(df, col) {
    if (nrow(df) == 0) {
      out <- data.frame(id=character(0), x=integer(0), stringsAsFactors=FALSE)
      names(out) <- c("id", col)
      return(out)
    }
    tbl <- as.data.frame(table(df$id), stringsAsFactors=FALSE)
    names(tbl) <- c("id", col)
    tbl
  }

  n_open   <- count_by(open_iss,   "n_open")
  n_closed <- count_by(closed_iss, "n_closed")

  reg_head <- state$rule_registry[, c("id","description","category",
                                       "dm_report","mw_report",
                                       "sdtm_report","adam_report")]
  sl       <- state$summary_log[, c("headlink","nu")]
  names(sl)[names(sl)=="headlink"] <- "id"

  head_sum <- merge(reg_head, sl,       by="id", all.x=TRUE)
  head_sum <- merge(head_sum, n_open,   by="id", all.x=TRUE)
  head_sum <- merge(head_sum, n_closed, by="id", all.x=TRUE)
  head_sum$nu       <- ifelse(is.na(head_sum$nu),       0L, head_sum$nu)
  head_sum$n_open   <- ifelse(is.na(head_sum$n_open),   0L, head_sum$n_open)
  head_sum$n_closed <- ifelse(is.na(head_sum$n_closed), 0L, head_sum$n_closed)

  #  Write reports 
  if (!dir.exists(cfg$reports)) dir.create(cfg$reports, recursive=TRUE)

  run_label <- paste("Last run:", sdate, stime)

  make_report <- function(out_name, sum_df, det_df, title) {
    outpath <- file.path(cfg$reports, paste0(out_name, ".xlsx"))
    message("  Writing: ", basename(outpath))

    if (nrow(sum_df) == 0) {
      sum_df <- data.frame(description="No issues found", id="N/A",
                           nu=0L, n_open=0L, n_closed=0L,
                           stringsAsFactors=FALSE)
    }
    if (nrow(det_df) == 0) {
      det_df <- data.frame(
        id="N/A", subj_id="N/A", vis_id=0,
        desrp="No issues found", find_dt=Sys.Date(),
        status="N/A", analyst_note="This is empty",
        analyst_id="", review_note="", reviewer_id="",
        stringsAsFactors=FALSE)
    }
    for (col in c("review_note","reviewer_id")) {
      if (!col %in% names(det_df)) det_df[[col]] <- ""
    }

    wb <- openxlsx::createWorkbook()

    # Summary sheet
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary",
                        x = paste("CoreGage Report -", title), startRow=1)
    openxlsx::writeData(wb, "Summary", x = run_label, startRow=2)

    s_out <- sum_df[, intersect(c("description","id","nu","n_open","n_closed"),
                                names(sum_df))]
    names(s_out) <- c("Check Description","Check ID","New","Open","Closed")[
      seq_len(ncol(s_out))]
    openxlsx::writeDataTable(wb, "Summary", x=s_out,
                             startRow=4, tableStyle="TableStyleMedium9")

    sum_df$n_open   <- ifelse(is.na(sum_df$n_open),   0L, sum_df$n_open)
    sum_df$n_closed <- ifelse(is.na(sum_df$n_closed), 0L, sum_df$n_closed)

    for (r in seq_len(nrow(sum_df))) {
      openxlsx::addStyle(wb, "Summary",
        style=openxlsx::createStyle(
          fgFill=if (isTRUE(sum_df$n_open[r]==0)) "#C6EFCE" else "#FFEB9C"),
        rows=r+4, cols=4, stack=TRUE)
      openxlsx::addStyle(wb, "Summary",
        style=openxlsx::createStyle(
          fgFill=if (isTRUE(sum_df$n_closed[r]==0)) "#FFC7CE" else "#FFEB9C"),
        rows=r+4, cols=5, stack=TRUE)
    }

    # Details sheet
    openxlsx::addWorksheet(wb, "Details")
    openxlsx::writeData(wb, "Details",
                        x = paste("All", title, "Issues"), startRow=1)
    openxlsx::writeData(wb, "Details", x = run_label, startRow=2)

    keep <- c("id","subj_id","vis_id","desrp","find_dt","status",
              "analyst_note","analyst_id","review_note","reviewer_id")
    dout <- det_df[, intersect(keep, names(det_df))]
    lbls <- c("CHECK ID","SUBJECT ID","VISIT ID","DESCRIPTION","FIND DATE",
              "STATUS","ANALYST NOTE","ANALYST ID","REVIEW NOTE","REVIEWER ID")
    names(dout) <- lbls[seq_len(ncol(dout))]
    openxlsx::writeDataTable(wb, "Details", x=dout,
                             startRow=3, tableStyle="TableStyleMedium2")

    if (nrow(dout) > 0) {
      even <- which(seq_len(nrow(dout)) %% 2 == 0) + 3
      if (length(even) > 0)
        openxlsx::addStyle(wb, "Details",
          style=openxlsx::createStyle(fgFill="#EAF1DD"),
          rows=even, cols=seq_len(ncol(dout)),
          gridExpand=TRUE, stack=TRUE)
    }

    openxlsx::saveWorkbook(wb, outpath, overwrite=TRUE)
  }

  # Role-filtered reports
  .rpt <- function(col) {
    if (!col %in% names(head_sum)) return(rep(FALSE, nrow(head_sum)))
    startsWith(toupper(trimws(head_sum[[col]])), "Y")
  }
  .rpt_det <- function(df, col) {
    if (nrow(df)==0 || !col %in% names(df)) return(df)
    df[startsWith(toupper(trimws(df[[col]])), "Y"), ]
  }

  make_report("DM_issues",   head_sum[.rpt("dm_report"),],
              .rpt_det(open_iss,"dm_report"),   "DM")
  make_report("MW_issues",   head_sum[.rpt("mw_report"),],
              .rpt_det(open_iss,"mw_report"),   "MW")
  make_report("SDTM_issues", head_sum[.rpt("sdtm_report"),],
              .rpt_det(open_iss,"sdtm_report"), "SDTM")
  make_report("ADAM_issues", head_sum[.rpt("adam_report"),],
              .rpt_det(open_iss,"adam_report"), "ADAM")
  make_report("all_open",    head_sum, open_iss,   "All Open")
  make_report("all_closed",
              head_sum[head_sum$n_closed > 0, ],
              closed_iss, "All Closed")

  message(">> [reporter] All reports written to: ", cfg$reports)
  invisible(NULL)
}
