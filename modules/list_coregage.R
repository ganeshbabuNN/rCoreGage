# ==============================================================================
# modules/list_coregage.R
# Description : Consolidation and reporting module for CoreGage.
#   Part 1 - Import existing open / closed issue Excel files (if present)
#   Part 2 - Merge new findings with old; assign / update status
#   Part 3 - Incorporate reviewer feedback from Feedback folder
#   Part 4 - Summarise open / closed counts per check
#   Part 5 - Write final Excel reports
# ==============================================================================

list_coregage <- function(config, ctx) {

  message(">> [list_coregage] Starting consolidation ...")

  sdate <- ctx$run_meta$sdate
  stime <- ctx$run_meta$stime

  cleanup_text <- function(x) {
    x <- gsub("[\x01-\x09\x0b\x0e-\x1f]", "", x)
    x <- gsub("[\x0a\x0c\x0d]", " ", x)
    trimws(gsub("\\s+", " ", x))
  }

  empty_olddata <- function() {
    data.frame(id=character(0), subj_id=character(0), vis_id=numeric(0),
               desrp=character(0), dup_id=character(0),
               find_dt=as.Date(character(0)), status=character(0),
               prog_comment=character(0), prog_initials=character(0),
               stringsAsFactors=FALSE)
  }

  import_issues <- function(type) {
    path <- file.path(config$outputlib, paste0(type, "_issues.xlsx"))
    if (!file.exists(path)) return(NULL)
    message(">> [list_coregage] Importing existing ", type, " issues ...")
    df <- tryCatch(
      readxl::read_xlsx(path, sheet="Details", range="A3:J500000", col_types="text"),
      error = function(e) { message("  WARNING: could not read ", path); NULL }
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)

    col_map <- c("CHECK ID"="id","SUBJECT ID"="subj_id","VISIT ID"="vis_id",
                 "DESCRIPTION"="desrp","FIND DATE"="find_dt","STATUS"="status",
                 "PROG COMMENT"="prog_comment","PROG INITIALS"="prog_initials")
    for (old in names(col_map)) {
      if (old %in% names(df)) names(df)[names(df)==old] <- col_map[[old]]
    }
    needed <- c("id","subj_id","vis_id","desrp","find_dt","status","prog_comment","prog_initials")
    for (col in needed) if (!col %in% names(df)) df[[col]] <- NA_character_

    df <- df[!is.na(df$find_dt) &
               toupper(trimws(df$prog_comment)) != "THIS IS EMPTY", ]
    if (nrow(df) == 0) return(NULL)

    df$vis_id    <- suppressWarnings(as.numeric(df$vis_id))
    df$find_dt   <- suppressWarnings(as.Date(as.numeric(df$find_dt), origin="1899-12-30"))
    df$desrp         <- cleanup_text(df$desrp)
    df$prog_comment  <- cleanup_text(df$prog_comment)
    df$dup_id        <- gsub("\\s","",df$desrp)
    df[, c("id","subj_id","vis_id","desrp","dup_id","find_dt","status","prog_comment","prog_initials")]
  }

  # ===========================================================================
  # PART 1 - Import old open / closed issues
  # ===========================================================================
  open_olddata   <- import_issues("open")
  closed_olddata <- import_issues("closed")

  if (!is.null(open_olddata) || !is.null(closed_olddata)) {
    olddata <- dplyr::bind_rows(
      if (!is.null(open_olddata))   open_olddata   else empty_olddata(),
      if (!is.null(closed_olddata)) closed_olddata else empty_olddata()
    )
  } else {
    olddata <- empty_olddata()
  }

  has_new <- nrow(ctx$findings) > 0
  has_old <- nrow(olddata) > 0

  if (!has_new && !has_old) {
    message(">> [list_coregage] No findings to report. Writing empty reports.")
    issuelist3 <- data.frame(
      id=character(0), subj_id=character(0), vis_id=numeric(0),
      desrp=character(0), dup_id=character(0), find_dt=as.Date(character(0)),
      status=character(0), prog_comment=character(0), prog_initials=character(0),
      reviewer_comment=character(0), reviewer_initials=character(0),
      category=character(0), subcategory=character(0), sheet=character(0),
      headtxt=character(0), tvp=character(0), dm=character(0),
      stringsAsFactors=FALSE)
  } else {

    # =========================================================================
    # PART 2 - Merge new with old; set status
    # =========================================================================
    if (has_new) {
      newfindings <- ctx$findings
      names(newfindings)[names(newfindings)=="description"] <- "desrp"
      newfindings <- unique(newfindings[, c("id","subj_id","vis_id","desrp")])
      newfindings$dup_id <- gsub("\\s","",newfindings$desrp)
    } else {
      newfindings <- data.frame(id=character(0),subj_id=character(0),
                                vis_id=numeric(0),desrp=character(0),
                                dup_id=character(0),stringsAsFactors=FALSE)
    }

    if (nrow(newfindings) > 0) newfindings$.new <- TRUE
    if (nrow(olddata)     > 0) olddata$.old     <- TRUE

    if (nrow(newfindings) > 0 && nrow(olddata) > 0) {
      merged <- merge(newfindings, olddata,
                      by=c("id","subj_id","vis_id","dup_id"),
                      all.x=TRUE, all.y=TRUE)
      merged$.new <- ifelse(is.na(merged$.new), FALSE, merged$.new)
      merged$.old <- ifelse(is.na(merged$.old), FALSE, merged$.old)
    } else if (nrow(newfindings) > 0) {
      merged <- newfindings
      merged$.old           <- FALSE
      merged$find_dt        <- as.Date(NA)
      merged$status         <- NA_character_
      merged$prog_comment   <- NA_character_
      merged$prog_initials  <- NA_character_
      merged$desrp.x        <- merged$desrp
      merged$desrp.y        <- NA_character_
    } else {
      merged <- olddata
      merged$.new    <- FALSE
      merged$desrp.x <- NA_character_
      merged$desrp.y <- merged$desrp
    }

    if ("desrp.x" %in% names(merged) && "desrp.y" %in% names(merged)) {
      merged$desrp   <- ifelse(!is.na(merged$desrp.x), merged$desrp.x, merged$desrp.y)
      merged$desrp.x <- NULL
      merged$desrp.y <- NULL
    }

    for (col in c("status","prog_comment","prog_initials")) {
      if (!col %in% names(merged)) merged[[col]] <- NA_character_
    }
    if (!"find_dt" %in% names(merged)) merged$find_dt <- as.Date(NA)

    assign_status <- function(is_new, is_old, st, cmt, init) {
      st_l  <- tolower(trimws(ifelse(is.na(st),"",st)))
      cmt_l <- tolower(trimws(ifelse(is.na(cmt),"",cmt)))
      hi    <- nchar(trimws(ifelse(is.na(init),"",init))) > 0
      by_prog <- grepl("closed by programmer", cmt_l)
      if  (is_new && !is_old)                                      return("Open")
      if  (is_new &&  is_old && st_l != "closed")                  return(st)
      if  (is_new &&  is_old && st_l=="closed" &&  by_prog &&  hi) return("Closed")
      if  (is_new &&  is_old && st_l=="closed" &&  by_prog && !hi) return("Open")
      if  (is_new &&  is_old && st_l=="closed" && !by_prog)        return("Open")
      if (!is_new &&  is_old)                                       return("Closed")
      return(ifelse(is.na(st),"Open",st))
    }

    if (nrow(merged) > 0) {
      merged$status <- mapply(assign_status,
        merged$.new, merged$.old,
        ifelse(is.na(merged$status),"",merged$status),
        ifelse(is.na(merged$prog_comment),"",merged$prog_comment),
        ifelse(is.na(merged$prog_initials),"",merged$prog_initials),
        SIMPLIFY=TRUE)

      update_comment <- function(is_new, is_old, st, cmt) {
        ac_tag <- paste0("[Not reflected in data anymore (", sdate, ").]")
        ro_tag <- paste0("[Finding was closed but is still reflected in data (", sdate, ").]")
        c_out  <- trimws(ifelse(is.na(cmt),"",cmt))
        st_l   <- tolower(trimws(st))
        if (!is_new && is_old && !grepl(ac_tag, c_out, fixed=TRUE))
          return(paste(c_out, ac_tag))
        if (is_new && is_old && st_l=="open" &&
            !grepl("closed by programmer", tolower(c_out)) &&
            !grepl(ro_tag, c_out, fixed=TRUE))
          return(paste(c_out, ro_tag))
        return(c_out)
      }

      merged$prog_comment <- mapply(update_comment,
        merged$.new, merged$.old, merged$status,
        ifelse(is.na(merged$prog_comment),"",merged$prog_comment),
        SIMPLIFY=TRUE)

      merged$find_dt <- as.Date(
        ifelse(is.na(merged$find_dt), as.numeric(Sys.Date()), as.numeric(merged$find_dt)),
        origin="1970-01-01")
    }

    merged$.new <- NULL ; merged$.old <- NULL
    issuelist0 <- unique(merged)

    mc_sub     <- ctx$master_checks[, c("id","category","subcategory","sheet","headtxt","tvp","dm")]
    issuelist2 <- merge(mc_sub, issuelist0, by="id", all.y=TRUE)

    # =========================================================================
    # PART 3 - Incorporate reviewer feedback from Feedback folder
    # =========================================================================
    feedback_dir   <- file.path(config$outputlib, "Feedback")
    feedback_files <- character(0)
    if (dir.exists(feedback_dir)) {
      all_f <- list.files(feedback_dir, full.names=TRUE)
      feedback_files <- all_f[grepl("(TVP|DM|Review).*\\.xlsx$",
                                    basename(all_f), ignore.case=TRUE)]
    }

    if (length(feedback_files) > 0) {
      message(">> [list_coregage] Importing reviewer comments from Feedback folder ...")
      rev_list <- lapply(feedback_files, function(fpath) {
        tryCatch({
          df <- readxl::read_xlsx(fpath, sheet="Details",
                                  range="A3:J500000", col_types="text")
          col_map2 <- c("CHECK ID"="id","SUBJECT ID"="subj_id","VISIT ID"="vis_id",
                        "DESCRIPTION"="desrp","FIND DATE"="find_dt","STATUS"="status",
                        "PROG COMMENT"="prog_comment",
                        "REVIEWER COMMENT"="reviewer_comment",
                        "REVIEWER INITIALS"="reviewer_initials")
          for (o in names(col_map2)) if (o %in% names(df)) names(df)[names(df)==o] <- col_map2[[o]]
          df <- df[!is.na(df$find_dt) &
                     toupper(trimws(df$prog_comment)) != "THIS IS EMPTY", ]
          if (nrow(df)==0) return(NULL)
          df$vis_id  <- suppressWarnings(as.numeric(df$vis_id))
          df$find_dt <- suppressWarnings(as.Date(as.numeric(df$find_dt), origin="1899-12-30"))
          df$last_mod <- file.mtime(fpath)
          df$dup_id   <- gsub("\\s","",cleanup_text(df$desrp))
          df$desrp    <- cleanup_text(df$desrp)
          if ("reviewer_comment" %in% names(df))
            df$reviewer_comment <- cleanup_text(df$reviewer_comment)
          df
        }, error=function(e) { message("  WARNING: could not import: ", fpath); NULL })
      })

      rev_all <- do.call(dplyr::bind_rows, Filter(Negate(is.null), rev_list))

      if (!is.null(rev_all) && nrow(rev_all) > 0) {
        rev_all <- rev_all[order(rev_all$id, rev_all$subj_id, rev_all$vis_id,
                                 rev_all$dup_id, rev_all$last_mod, decreasing=TRUE), ]
        holder  <- rev_all[!duplicated(rev_all[, c("id","subj_id","vis_id","dup_id")]), ]
        keep    <- intersect(c("id","subj_id","vis_id","dup_id","reviewer_comment",
                               "reviewer_initials","status"), names(holder))
        holder  <- holder[, keep]
        names(holder)[names(holder)=="status"] <- "reviewer_status"

        issuelist3 <- merge(issuelist2, holder,
                            by=c("id","subj_id","vis_id","dup_id"), all.x=TRUE)
        upd <- !is.na(issuelist3$reviewer_status) &
               tolower(trimws(issuelist3$status)) != "closed"
        issuelist3$status[upd] <- issuelist3$reviewer_status[upd]
        issuelist3$reviewer_status <- NULL
      } else {
        issuelist3 <- issuelist2
        issuelist3$reviewer_comment  <- ""
        issuelist3$reviewer_initials <- ""
      }
    } else {
      issuelist3 <- issuelist2
      issuelist3$reviewer_comment  <- ""
      issuelist3$reviewer_initials <- ""
    }

  } # end has findings block

  # ===========================================================================
  # PART 4 - Summarise open / closed counts
  # ===========================================================================
  open_issues   <- issuelist3[tolower(trimws(issuelist3$status)) != "closed", ]
  closed_issues <- issuelist3[tolower(trimws(issuelist3$status)) == "closed", ]

  count_by_id <- function(df, col_name) {
    if (nrow(df)==0) {
      out <- data.frame(id=character(0), x=integer(0), stringsAsFactors=FALSE)
      names(out)[2] <- col_name ; return(out)
    }
    tbl <- as.data.frame(table(df$id), stringsAsFactors=FALSE)
    names(tbl) <- c("id", col_name) ; tbl
  }

  n_open   <- count_by_id(open_issues,   "n_open")
  n_closed <- count_by_id(closed_issues, "n_closed")

  mc_head  <- ctx$master_checks[, c("id","headtxt","category","tvp","dm")]
  head_hl  <- ctx$headlines[, c("headlink","nu")]
  names(head_hl)[names(head_hl)=="headlink"] <- "id"

  head_summary <- merge(mc_head,      head_hl,  by="id", all.x=TRUE)
  head_summary <- merge(head_summary, n_open,   by="id", all.x=TRUE)
  head_summary <- merge(head_summary, n_closed, by="id", all.x=TRUE)

  head_summary$nu       <- ifelse(is.na(head_summary$nu),       0L, head_summary$nu)
  head_summary$n_open   <- ifelse(is.na(head_summary$n_open),   0L, head_summary$n_open)
  head_summary$n_closed <- ifelse(is.na(head_summary$n_closed), 0L, head_summary$n_closed)

  # ===========================================================================
  # PART 5 - Write Excel reports
  # ===========================================================================
  if (!dir.exists(config$outputlib)) dir.create(config$outputlib, recursive=TRUE)
  run_label <- paste("Last run:", sdate, stime)

  make_output <- function(out_name, summary_df, detail_df, title) {
    outpath <- file.path(config$outputlib, paste0(out_name, ".xlsx"))
    message(">> [list_coregage] Writing: ", outpath)

    if (nrow(summary_df)==0) {
      summary_df <- data.frame(headtxt="No issues found", id="N/A",
                               nu=0L, n_open=0L, n_closed=0L, stringsAsFactors=FALSE)
    }
    if (nrow(detail_df)==0) {
      detail_df <- data.frame(id="N/A", subj_id="N/A", vis_id=0,
        desrp="No issues found", find_dt=Sys.Date(), status="N/A",
        prog_comment="This is empty", prog_initials="",
        reviewer_comment="", reviewer_initials="", stringsAsFactors=FALSE)
    }
    for (col in c("reviewer_comment","reviewer_initials")) {
      if (!col %in% names(detail_df)) detail_df[[col]] <- ""
    }

    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(wb, "List")
    openxlsx::writeData(wb, "List", x=paste("Data Check Report -", title), startRow=1)
    openxlsx::writeData(wb, "List", x=run_label, startRow=2)
    list_out <- summary_df[, intersect(c("headtxt","id","nu","n_open","n_closed"), names(summary_df))]
    names(list_out) <- c("Check Description","Check ID","New","Open","Closed")[seq_len(ncol(list_out))]
    openxlsx::writeDataTable(wb, "List", x=list_out, startRow=4, tableStyle="TableStyleMedium9")

    summary_df$n_open   <- ifelse(is.na(summary_df$n_open),   0L, summary_df$n_open)
    summary_df$n_closed <- ifelse(is.na(summary_df$n_closed), 0L, summary_df$n_closed)
    for (r in seq_len(nrow(summary_df))) {
      openxlsx::addStyle(wb, "List",
        style=openxlsx::createStyle(fgFill=if (isTRUE(summary_df$n_open[r]==0)) "#C6EFCE" else "#FFEB9C"),
        rows=r+4, cols=4, stack=TRUE)
      openxlsx::addStyle(wb, "List",
        style=openxlsx::createStyle(fgFill=if (isTRUE(summary_df$n_closed[r]==0)) "#FFC7CE" else "#FFEB9C"),
        rows=r+4, cols=5, stack=TRUE)
    }

    openxlsx::addWorksheet(wb, "Details")
    openxlsx::writeData(wb, "Details", x=paste("All", title, "Issue List"), startRow=1)
    openxlsx::writeData(wb, "Details", x=run_label, startRow=2)

    keep <- c("id","subj_id","vis_id","desrp","find_dt","status",
              "prog_comment","prog_initials","reviewer_comment","reviewer_initials")
    dout <- detail_df[, intersect(keep, names(detail_df))]
    lbls <- c("CHECK ID","SUBJECT ID","VISIT ID","DESCRIPTION","FIND DATE","STATUS",
              "PROG COMMENT","PROG INITIALS","REVIEWER COMMENT","REVIEWER INITIALS")
    names(dout) <- lbls[seq_len(ncol(dout))]
    openxlsx::writeDataTable(wb, "Details", x=dout, startRow=3, tableStyle="TableStyleMedium2")

    if (nrow(dout) > 0) {
      even <- which(seq_len(nrow(dout)) %% 2 == 0) + 3
      if (length(even) > 0)
        openxlsx::addStyle(wb, "Details",
          style=openxlsx::createStyle(fgFill="#EAF1DD"),
          rows=even, cols=seq_len(ncol(dout)), gridExpand=TRUE, stack=TRUE)
    }
    openxlsx::saveWorkbook(wb, outpath, overwrite=TRUE)
  }

  tvp_idx <- grepl("VALIDATION PLAN", toupper(head_summary$category)) |
             startsWith(toupper(trimws(head_summary$tvp)), "Y")
  oi_tvp  <- if (nrow(open_issues)>0)
    open_issues[grepl("VALIDATION PLAN",toupper(open_issues$category)) |
                startsWith(toupper(trimws(open_issues$tvp)),"Y"), ]
  else open_issues
  make_output("TVP_open_issues", head_summary[tvp_idx, ], oi_tvp, "TVP")

  dm_idx <- startsWith(toupper(trimws(head_summary$dm)), "Y")
  oi_dm  <- if (nrow(open_issues)>0)
    open_issues[startsWith(toupper(trimws(open_issues$dm)),"Y"), ]
  else open_issues
  make_output("DM_open_issues", head_summary[dm_idx, ], oi_dm, "DM")

  make_output("open_issues",   head_summary, open_issues, "Open")
  make_output("closed_issues",
              head_summary[head_summary$n_closed > 0, ],
              closed_issues, "Closed")

  message(">> [list_coregage] All reports written to: ", config$outputlib)
  invisible(NULL)
}
