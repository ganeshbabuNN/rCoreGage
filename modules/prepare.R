# ==============================================================================
# modules/prepare.R
# Description : Findings preparation helper for CoreGage.
#   - Validates the findings data frame for a single check
#   - Counts valid observations via numobs()
#   - Appends findings to the master findings table
#   - Appends a headline row to the master headlines table
# ==============================================================================

prepare <- function(ctx, df, id, desc_col = "description",
                    sobs = TRUE, unblind_topic_codes = character(0)) {

  if (is.null(df) || !is.data.frame(df)) {
    message("WARNING [prepare]: dataset for ", id, " is NULL or not a data frame. Skipping.")
    return(ctx)
  }
  if (!"subj_id" %in% names(df)) {
    message("WARNING [prepare]: subj_id column missing for ", id, ". Skipping.")
    return(ctx)
  }
  if (!desc_col %in% names(df)) {
    message("WARNING [prepare]: description column '", desc_col, "' missing for ", id, ". Skipping.")
    return(ctx)
  }

  missing_desc <- df[is.na(df[[desc_col]]) | trimws(df[[desc_col]]) == "", ]
  if (nrow(missing_desc) > 0) {
    message("WARNING [prepare]: ", nrow(missing_desc), " rows have missing descriptions for ",
            id, ". Skipping.")
    return(ctx)
  }

  n <- numobs(df, unblind_topic_codes)
  message(">> [prepare] Appending ", n, " finding(s) for check: ", id)

  findings_new <- df
  names(findings_new)[names(findings_new) == desc_col] <- "description"
  findings_new <- findings_new[, intersect(c("subj_id","vis_id","description"),
                                           names(findings_new))]
  findings_new$id          <- id
  findings_new$review      <- "PROGRAMMER"
  findings_new$subj_id     <- as.character(findings_new$subj_id)
  findings_new$vis_id      <- if ("vis_id" %in% names(findings_new))
    as.numeric(findings_new$vis_id) else NA_real_
  findings_new$description <- substr(as.character(findings_new$description), 1, 200)

  findings_new <- unique(findings_new[, c("id","subj_id","vis_id","description","review")])

  ctx$findings <- unique(rbind(ctx$findings, findings_new))

  headline_new <- data.frame(headlink=id, nu=as.integer(n), cmv=id,
                             sobs=if (sobs) "Y" else "N",
                             status=NA_character_, info=NA_character_,
                             stringsAsFactors=FALSE)
  ctx$headlines <- rbind(ctx$headlines, headline_new)

  ctx
}
