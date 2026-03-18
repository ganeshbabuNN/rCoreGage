# ==============================================================================
# modules/numobs.R
# Description : Counts valid observations in a findings data frame.
#               Optionally excludes unblinding-sensitive records when
#               the subject ID is negative AND the topic code is absent
#               from the description column.
# ==============================================================================

numobs <- function(df, unblind_topic_codes = character(0)) {

  if (nrow(df) == 0) return(0L)

  if (length(unblind_topic_codes) > 0) {
    df <- df[!(grepl("^-", df$subj_id) &
               !sapply(df$description, function(desc) {
                 any(grepl(unblind_topic_codes, desc, fixed = TRUE))
               })), ]
  }

  nrow(df)
}
