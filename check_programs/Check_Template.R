# ==============================================================================
# check_programs/Check_Template.R
# Description : Template for writing domain checks in CoreGage.
#               Copy this file, rename the function to run_<Batch_ID>,
#               and implement your check logic.
#
# Convention  : Every check program must expose exactly one function:
#               run_<Batch_ID>(ctx, config) -> ctx
#               The function receives the shared context and returns it updated.
# ==============================================================================

run_BATCHID <- function(ctx, config) {   # <-- rename to run_<your_batch_id>

  data_list <- ctx$data_list  # named list of domain data frames
  switches  <- ctx$switches   # named logical vector: TRUE = check is ON

  # ===========================================================================
  # Check: CHECKID   <-- replace with your check ID from master_checks.xlsx
  # ===========================================================================
  if (isTRUE(switches["CHECKID"])) {

    # Step 1 - Read / filter the input data
    df <- data_list$your_domain_dataset |>
      dplyr::filter(...)

    # Step 2 - Perform the check logic

    # Step 3 - Build result data frame.
    #   Required columns: subj_id (character), vis_id (numeric),
    #                     description (character, max 200 chars)
    CHECKID <- df |>
      dplyr::mutate(
        subj_id     = USUBJID,
        vis_id      = NA_real_,
        description = paste0("Meaningful description of the issue")
      ) |>
      dplyr::select(subj_id, vis_id, description)

    # Step 4 - Append findings to master tables
    ctx <- prepare(ctx, CHECKID, id = "CHECKID")
  }

  # Add more checks below following the same pattern ...

  ctx   # always return the updated context
}
