# rCoreGage - Data Quality Check Framework

rCoreGage is a language-agnostic data quality check framework implemented in R.
It provides a structured, configuration-driven approach to running domain-level
data checks and consolidating findings into Excel issue reports.

## Folder structure

```
rCoreGage/
|   master_run.R              <- main driver
|   README.md
|
+-- modules/
|   +-- init_rCoreGage.R       <- initialisation: import check registry, setup tables
|   +-- exec_rCoreGage.R       <- execution: loop through active checks and run them
|   +-- list_rCoreGage.R       <- consolidation: merge findings and write reports
|   +-- prepare.R             <- helper: append findings to master tables
|   +-- numobs.R              <- helper: count valid observations
|
+-- copy_to_trial/
|   +-- document/
|   |   +-- master_checks.xlsx   <- check registry (Project + Trial sheets)
|   +-- program/
|       +-- AE.R              <- Adverse Events trial checks
|       +-- EC.R              <- Exposure trial checks
|       +-- LB.R              <- Laboratory trial checks
|
+-- check_programs/
|   +-- Check_Template.R      <- blank scaffold for new checks
|   +-- project/
|       +-- AE_PRJ.R          <- Adverse Events project checks
|       +-- EC_PRJ.R          <- Exposure project checks
|       +-- LB_PRJ.R          <- Laboratory project checks
|
+-- sample_data/
    +-- AE.csv                <- sample adverse events data (73 rows)
    +-- EC.csv                <- sample exposure data (80 rows)
    +-- LB.csv                <- sample laboratory data (1003 rows)
```

## Concept mapping

| Original concept     | rCoreGage equivalent              |
|----------------------|----------------------------------|
| Macro / %macro       | Module function (modules/)       |
| Global state object  | ctx (context list)               |
| Data input list      | data_list (ctx$data_list)        |
| Config variables     | config (named list)              |
| SDTM programmer note | prog_comment / prog_initials     |
| DM reviewer note     | reviewer_comment / reviewer_initials |
| Closed by programmer | "closed by programmer" string    |

## Required R packages

```r
install.packages(c("readxl","dplyr","openxlsx","stringr",
                   "lubridate","purrr","fs","haven"))
```

## How to run

1. Open rCoreGage.Rproj in RStudio
2. Update paths in master_run.R if needed (getwd() auto-resolves for .Rproj)
3. Run: source("master_run.R")

## Writing a new check

Copy Check_Template.R, rename the function to run_<Batch_ID>, and implement
your logic. The result data frame must have three columns:
- subj_id     (character)
- vis_id      (numeric)
- description (character, max 200 chars)

Call prepare(ctx, your_df, id = "CHECKID") at the end of each check block.
Add a row to master_checks.xlsx with Batch_ID matching your filename.

## Status logic

| Situation                                     | Status assigned   |
|-----------------------------------------------|-------------------|
| New finding (not seen before)                 | Open              |
| Existing finding, not closed                  | Kept as-is        |
| No longer in data                             | Auto-Closed       |
| Closed by programmer (with initials)          | Closed            |
| Closed by programmer but no initials          | Re-opened         |
| Previously closed, now reappearing            | Re-opened         |
| Reviewer has updated status                   | Reviewer status   |
