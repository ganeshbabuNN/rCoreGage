# ==============================================================================
# master_run.R
# Description : Main driver for CoreGage data quality check framework.
#               Loads domain data, runs all active checks, writes reports.
#
# HOW TO ADD A NEW DOMAIN:
#   1. Drop DOMAIN.csv (or .sas7bdat) into sample_data/
#   2. Drop DOMAIN.R   into copy_to_trial/program/    (trial checks)
#   3. Drop DOMAIN_PRJ.R into check_programs/project/ (project checks)
#   4. Add rows to master_checks.xlsx with Batch_ID = DOMAIN / DOMAIN_PRJ
#   exec_coregage picks them up automatically. Nothing else to change.
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(openxlsx)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(fs)
})

# -- Source all framework modules ----------------------------------------------
source("modules/init_coregage.R")
source("modules/exec_coregage.R")
source("modules/list_coregage.R")
source("modules/prepare.R")
source("modules/numobs.R")

# -- Path configuration --------------------------------------------------------
# getwd() resolves correctly when the project is opened via coregage.Rproj
base <- getwd()

config <- list(
  masterlib = file.path(base, "copy_to_trial/document"),
  outputlib = file.path(base, "copy_to_trial/output"),
  tchecklib = file.path(base, "copy_to_trial/program"),
  pchecklib = file.path(base, "check_programs/project")
)

# -- Domain data loader --------------------------------------------------------
# Reads every .csv and .sas7bdat from sample_data/ into a named list.
# Name = lowercase filename without extension  e.g. AE.csv -> data_list$ae
# To add a new domain just drop the file in sample_data/ - no code change.
load_domain_data <- function(data_dir = file.path(base, "sample_data")) {

  if (!dir.exists(data_dir)) {
    message("WARNING: sample_data/ not found at ", data_dir)
    return(list())
  }

  data_list <- list()

  csv_files <- list.files(data_dir, pattern = "\\.csv$",
                          full.names = TRUE, ignore.case = TRUE)
  for (f in csv_files) {
    nm <- tolower(tools::file_path_sans_ext(basename(f)))
    data_list[[nm]] <- read.csv(f, stringsAsFactors = FALSE,
                                na.strings = c("", "NA"), check.names = FALSE)
    message("   ", basename(f), " -> data_list$", nm,
            "  (", nrow(data_list[[nm]]), " rows)")
  }

  sas7bdat_files <- list.files(data_dir, pattern = "\\.sas7bdat$",
                               full.names = TRUE, ignore.case = TRUE)
  if (length(sas7bdat_files) > 0) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      message("WARNING: .sas7bdat files found but 'haven' package not installed.")
      message("         Run:  install.packages('haven')")
    } else {
      for (f in sas7bdat_files) {
        nm <- tolower(tools::file_path_sans_ext(basename(f)))
        data_list[[nm]] <- haven::read_sas(f)
        message("   ", basename(f), " -> data_list$", nm,
                "  (", nrow(data_list[[nm]]), " rows)")
      }
    }
  }

  data_list
}

# -- Main run function ---------------------------------------------------------
master_run <- function(config) {

  message("========== CoreGage : Starting Run ==========")

  # Phase 1 - initialise registry and tables
  ctx <- init_coregage(config)

  # Phase 2 - load domain data
  message(">> Loading domain data from sample_data/ ...")
  ctx$data_list <- load_domain_data()
  message("   Domains: ", paste(toupper(names(ctx$data_list)), collapse = ", "))

  # Phase 3 - execute all active check programs
  ctx <- exec_coregage(config, ctx)

  # Phase 4 - consolidate findings and write Excel reports
  list_coregage(config, ctx)

  message("========== CoreGage : Run Complete ==========")
  message(">> Reports: ", config$outputlib)
}

master_run(config)
