#' Load domain input data files
#'
#' Reads every \code{.csv} and (optionally) \code{.sas7bdat} file from the
#' \code{inputs/} folder into a named list. The name of each element is the
#' lowercase filename without extension (e.g. \code{AE.csv} becomes
#' \code{domains$ae}). Drop a new domain file into \code{inputs/} and it is
#' picked up automatically on the next run - no code change required.
#'
#' @param cfg A named list of paths from \code{project_config.R}.
#'   Must contain \code{cfg$inputs}.
#'
#' @return A named list of data frames, one per domain file found.
#'
#' @export
#' @examples
#' \donttest{
#' # Create a temporary inputs folder with a sample CSV
#' tmp_inp <- tempdir()
#' write.csv(
#'   data.frame(USUBJID = "SUBJ-001", AETERM = "RASH"),
#'   file.path(tmp_inp, "AE.csv"), row.names = FALSE
#' )
#' cfg     <- list(inputs = tmp_inp)
#' domains <- load_inputs(cfg)
#' # domains$ae contains the loaded Adverse Events data
#' }
load_inputs <- function(cfg) {

  data_dir <- cfg$inputs
  if (!dir.exists(data_dir)) {
    warning("inputs/ folder not found at: ", data_dir)
    return(list())
  }

  domains <- list()

  csv_files <- list.files(data_dir, pattern = "\\.csv$",
                          full.names = TRUE, ignore.case = TRUE)
  for (f in csv_files) {
    nm <- tolower(tools::file_path_sans_ext(basename(f)))
    domains[[nm]] <- utils::read.csv(f, stringsAsFactors = FALSE,
                              na.strings  = c("", "NA"),
                              check.names = FALSE)
    message("   ", basename(f), " -> domains$", nm,
            "  (", nrow(domains[[nm]]), " rows)")
  }

  sas_files <- list.files(data_dir, pattern = "\\.sas7bdat$",
                          full.names = TRUE, ignore.case = TRUE)
  if (length(sas_files) > 0) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      warning(".sas7bdat files found but 'haven' is not installed. ",
              "Run: install.packages('haven')")
    } else {
      for (f in sas_files) {
        nm <- tolower(tools::file_path_sans_ext(basename(f)))
        domains[[nm]] <- haven::read_sas(f)
        message("   ", basename(f), " -> domains$", nm,
                "  (", nrow(domains[[nm]]), " rows)")
      }
    }
  }

  if (length(domains) == 0) {
    warning("No data files found in: ", data_dir,
            ". Drop .csv or .sas7bdat files into inputs/ and re-run.")
  }

  domains
}

# Internal helper: clean special characters from text columns
.cleanup_text <- function(x) {
  x <- gsub("[\x01-\x09\x0b\x0e-\x1f]", "", x)
  x <- gsub("[\x0a\x0c\x0d]", " ", x)
  trimws(gsub("\\s+", " ", x))
}
