#' Create a new CoreGage project
#'
#' Scaffolds a complete CoreGage project folder structure at the specified
#' path and copies all required template files from the package installation.
#' After running this function, open the generated \code{.Rproj} file in
#' RStudio and start working.
#'
#' @param name Character. Project name. Used as the folder name and
#'   \code{.Rproj} file name.
#' @param path Character. Parent directory where the project folder will
#'   be created. Defaults to the current working directory.
#' @param overwrite Logical. Whether to overwrite an existing project at
#'   the same path. Defaults to \code{FALSE}.
#'
#' @return Invisibly returns the full path to the created project.
#'
#' @export
#' @examples
#' \donttest{
#' proj_path <- file.path(tempdir(), "example_project")
#' dir.create(proj_path, showWarnings = FALSE)
#' create_project(name = "TRIAL_ABC", path = proj_path)
#' }
create_project <- function(name, path, overwrite = FALSE) {

  project_root <- file.path(path, name)

  if (dir.exists(project_root) && !overwrite) {
    stop("Project folder already exists: ", project_root,
         "\nSet overwrite = TRUE to overwrite.")
  }

  if (missing(path) || !nzchar(path)) {
    stop("'path' must be supplied. ",
         "Use path = tempdir() for testing or specify your project directory.")
  }

  message(">> Creating CoreGage project '", name, "' at: ", project_root)

  #  Create folder structure 
  dirs <- c(
    "rules/config",
    "rules/trial",
    "rules/study",
    "inputs",
    "outputs/reports",
    "outputs/feedback/DM",
    "outputs/feedback/MW",
    "outputs/feedback/SDTM",
    "outputs/feedback/ADAM"
  )
  for (d in dirs) {
    dir.create(file.path(project_root, d), recursive = TRUE,
               showWarnings = FALSE)
  }
  message("  Created folder structure")

  #  Copy engine templates from inst/templates/ 
  tmpl_dir <- system.file("templates", package = "rCoreGage")
  if (!nzchar(tmpl_dir)) {
    warning("Package templates not found. Install rCoreGage properly.")
  } else {
    # run_coregage.R - fill in the project root placeholder
    run_src <- file.path(tmpl_dir, "run_coregage.R")
    if (file.exists(run_src)) {
      run_lines <- readLines(run_src)
      run_lines <- gsub("__PROJECT_ROOT__",
                        gsub("\\\\", "/", project_root),
                        run_lines)
      run_lines <- gsub("__PROJECT_NAME__", name, run_lines)
      writeLines(run_lines, file.path(project_root, "run_coregage.R"))
      message("  Copied run_coregage.R")
    }

    # project_config.R - fill in paths
    cfg_src <- file.path(tmpl_dir, "project_config.R")
    if (file.exists(cfg_src)) {
      cfg_lines <- readLines(cfg_src)
      cfg_lines <- gsub("__PROJECT_ROOT__",
                        gsub("\\\\", "/", project_root),
                        cfg_lines)
      cfg_lines <- gsub("__PROJECT_NAME__", name, cfg_lines)
      writeLines(cfg_lines, file.path(project_root,
                                      "rules/config/project_config.R"))
      message("  Copied project_config.R")
    }

    # Check_Template.R
    chk_src <- file.path(tmpl_dir, "Check_Template.R")
    if (file.exists(chk_src)) {
      file.copy(chk_src,
                file.path(project_root, "rules/trial/Check_Template.R"),
                overwrite = TRUE)
      message("  Copied Check_Template.R")
    }
  }

  #  Copy rule_registry.xlsx from inst/extdata/ 
  reg_src <- system.file("extdata", "rule_registry.xlsx",
                         package = "rCoreGage")
  if (!nzchar(reg_src)) {
    warning("rule_registry.xlsx template not found in package.")
  } else {
    file.copy(reg_src,
              file.path(project_root, "rules/config/rule_registry.xlsx"),
              overwrite = TRUE)
    message("  Copied rule_registry.xlsx")
  }

  #  Create .Rproj file 
  rproj_content <- c(
    "Version: 1.0", "",
    "RestoreWorkspace: No",
    "SaveWorkspace: No",
    "AlwaysSaveHistory: No", "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8", "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX"
  )
  writeLines(rproj_content,
             file.path(project_root, paste0(name, ".Rproj")))
  message("  Created ", name, ".Rproj")

  #  Create .gitignore 
  gitignore <- c(
    "# CoreGage project gitignore",
    "inputs/",
    "outputs/",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    "*.Rproj.user/"
  )
  writeLines(gitignore, file.path(project_root, ".gitignore"))

  message("")
  message("  CoreGage project '", name, "' created successfully.")
  message("")
  message("  Next steps:")
  message("  1. Open ", name, ".Rproj in RStudio")
  message("  2. Fill in rules/config/rule_registry.xlsx with check definitions")
  message("  3. Write check scripts in rules/trial/ and rules/study/")
  message("  4. Drop domain data files (.csv or .sas7bdat) into inputs/")
  message("  5. Run: source('run_coregage.R')")

  invisible(project_root)
}
