# ==============================================================================
# rules/config/project_config.R  -  TRIAL_ABC path configuration
# Edit the project_root below if you move this project to a different machine.
# ==============================================================================

project_root <- getwd()   # works when opened via TRIAL_ABC.Rproj

cfg <- list(
  project_name  = "TRIAL_ABC",
  rule_registry = file.path(project_root, "rules/config/rule_registry.xlsx"),
  trial_checks  = file.path(project_root, "rules/trial"),
  study_checks  = file.path(project_root, "rules/study"),
  inputs        = file.path(project_root, "inputs"),
  reports       = file.path(project_root, "outputs/reports"),
  feedback      = file.path(project_root, "outputs/feedback")
)
