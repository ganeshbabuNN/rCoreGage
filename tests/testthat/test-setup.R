# ==============================================================================
# test-setup.R  —  Tests for setup_coregage()
# 20 scenarios (including scenario 18 which was previously missing)
# ==============================================================================

# ── Helper: unique temp xlsx using tempfile() ─────────────────────────────────
make_registry <- function(trial_rows=NULL, study_rows=NULL) {
  path <- tempfile(fileext=".xlsx")
  cols <- c("Category","Subcategory","ID","Active",
            "DM_Report","MW_Report","SDTM_Report","ADAM_Report",
            "Rule_Set","Description","Notes")
  wb <- openxlsx::createWorkbook()
  if (!is.null(trial_rows)) {
    df <- as.data.frame(matrix(trial_rows, ncol=length(cols), byrow=TRUE,
                               dimnames=list(NULL, cols)), stringsAsFactors=FALSE)
    openxlsx::addWorksheet(wb, "Trial")
    openxlsx::writeData(wb, "Trial", df)
  }
  if (!is.null(study_rows)) {
    df <- as.data.frame(matrix(study_rows, ncol=length(cols), byrow=TRUE,
                               dimnames=list(NULL, cols)), stringsAsFactors=FALSE)
    openxlsx::addWorksheet(wb, "Study")
    openxlsx::writeData(wb, "Study", df)
  }
  openxlsx::saveWorkbook(wb, path, overwrite=TRUE)
  path
}

make_cfg <- function(reg_path) {
  list(rule_registry=reg_path, reports=tempdir(), feedback=tempdir())
}

valid_row <- function(id="AECHK001", active="Yes", rule_set="AE") {
  c("AE","Date",id,active,"Yes","No","Yes","No",rule_set,"Check desc","")
}

# ==============================================================================
test_that("setup_coregage: missing registry file stops with error", {
  cfg <- make_cfg("/nonexistent/path/reg.xlsx")
  expect_error(setup_coregage(cfg), "not found")
})

test_that("setup_coregage: Trial-only registry returns state", {
  path <- make_registry(trial_rows=valid_row())
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_type(st, "list")
  expect_true("rule_registry" %in% names(st))
})

test_that("setup_coregage: Study-only registry returns state", {
  path <- make_registry(study_rows=valid_row("AEPRJ001","Yes","AE_study"))
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$rule_registry), 1L)
  expect_equal(st$rule_registry$sheet[1], "Study")
})

test_that("setup_coregage: Trial + Study both loaded and combined", {
  path <- make_registry(
    trial_rows = valid_row("AECHK001","Yes","AE"),
    study_rows = valid_row("AEPRJ001","Yes","AE_study")
  )
  st <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$rule_registry), 2L)
  expect_true("Trial" %in% st$rule_registry$sheet)
  expect_true("Study" %in% st$rule_registry$sheet)
})

test_that("setup_coregage: Active=Yes row is TRUE in active_rules", {
  path <- make_registry(trial_rows=valid_row("AECHK001","Yes","AE"))
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_true(isTRUE(st$active_rules["AECHK001"]))
})

test_that("setup_coregage: Active=No row is FALSE in active_rules", {
  path <- make_registry(trial_rows=valid_row("AECHK001","No","AE"))
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_false(isTRUE(st$active_rules["AECHK001"]))
})

test_that("setup_coregage: active_rules names match registry IDs", {
  path <- make_registry(
    trial_rows=c(valid_row("AECHK001","Yes","AE"),
                 valid_row("LBCHK001","No","LB"))
  )
  st <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_true("AECHK001" %in% names(st$active_rules))
  expect_true("LBCHK001" %in% names(st$active_rules))
})

test_that("setup_coregage: IDs are uppercased", {
  path <- make_registry(trial_rows=valid_row("aechk001","Yes","AE"))
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_true("AECHK001" %in% names(st$active_rules))
})

test_that("setup_coregage: blank ID rows filtered out", {
  rows <- c(valid_row("AECHK001","Yes","AE"),
            c("AE","Date","","Yes","Yes","No","Yes","No","AE","Blank",""))
  path <- make_registry(trial_rows=rows)
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$rule_registry), 1L)
})

test_that("setup_coregage: YOURID placeholder rows filtered out", {
  rows <- c(valid_row("AECHK001","Yes","AE"),
            c("AE","Date","YOURID","Yes","Yes","No","Yes","No","AE","Tmpl",""))
  path <- make_registry(trial_rows=rows)
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$rule_registry), 1L)
})

test_that("setup_coregage: state$issues is empty with correct columns", {
  path <- make_registry(trial_rows=valid_row())
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$issues), 0L)
  expect_true(all(c("id","subj_id","vis_id","description","review")
                  %in% names(st$issues)))
})

test_that("setup_coregage: state$summary_log is empty with correct columns", {
  path <- make_registry(trial_rows=valid_row())
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$summary_log), 0L)
  expect_true(all(c("headlink","nu","rule_set","sobs")
                  %in% names(st$summary_log)))
})

test_that("setup_coregage: state$review_log is empty with correct columns", {
  path <- make_registry(trial_rows=valid_row())
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$review_log), 0L)
  expect_true(all(c("id","subj_id","vis_id","status","analyst_note",
                    "review_note") %in% names(st$review_log)))
})

test_that("setup_coregage: session contains sdate and stime", {
  path <- make_registry(trial_rows=valid_row())
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_true("sdate" %in% names(st$session))
  expect_true("stime" %in% names(st$session))
  expect_type(st$session$sdate, "character")
  expect_type(st$session$stime, "character")
})

test_that("setup_coregage: rule_registry has all required columns", {
  path <- make_registry(trial_rows=valid_row())
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  req  <- c("id","active","rule_set","dm_report","mw_report",
            "sdtm_report","adam_report","sheet")
  expect_true(all(req %in% names(st$rule_registry)))
})

test_that("setup_coregage: rule_registry sorted by rule_set then id", {
  rows <- c(valid_row("LBCHK001","Yes","LB"),
            valid_row("AECHK001","Yes","AE"),
            valid_row("AECHK002","Yes","AE"))
  path <- make_registry(trial_rows=rows)
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(st$rule_registry$rule_set[1], "AE")
  ae_rows <- st$rule_registry[st$rule_registry$rule_set=="AE",]
  expect_equal(ae_rows$id[1], "AECHK001")
})

test_that("setup_coregage: sheet with only blank IDs stops with error", {
  blank_row <- c("AE","Date","","Yes","Yes","No","Yes","No","AE","","")
  path <- make_registry(trial_rows=blank_row)
  expect_error(suppressMessages(setup_coregage(make_cfg(path))),
               "No valid check definitions")
})

test_that("setup_coregage: extra unknown columns in registry handled gracefully", {
  # Write registry with an extra unknown column not in the required set
  path <- tempfile(fileext=".xlsx")
  cols_extra <- c("Category","Subcategory","ID","Active",
                  "DM_Report","MW_Report","SDTM_Report","ADAM_Report",
                  "Rule_Set","Description","Notes","EXTRA_COL")
  df <- data.frame(matrix(
    c("AE","Date","AECHK001","Yes","Yes","No","Yes","No","AE","Check","","extra_val"),
    ncol=12, dimnames=list(NULL, cols_extra)), stringsAsFactors=FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Trial")
  openxlsx::writeData(wb, "Trial", df)
  openxlsx::saveWorkbook(wb, path, overwrite=TRUE)
  # Should not crash — extra columns silently ignored
  expect_no_error(suppressMessages(setup_coregage(make_cfg(path))))
  st <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_equal(nrow(st$rule_registry), 1L)
})

test_that("setup_coregage: mixed Active case values all handled", {
  rows <- c(valid_row("CHK001","YES","AE"),
            valid_row("CHK002","yes","LB"),
            valid_row("CHK003","No","CM"))
  path <- make_registry(trial_rows=rows)
  st   <- suppressMessages(setup_coregage(make_cfg(path)))
  expect_true(isTRUE(st$active_rules["CHK001"]))
  expect_true(isTRUE(st$active_rules["CHK002"]))
  expect_false(isTRUE(st$active_rules["CHK003"]))
})

test_that("setup_coregage: session$sdate is today in ddMMMYYYY format", {
  path     <- make_registry(trial_rows=valid_row())
  st       <- suppressMessages(setup_coregage(make_cfg(path)))
  expected <- format(Sys.Date(), "%d%b%Y")
  expect_equal(st$session$sdate, expected)
})
