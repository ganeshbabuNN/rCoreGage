# ==============================================================================
# test-runner.R  —  Tests for run_checks()
# All temp dirs use tempfile() to guarantee isolation between tests
# ==============================================================================

# ── Helpers ────────────────────────────────────────────────────────────────────
make_state <- function(registry_df=NULL) {
  if (is.null(registry_df)) {
    registry_df <- data.frame(
      id="CHK001", active="YES", rule_set="TESTCHECK",
      sheet="Trial", category="Test", subcategory="Test",
      description="Test", dm_report="YES", mw_report="NO",
      sdtm_report="NO", adam_report="NO", notes="",
      stringsAsFactors=FALSE
    )
  }
  active_rules <- structure(
    startsWith(registry_df$active, "Y"),
    names = registry_df$id
  )
  list(
    rule_registry = registry_df,
    active_rules  = active_rules,
    domains       = list(),
    issues        = data.frame(id=character(0), subj_id=character(0),
                               vis_id=numeric(0), description=character(0),
                               review=character(0), stringsAsFactors=FALSE),
    summary_log   = data.frame(headlink=character(0), nu=integer(0),
                               rule_set=character(0), sobs=character(0),
                               stringsAsFactors=FALSE)
  )
}

# Always use tempfile() — guarantees a unique dir every call
new_dir <- function() {
  d <- tempfile(pattern="rcg_test_")
  dir.create(d, showWarnings=FALSE)
  d
}

write_check <- function(dir, rule_set, n=2, fn=NULL, crash=FALSE) {
  fn   <- if (is.null(fn)) paste0("check_", rule_set) else fn
  path <- file.path(dir, paste0(rule_set, ".R"))
  if (crash) {
    writeLines(paste0(fn," <- function(state,cfg){ stop('deliberate') }"), path)
  } else {
    writeLines(c(
      paste0(fn," <- function(state, cfg) {"),
      paste0("  if (", n," > 0) {"),
      paste0("    df <- data.frame(subj_id=paste0('S',seq_len(",n,")),"),
      "      vis_id=NA_real_,",
      paste0("      description=paste0('Issue ',seq_len(",n,")),"),
      "      stringsAsFactors=FALSE)",
      paste0("    state <- collect_findings(state, df, id='",rule_set,"')"),
      "  }",
      "  state",
      "}"
    ), path)
  }
  path
}

# ==============================================================================
test_that("run_checks: no active rules — message emitted, state unchanged", {
  reg <- data.frame(id="CHK001", active="NO", rule_set="TESTCHECK",
                    sheet="Trial", category="T", subcategory="T",
                    description="T", dm_report="NO", mw_report="NO",
                    sdtm_report="NO", adam_report="NO", notes="",
                    stringsAsFactors=FALSE)
  st  <- make_state(reg)
  cfg <- list(trial_checks=new_dir(), study_checks=new_dir())
  expect_message(st2 <- run_checks(cfg, st), "No active rule sets")
  expect_equal(nrow(st2$issues), 0L)
})

test_that("run_checks: missing check file skipped with warning", {
  st  <- make_state()
  cfg <- list(trial_checks="/nonexistent/dir", study_checks=new_dir())
  expect_message(st2 <- run_checks(cfg, st), "not found")
  expect_equal(nrow(st2$issues), 0L)
})

test_that("run_checks: valid check script executed — findings appended", {
  td  <- new_dir()
  write_check(td, "TESTCHECK", n=3)
  st  <- make_state()
  st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st)
  expect_equal(nrow(st2$issues), 3L)
})

test_that("run_checks: Trial sheet rules sourced from trial_checks dir", {
  td  <- new_dir()
  write_check(td, "TESTCHECK", n=2)
  st  <- make_state()
  st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st)
  expect_equal(nrow(st2$issues), 2L)
})

test_that("run_checks: Study sheet rules sourced from study_checks dir", {
  sd  <- new_dir()
  reg <- data.frame(id="SPRJ001", active="YES", rule_set="STUDYCHK",
                    sheet="Study", category="T", subcategory="T",
                    description="T", dm_report="NO", mw_report="NO",
                    sdtm_report="NO", adam_report="NO", notes="",
                    stringsAsFactors=FALSE)
  write_check(sd, "STUDYCHK", n=1)
  st  <- make_state(reg)
  st2 <- run_checks(list(trial_checks=new_dir(), study_checks=sd), st)
  expect_equal(nrow(st2$issues), 1L)
})

test_that("run_checks: error in check script caught — runner continues", {
  td <- new_dir()
  write_check(td, "TESTCHECK", crash=TRUE)
  st <- make_state()
  expect_message(
    st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st),
    "ERROR in rule set"
  )
  expect_type(st2, "list")
})

test_that("run_checks: wrong function name caught gracefully", {
  td <- new_dir()
  write_check(td, "TESTCHECK", fn="check_WRONGNAME")
  st <- make_state()
  expect_message(
    st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st),
    "ERROR"
  )
  expect_equal(nrow(st2$issues), 0L)
})

test_that("run_checks: multiple rule sets run — both results present", {
  td <- new_dir()
  write_check(td, "ZZZ", n=1)
  write_check(td, "AAA", n=1)
  reg <- data.frame(
    id=c("ZZZ001","AAA001"), active=c("YES","YES"),
    rule_set=c("ZZZ","AAA"), sheet=c("Trial","Trial"),
    category="T", subcategory="T", description="T",
    dm_report="NO", mw_report="NO", sdtm_report="NO",
    adam_report="NO", notes="", stringsAsFactors=FALSE
  )
  st  <- make_state(reg)
  st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st)
  expect_equal(nrow(st2$issues), 2L)
  expect_true(all(c("AAA","ZZZ") %in% st2$issues$id))
})

test_that("run_checks: Active=No rule sets not executed", {
  td  <- new_dir()
  write_check(td, "TESTCHECK", n=5)
  reg <- data.frame(id="CHK001", active="NO", rule_set="TESTCHECK",
                    sheet="Trial", category="T", subcategory="T",
                    description="T", dm_report="NO", mw_report="NO",
                    sdtm_report="NO", adam_report="NO", notes="",
                    stringsAsFactors=FALSE)
  st  <- make_state(reg)
  st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st)
  expect_equal(nrow(st2$issues), 0L)
})

test_that("run_checks: total findings count in final message", {
  td <- new_dir()
  write_check(td, "TESTCHECK", n=7)
  st <- make_state()
  expect_message(
    run_checks(list(trial_checks=td, study_checks=new_dir()), st),
    "Total findings: 7"
  )
})

test_that("run_checks: returns updated state object", {
  td  <- new_dir()
  write_check(td, "TESTCHECK", n=1)
  st  <- make_state()
  st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st)
  expect_type(st2, "list")
  expect_true(all(c("issues","rule_registry","active_rules") %in% names(st2)))
})

test_that("run_checks: domains accessible inside check function", {
  td <- new_dir()
  writeLines(c(
    "check_TESTCHECK <- function(state, cfg) {",
    "  ae <- state$domains$ae",
    "  if (!is.null(ae) && nrow(ae) > 0) {",
    "    df <- data.frame(subj_id=ae$USUBJID, vis_id=NA_real_,",
    "                     description='Found row', stringsAsFactors=FALSE)",
    "    state <- collect_findings(state, df, id='TESTCHECK')",
    "  }",
    "  state",
    "}"
  ), file.path(td, "TESTCHECK.R"))
  st <- make_state()
  st$domains$ae <- data.frame(USUBJID=c("S1","S2"), stringsAsFactors=FALSE)
  st2 <- run_checks(list(trial_checks=td, study_checks=new_dir()), st)
  expect_equal(nrow(st2$issues), 2L)
})
