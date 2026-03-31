test_that("collect_findings appends to state$issues", {
  state <- list(
    issues      = data.frame(id=character(0), subj_id=character(0),
                             vis_id=numeric(0), description=character(0),
                             review=character(0), stringsAsFactors=FALSE),
    summary_log = data.frame(headlink=character(0), nu=integer(0),
                             rule_set=character(0), sobs=character(0),
                             stringsAsFactors=FALSE)
  )
  df <- data.frame(subj_id="SUBJ-001", vis_id=200,
                   description="Test issue", stringsAsFactors=FALSE)
  state2 <- collect_findings(state, df, id="TESTCHK001")
  expect_equal(nrow(state2$issues), 1L)
  expect_equal(state2$issues$id[1], "TESTCHK001")
})

test_that("collect_findings skips NULL input", {
  state <- list(
    issues      = data.frame(id=character(0), subj_id=character(0),
                             vis_id=numeric(0), description=character(0),
                             review=character(0), stringsAsFactors=FALSE),
    summary_log = data.frame(headlink=character(0), nu=integer(0),
                             rule_set=character(0), sobs=character(0),
                             stringsAsFactors=FALSE)
  )
  state2 <- collect_findings(state, NULL, id="TESTCHK001")
  expect_equal(nrow(state2$issues), 0L)
})
