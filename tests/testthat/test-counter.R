test_that("count_valid returns correct row count", {
  df <- data.frame(subj_id=c("001","002","003"),
                   description=c("A","B","C"), stringsAsFactors=FALSE)
  expect_equal(count_valid(df), 3L)
})

test_that("count_valid returns 0 for empty data frame", {
  df <- data.frame(subj_id=character(0), description=character(0),
                   stringsAsFactors=FALSE)
  expect_equal(count_valid(df), 0L)
})

test_that("count_valid excludes unblinding rows correctly", {
  df <- data.frame(
    subj_id     = c("001", "-002", "-003"),
    description = c("Normal issue", "Topic: HBA1C issue", "Unrelated issue"),
    stringsAsFactors = FALSE
  )
  # -003 has no HBA1C in description and negative ID -> excluded
  expect_equal(count_valid(df, unblind_codes = "HBA1C"), 2L)
})
