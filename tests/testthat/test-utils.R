# ==============================================================================
# test-utils.R  —  Tests for load_inputs() and .cleanup_text()
# All temp dirs use tempfile() to guarantee isolation
# ==============================================================================

make_inputs_cfg <- function(dir) list(inputs=dir)

write_csv_input <- function(dir, name, df) {
  utils::write.csv(df, file.path(dir, name), row.names=FALSE, na="")
}

new_dir <- function() {
  d <- tempfile(pattern="rcg_inp_")
  dir.create(d, showWarnings=FALSE)
  d
}

# ==============================================================================
# load_inputs
# ==============================================================================

test_that("load_inputs: non-existent dir returns empty list with warning", {
  cfg <- make_inputs_cfg("/nonexistent/path/inputs")
  expect_warning(result <- load_inputs(cfg), "not found")
  expect_equal(length(result), 0L)
})

test_that("load_inputs: empty inputs dir returns empty list with warning", {
  cfg <- make_inputs_cfg(new_dir())
  expect_warning(result <- load_inputs(cfg), "No data files found")
  expect_equal(length(result), 0L)
})

test_that("load_inputs: single CSV loaded with correct lowercase name", {
  td <- new_dir()
  write_csv_input(td, "AE.csv",
                  data.frame(USUBJID="S1", AETERM="Rash", stringsAsFactors=FALSE))
  result <- load_inputs(make_inputs_cfg(td))
  expect_true("ae" %in% names(result))
})

test_that("load_inputs: multiple CSVs loaded as separate elements", {
  td <- new_dir()
  write_csv_input(td, "AE.csv", data.frame(X=1))
  write_csv_input(td, "LB.csv", data.frame(Y=2))
  write_csv_input(td, "CM.csv", data.frame(Z=3))
  result <- load_inputs(make_inputs_cfg(td))
  expect_equal(sort(names(result)), c("ae","cm","lb"))
})

test_that("load_inputs: AE.csv name converted to lowercase key 'ae'", {
  td <- new_dir()
  write_csv_input(td, "AE.csv",
                  data.frame(USUBJID="S1", stringsAsFactors=FALSE))
  result <- load_inputs(make_inputs_cfg(td))
  expect_true("ae" %in% names(result))
  expect_false("AE" %in% names(result))
})

test_that("load_inputs: uppercase CSV extension handled (AE.CSV)", {
  td <- new_dir()
  write_csv_input(td, "AE.CSV",
                  data.frame(USUBJID="S1", stringsAsFactors=FALSE))
  result <- load_inputs(make_inputs_cfg(td))
  expect_equal(length(result), 1L)
})

test_that("load_inputs: loaded data frame has correct row count", {
  td <- new_dir()
  df <- data.frame(USUBJID=paste0("S", 1:10), stringsAsFactors=FALSE)
  write_csv_input(td, "AE.csv", df)
  result <- load_inputs(make_inputs_cfg(td))
  expect_equal(nrow(result$ae), 10L)
})

test_that("load_inputs: loaded data frame preserves column names", {
  td <- new_dir()
  df <- data.frame(USUBJID="S1", AETERM="Rash", AESTDTC="2022-01-01",
                   stringsAsFactors=FALSE)
  write_csv_input(td, "AE.csv", df)
  result <- load_inputs(make_inputs_cfg(td))
  expect_true(all(c("USUBJID","AETERM","AESTDTC") %in% names(result$ae)))
})

test_that("load_inputs: empty CSV loads as 0-row data frame", {
  td <- new_dir()
  df <- data.frame(USUBJID=character(0), stringsAsFactors=FALSE)
  write_csv_input(td, "AE.csv", df)
  result <- load_inputs(make_inputs_cfg(td))
  expect_equal(nrow(result$ae), 0L)
})

test_that("load_inputs: empty string in CSV becomes NA", {
  td <- new_dir()
  df <- data.frame(USUBJID="S1", AETERM="", stringsAsFactors=FALSE)
  write_csv_input(td, "AE.csv", df)
  result <- load_inputs(make_inputs_cfg(td))
  expect_true(is.na(result$ae$AETERM[1]))
})

test_that("load_inputs: 'NA' string in CSV becomes NA", {
  td <- new_dir()
  writeLines("USUBJID,AETERM\nS1,NA", file.path(td, "AE.csv"))
  result <- load_inputs(make_inputs_cfg(td))
  expect_true(is.na(result$ae$AETERM[1]))
})

test_that("load_inputs: sas7bdat handled gracefully when haven absent", {
  skip_if(requireNamespace("haven", quietly=TRUE),
          "haven is installed — this test only applies when haven is absent")
  td <- new_dir()
  writeLines("dummy", file.path(td, "AE.sas7bdat"))
  expect_warning(load_inputs(make_inputs_cfg(td)))
})

# ==============================================================================
# .cleanup_text
# ==============================================================================

test_that(".cleanup_text: removes control characters (ASCII 1-9)", {
  x      <- paste0("Hello", rawToChar(as.raw(c(0x01,0x07))), "World")
  result <- rCoreGage:::.cleanup_text(x)
  expect_equal(result, "HelloWorld")
})

test_that(".cleanup_text: replaces newline with space", {
  result <- rCoreGage:::.cleanup_text("line1\nline2")
  expect_equal(result, "line1 line2")
})

test_that(".cleanup_text: collapses multiple spaces to one", {
  result <- rCoreGage:::.cleanup_text("too   many    spaces")
  expect_equal(result, "too many spaces")
})

test_that(".cleanup_text: trims leading and trailing whitespace", {
  result <- rCoreGage:::.cleanup_text("  padded  ")
  expect_equal(result, "padded")
})

test_that(".cleanup_text: normal text unchanged", {
  result <- rCoreGage:::.cleanup_text("Normal text 123")
  expect_equal(result, "Normal text 123")
})

test_that(".cleanup_text: empty string returns empty string", {
  result <- rCoreGage:::.cleanup_text("")
  expect_equal(result, "")
})
