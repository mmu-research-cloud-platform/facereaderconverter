TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("readxl")
skip_if_not_installed("openxlsx")

read_excel_control <- function(file_stub) {
  xlsx_path <- testthat::test_path("testdata", paste0(file_stub, ".xlsx"))
  txt_path <- testthat::test_path("testdata", paste0(file_stub, "_control.txt"))
  list(
    excel = convertFRExcelFiles(
      xlsx_path,
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE
    ),
    control = convertFRFiles(
      txt_path,
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE
    )
  )
}

test_that("convertFRExcelFiles matches detailed control output", {
  res <- read_excel_control("testdata_excel_detailed")
  expect_equal(res$excel[names(res$control)], res$control, tolerance = 1e-4)
})

test_that("convertFRExcelFiles converts an FR10 detailed export", {
  skip_if_not_installed("readxl")
  path <- file.path(Sys.getenv("TEST_DATA"), "FR10")
  skip_if(!dir.exists(path), "The FR10 directory fixture is not available")
  files <- list.files(
    path,
    pattern = "detailed\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(files) == 0L,
    "The FR10 directory contains no detailed Excel files"
  )

  data <- convertFRExcelFiles(files[[1]], return_data = TRUE)
  expect_gt(nrow(data), 0L)
  expect_true(all(c("video_time", "neutral", "event_marker") %in% names(data)))
})
