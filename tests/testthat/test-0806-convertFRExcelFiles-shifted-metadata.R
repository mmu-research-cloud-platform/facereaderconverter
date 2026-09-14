TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("readxl")
skip_if_not_installed("openxlsx")

read_excel_control <- function(file_stub) {
  xlsx_path <- file.path("testdata", paste0(file_stub, ".xlsx"))
  txt_path <- file.path("testdata", paste0(file_stub, "_control.txt"))
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

test_that("convertFRExcelFiles handles shifted metadata lines", {
  excel_line_change <- convertFRExcelFiles(
    file.path("testdata", "testdata_excel_detailed_line_change.xlsx"),
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  )
  excel_original <- convertFRExcelFiles(
    file.path("testdata", "testdata_excel_detailed.xlsx"),
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  )
  expect_equal(excel_line_change, excel_original, tolerance = 1e-4)
})

test_that("convertFRExcelFiles handles FR10 metadata layouts", {
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
  expect_true("video_time" %in% names(data))
})
