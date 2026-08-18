TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("readxl")

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

test_that("convertFRExcelFiles matches detailed control output", {
  res <- read_excel_control("testdata_excel_detailed")
  expect_equal(res$excel, res$control, tolerance = 1e-4)
})

test_that("convertFRExcelFiles matches state control output", {
  res <- read_excel_control("testdata_excel_state")
  expect_identical(res$excel, res$control)
})

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
