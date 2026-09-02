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

test_that("convertFRExcelFiles handles missing metadata", {
  detailed <- suppressWarnings(convertFRExcelFiles(
    file.path("testdata", "testdata_extracols_nometadata_detailed.xlsx"),
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  ))
  state <- suppressWarnings(convertFRExcelFiles(
    file.path("testdata", "testdata_extracols_nometadata_state.xlsx"),
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  ))

  expect_true(ncol(detailed) == 15)
  expect_true(ncol(state) == 4)
  expect_true(all(c("participant_name", "analysis_index") %in% names(detailed)))
})
