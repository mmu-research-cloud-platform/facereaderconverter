TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

test_data_path <- file.path(TEST_DATA, "test_data.RDa")
test_data_error <- tryCatch(
  {
    load(test_data_path)
    NULL
  },
  error = identity
)
if (inherits(test_data_error, "error")) {
  testthat::skip(sprintf(
    "Could not load test data fixtures: %s",
    test_data_error$message
  ))
}

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

test_that("convertFRExcelFiles converts FR10 metadata-bearing exports", {
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
