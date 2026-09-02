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

test_that("convertFRExcelFiles preserves extra detailed columns", {
  x <- convertFRExcelFiles(
    file.path("testdata", "testdata_extracols_detailed.xlsx"),
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  )

  expect_true(all(
    c(
      "video_time",
      "neutral",
      "happy",
      "sad",
      "angry",
      "surprised",
      "scared",
      "disgusted",
      "gender",
      "age",
      "glasses",
      "participant_name",
      "analysis_index"
    ) %in%
      names(x)
  ))
  expect_equal(class(x$age), "numeric")
  expect_true(all(x$participant_name == "Rebecca"))
})
