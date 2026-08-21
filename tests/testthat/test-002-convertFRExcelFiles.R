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

test_that("convertFRExcelFiles matches detailed control output", {
  res <- read_excel_control("testdata_excel_detailed")
  expect_equal(res$excel[names(res$control)], res$control, tolerance = 1e-4)
})

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

test_that("convertFRExcelFiles respects participant-aware duplicates", {
  header_meta <- data.frame(
    V1 = c(
      "Video analysis detailed log",
      "",
      "Face Model",
      "Calibration",
      "Start time",
      "Filename",
      "Frame rate",
      "",
      "",
      ""
    ),
    V2 = c(
      "",
      "",
      "General",
      "-",
      "6/4/2026 13:31:06.331",
      "C:/tmp/example.mp4",
      "30.000000000",
      "",
      "",
      ""
    ),
    check.names = FALSE
  )
  header_row <- data.frame(
    `Video Time` = "Video Time",
    Neutral = "Neutral",
    Happy = "Happy",
    Sad = "Sad",
    Angry = "Angry",
    Surprised = "Surprised",
    Scared = "Scared",
    Disgusted = "Disgusted",
    `Participant Name` = "Participant Name",
    `Analysis Index` = "Analysis Index",
    check.names = FALSE
  )
  ok_data <- data.frame(
    `Video Time` = c("00:00:00.000", "00:00:00.033"),
    Neutral = c("0", "0"),
    Happy = c("0", "0"),
    Sad = c("0", "0"),
    Angry = c("0", "0"),
    Surprised = c("0", "0"),
    Scared = c("0", "0"),
    Disgusted = c("0", "0"),
    `Participant Name` = c("Rebecca", "Alex"),
    `Analysis Index` = c("Analysis 5", "Analysis 6"),
    check.names = FALSE
  )
  bad_data <- ok_data
  bad_data$`Video Time`[2] <- "00:00:00.000"
  bad_data$`Participant Name`[2] <- "Rebecca"
  bad_data$`Analysis Index`[2] <- "Analysis 5"

  make_workbook <- function(data, path) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Sheet1")
    openxlsx::writeData(
      wb,
      "Sheet1",
      header_meta,
      startRow = 1,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb,
      "Sheet1",
      header_row,
      startRow = 11,
      colNames = FALSE
    )
    openxlsx::writeData(wb, "Sheet1", data, startRow = 12, colNames = FALSE)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    path
  }

  expect_no_error(
    convertFRExcelFiles(
      make_workbook(ok_data, tempfile(fileext = ".xlsx")),
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE
    )
  )
  expect_error(
    convertFRExcelFiles(
      make_workbook(bad_data, tempfile(fileext = ".xlsx")),
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE,
      duplicate_timecodes_as_error = TRUE
    ),
    "Duplicate timecodes"
  )
})

test_that("convertFRExcelFiles silences name repair messages", {
  expect_silent(
    convertFRExcelFiles(
      file.path("testdata", "testdata_excel_detailed.xlsx"),
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE
    )
  )
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
