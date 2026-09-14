TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

skip_if_not_installed("readxl")

fr10_dir <- file.path(TEST_DATA, "FR10")

skip_if(
  !dir.exists(fr10_dir),
  "The FR10 directory fixture is not available"
)

fr10_xlsx <- list.files(
  fr10_dir,
  pattern = "\\.xlsx$",
  ignore.case = TRUE,
  full.names = TRUE
)

skip_if(
  length(fr10_xlsx) == 0L,
  "The FR10 directory contains no Excel files"
)

test_that("convertFRExcelFiles converts FR10 detailed exports with extra text columns", {
  detailed <- fr10_xlsx[grepl("detailed", basename(fr10_xlsx), ignore.case = TRUE)]
  skip_if(length(detailed) == 0L, "The FR10 directory contains no detailed Excel files")

  data <- convertFRExcelFiles(
    detailed[[1]],
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  )

  expect_gt(nrow(data), 0L)
  expect_true(all(c(
    "video_time",
    "neutral",
    "happy",
    "sad",
    "angry",
    "surprised",
    "scared",
    "disgusted"
  ) %in% names(data)))
  expect_true(any(c("stimulus", "event_marker", "speech_rate") %in% names(data)))
  expect_s3_class(data$video_time, "hms")
})

test_that("convertFRExcelFiles converts the FR10 state export", {
  state <- fr10_xlsx[grepl("state", basename(fr10_xlsx), ignore.case = TRUE)]
  skip_if(length(state) == 0L, "The FR10 directory contains no state Excel files")

  data <- convertFRExcelFiles(
    state[[1]],
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  )

  expect_gt(nrow(data), 0L)
  expect_setequal(names(data), c("video_time", "dominant_expression"))
})
