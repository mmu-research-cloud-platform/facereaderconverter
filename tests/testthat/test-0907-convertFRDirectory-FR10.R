TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

fr10_dir <- file.path(TEST_DATA, "FR10")

skip_if(
  !dir.exists(fr10_dir),
  "The FR10 directory fixture is not available"
)

fr10_files <- list.files(
  fr10_dir,
  pattern = "\\.(txt|xlsx)$",
  ignore.case = TRUE,
  full.names = TRUE
)

skip_if(
  length(fr10_files) == 0L,
  "The FR10 directory contains no supported files"
)

test_that("convertFRDirectory converts FR10 TXT and Excel exports", {
  skip_if_not_installed("readxl")

  output_dir <- tempfile("fr10_converted_")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  result <- convertFRDirectory(
    fr10_dir,
    output_dir,
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_length(result$outpath, length(fr10_files))
  expect_true(all(file.exists(result$outpath)))
  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
  expect_setequal(
    basename(result$outpath),
    paste0(tools::file_path_sans_ext(basename(fr10_files)), ".csv")
  )
})

test_that("convertFRDirectory preserves FR10 detailed text columns", {
  detailed <- fr10_files[grepl("detailed", basename(fr10_files), ignore.case = TRUE)]
  skip_if(length(detailed) == 0L, "The FR10 directory contains no detailed files")

  output_dir <- tempfile("fr10_detailed_")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  result <- convertFRDirectory(
    fr10_dir,
    output_dir,
    pattern = "detailed",
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  converted <- readr::read_csv(result$outpath[[1]], show_col_types = FALSE)
  expect_true(any(c("stimulus", "event_marker", "speech_rate") %in% names(converted)))
  expect_gt(nrow(converted), 0L)
})
