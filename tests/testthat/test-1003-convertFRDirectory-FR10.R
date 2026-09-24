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

  output_dir <- file.path(TEST_DATA, "converted", "FR10")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  result <- convertFRDirectory(
    fr10_dir,
    output_dir,
    cores = 1L
  )
  writeLines("stale output", result$outpath[[1]])
  age_files(result$outpath[[1]])
  test_started <- Sys.time()
  result <- convertFRDirectory(
    fr10_dir,
    output_dir,
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_length(result$outpath, length(fr10_files))
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
  expect_files_modified_since(
    file.path(output_dir, "metadata.csv"),
    test_started
  )
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
  expect_setequal(
    basename(result$outpath),
    paste0(tools::file_path_sans_ext(basename(fr10_files)), ".csv")
  )
})

test_that("convertFRDirectory preserves FR10 detailed text columns", {
  detailed <- fr10_files[grepl(
    "detailed",
    basename(fr10_files),
    ignore.case = TRUE
  )]
  skip_if(
    length(detailed) == 0L,
    "The FR10 directory contains no detailed files"
  )

  output_dir <- file.path(TEST_DATA, "converted", "FR10_detailed")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  result <- convertFRDirectory(
    fr10_dir,
    output_dir,
    pattern = "detailed",
    cores = 1L
  )
  writeLines("stale output", result$outpath[[1]])
  age_files(c(result$outpath, file.path(output_dir, "metadata.csv")))
  test_started <- Sys.time()
  result <- convertFRDirectory(
    fr10_dir,
    output_dir,
    pattern = "detailed",
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  converted <- readr::read_csv(result$outpath[[1]], show_col_types = FALSE)
  expect_files_modified_since(result$outpath, test_started)
  expect_files_modified_since(
    file.path(output_dir, "metadata.csv"),
    test_started
  )
  expect_true(any(
    c("stimulus", "event_marker", "speech_rate") %in% names(converted)
  ))
  expect_gt(nrow(converted), 0L)
})
