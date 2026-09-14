TEST_DATA <- Sys.getenv("TEST_DATA")

test_that("convertFRDirectory converts all supported file types", {
  skip_if_not_installed("openxlsx")

  test_data <- Sys.getenv("TEST_DATA")
  input_dir <- file.path(test_data, "FR9")
  output_dir <- file.path(test_data, "FR9_converted")
  skip_if(
    !dir.exists(input_dir),
    "The FR9 directory fixture is not available"
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(
    list.files(output_dir, full.names = TRUE),
    recursive = TRUE,
    force = TRUE
  )
  input_files <- list.files(
    input_dir,
    pattern = "\\.(txt|xlsx|csv)$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  expect_true(length(input_files) > 0)
  expect_true(any(tolower(tools::file_ext(input_files)) == "xlsx"))

  expect_no_error(convertFRDirectory(
    input_dir,
    output_dir,
    cores = 1L
  ))

  expected_outputs <- c(
    paste0(tools::file_path_sans_ext(basename(input_files)), ".csv"),
    "metadata.csv"
  )
  expect_setequal(list.files(output_dir), expected_outputs)
  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
  expect_equal(
    nrow(read.csv(file.path(output_dir, "metadata.csv"))),
    length(input_files)
  )
})

test_that("convertFRDirectory converts FR10 supported file types", {
  skip_if_not_installed("readxl")
  path <- file.path(Sys.getenv("TEST_DATA"), "FR10")
  skip_if(!dir.exists(path), "The FR10 directory fixture is not available")
  files <- list.files(
    path,
    pattern = "\\.(txt|xlsx|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(length(files) == 0L, "The FR10 directory contains no supported files")

  output <- tempfile("fr10_types_")
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)
  result <- convertFRDirectory(path, output, cores = 1L)

  expect_true(all(result$status == "Success"))
  expect_true(all(file.exists(result$outpath)))
})
