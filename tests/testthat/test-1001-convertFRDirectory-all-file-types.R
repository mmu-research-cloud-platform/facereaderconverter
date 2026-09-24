TEST_DATA <- Sys.getenv("TEST_DATA")

test_that("convertFRDirectory converts all supported file types", {
  skip_if_not_installed("openxlsx")

  test_data <- Sys.getenv("TEST_DATA")
  input_dir <- file.path(test_data, "FR9")
  output_dir <- file.path(test_data, "converted", "FR9")
  skip_if(
    !dir.exists(input_dir),
    "The FR9 directory fixture is not available"
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
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
  output_paths <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(input_files)), ".csv")
  )
  writeLines("stale output", output_paths[[1]])
  age_files(c(output_paths, file.path(output_dir, "metadata.csv")))
  test_started <- Sys.time()
  expect_no_error(convertFRDirectory(
    input_dir,
    output_dir,
    cores = 1L
  ))
  expect_files_modified_since(output_paths, test_started)
  expect_files_modified_since(
    file.path(output_dir, "metadata.csv"),
    test_started
  )
  expect_false("stale output" %in% readLines(output_paths[[1]]))

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

  output <- file.path(TEST_DATA, "converted", "FR10_all_file_types")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  result <- convertFRDirectory(path, output, cores = 1L)
  writeLines("stale output", result$outpath[[1]])
  age_files(c(result$outpath, file.path(output, "metadata.csv")))
  test_started <- Sys.time()
  result <- convertFRDirectory(path, output, cores = 1L)

  expect_true(all(result$status == "Success"))
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
  expect_files_modified_since(file.path(output, "metadata.csv"), test_started)
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
})
