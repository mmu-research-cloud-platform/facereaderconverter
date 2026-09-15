TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

test_that("conversion metadata supports scalar and callback values", {
  path <- file.path("testdata", "testdata_detailed.txt")

  x <- convertFRFiles(
    path,
    return_data = TRUE,
    id = 12,
    subject = "Rebecca"
  )

  expect_true(all(x$id == 12))
  expect_true(all(x$subject == "Rebecca"))

  callback_data <- loadFRfile(
    path,
    id = function(x) sub(".*testdata_(.*)\\.txt$", "\\1", x),
    subject = function(x) basename(x)
  )
  expect_true(all(callback_data$id == "detailed"))
  expect_true(all(callback_data$subject == basename(path)))
})

test_that("metadata columns are optional and partial values preserve the stem", {
  path <- file.path("testdata", "testdata_detailed.txt")
  x <- convertFRFiles(path, return_data = TRUE)
  expect_false(any(c("id", "subject") %in% names(x)))

  output <- file.path(TEST_DATA, "converted", "metadata_partial")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  md <- convertFRFiles(
    path,
    outpath = file.path(output, "original.txt"),
    id = 12
  )
  writeLines("stale output", md$outpath)
  md <- convertFRFiles(
    path,
    outpath = file.path(output, "original.txt"),
    id = 12
  )
  expect_true(file.exists(file.path(output, "original.csv")))
  expect_true(grepl("original.csv$", md$outpath))
  expect_false("stale output" %in% readLines(md$outpath))
})

test_that("both metadata values determine output name and collisions fail", {
  path <- file.path("testdata", "testdata_detailed.txt")
  output <- file.path(TEST_DATA, "converted", "metadata_complete")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)

  md <- convertFRFiles(
    path,
    outpath = file.path(output, "ignored.txt"),
    id = 12,
    subject = "Rebecca"
  )
  writeLines("stale output", md$outpath)
  md <- convertFRFiles(
    path,
    outpath = file.path(output, "ignored.txt"),
    id = 12,
    subject = "Rebecca"
  )
  expect_true(file.exists(file.path(output, "12_Rebecca_detailed.csv")))
  expect_true(grepl("12_Rebecca_detailed.csv$", md$outpath))
  expect_false("stale output" %in% readLines(md$outpath))

  collision_path <- tempfile(fileext = ".csv")
  on.exit(unlink(collision_path), add = TRUE)
  readr::write_csv(data.frame(id = 1, value = 2), collision_path)
  expect_error(
    loadFRfile(collision_path, id = 12),
    "already exist"
  )
})

test_that("Excel and CSV imports apply metadata", {
  skip_if_not_installed("readxl")
  xlsx_path <- file.path("testdata", "testdata_excel_detailed.xlsx")
  csv_path <- file.path(TEST_DATA, "testdata_detailed.csv")

  xlsx <- loadFRfile(xlsx_path, id = 12, subject = "Rebecca")
  csv <- loadFRfile(csv_path, id = 12, subject = "Rebecca")

  expect_true(all(xlsx$id == 12))
  expect_true(all(xlsx$subject == "Rebecca"))
  expect_true(all(csv$id == 12))
  expect_true(all(csv$subject == "Rebecca"))
})

test_that("directory conversion applies metadata to supported file types", {
  input_dir <- file.path(TEST_DATA, "FR9")
  skip_if(!dir.exists(input_dir), "The FR9 directory fixture is not available")
  output_dir <- file.path(TEST_DATA, "converted", "metadata_directory")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "^8895 mum FR9 Participant 1_00024.*detailed\\.xlsx$",
    id = function(path) 12,
    subject = function(path) "Rebecca",
    cores = 1L
  )
  writeLines("stale output", result$outpath[[1]])
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "^8895 mum FR9 Participant 1_00024.*detailed\\.xlsx$",
    id = function(path) 12,
    subject = function(path) "Rebecca",
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(grepl("12_Rebecca_detailed.csv$", result$outpath)))
  expect_true(all(file.exists(result$outpath)))
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
})

test_that("FR10 directory conversion accepts metadata callbacks", {
  path <- file.path(Sys.getenv("TEST_DATA"), "FR10")
  skip_if(!dir.exists(path), "The FR10 directory fixture is not available")
  files <- list.files(
    path,
    pattern = "_state\\.txt$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(length(files) == 0L, "The FR10 directory contains no state TXT files")

  output <- file.path(TEST_DATA, "converted", "FR10_metadata")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  result <- convertFRDirectory(
    path,
    output,
    pattern = "_state\\.txt$",
    id = function(path) "fr10",
    subject = function(path) "fixture",
    cores = 1L
  )
  writeLines("stale output", result$outpath[[1]])
  result <- convertFRDirectory(
    path,
    output,
    pattern = "_state\\.txt$",
    id = function(path) "fr10",
    subject = function(path) "fixture",
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(grepl("fr10_fixture_state.csv$", result$outpath)))
  expect_true(all(file.exists(result$outpath)))
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
})
