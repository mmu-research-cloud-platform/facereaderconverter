TEST_DATA <- Sys.getenv("TEST_DATA")

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

test_that("explicit metadata overrides inferred filename metadata", {
  path <- file.path("testdata", "testdata_detailed.txt")
  data <- loadFRfile(path, id = "manual-id")

  expect_true(all(data$id == "manual-id"))
  expect_true(all(data$subject == "testdata_detailed"))
})

test_that("FaceReader files infer media ID and export subject by default", {
  path <- file.path("testdata", "testdata_detailed.txt")
  x <- convertFRFiles(path, return_data = TRUE)
  expect_true(all(x$id == "randomparent"))
  expect_true(all(x$subject == "testdata_detailed"))

  csv_path <- file.path(TEST_DATA, "testdata_detailed.csv")
  csv <- loadFRfile(csv_path)
  expect_false("id" %in% names(csv))
  expect_true(all(csv$subject == "testdata_detailed"))

  output <- tempfile("metadata_partial_")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)
  md <- convertFRFiles(
    path,
    outpath = file.path(output, "original.txt"),
    id = 12
  )
  writeLines("stale output", md$outpath)
  age_files(md$outpath)
  test_started <- Sys.time()
  md <- convertFRFiles(
    path,
    outpath = file.path(output, "original.txt"),
    id = 12
  )
  expect_true(file.exists(file.path(
    output,
    "12_testdata_detailed_original_detailed.csv"
  )))
  expect_files_modified_since(md$outpath, test_started)
  expect_true(grepl("12_testdata_detailed_original_detailed.csv$", md$outpath))
  expect_false("stale output" %in% readLines(md$outpath))
})

test_that("TXT ingestion can infer metadata from full paths", {
  input_dir <- tempfile("full_path_txt_")
  dir.create(file.path(input_dir, "full_path_subject"), recursive = TRUE)
  on.exit(unlink(input_dir, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(input_dir, "full_path_subject", "source.txt")
  file.copy(testthat::test_path("testdata", "testdata_detailed.txt"), path)

  data <- convertFRFiles(
    path,
    return_data = TRUE,
    id_pattern = "1234",
    subject_pattern = "full_path_subject[/\\\\]source",
    use_full_path = TRUE
  )

  expect_true(all(data$id == "1234"))
  expect_true(all(grepl("full_path_subject[/\\\\]source", data$subject)))
})

test_that("XLSX ingestion can infer metadata from full paths", {
  path <- file.path(
    TEST_DATA,
    "brazil/Participant 10_Participant 10_Analysis 1_video_20260918_143140_detailed.xlsx"
  )

  data <- loadFRfile(
    path,
    id_pattern = "8883",
    subject_pattern = "brazil",
    use_full_path = TRUE
  )

  expect_true(all(data$id == "8883"))
  expect_true(all(data$subject == "brazil"))
})

test_that("Excel conversion can infer metadata from full paths", {
  path <- file.path(
    TEST_DATA,
    "brazil/Participant 10_Participant 10_Analysis 1_video_20260918_143140_detailed.xlsx"
  )

  data <- convertFRExcelFiles(
    path,
    id_pattern = "8883",
    subject_pattern = "brazil",
    use_full_path = TRUE
  )

  expect_true(all(data$id == "8883"))
  expect_true(all(data$subject == "brazil"))
})

test_that("CSV ingestion can infer subject from the full path only", {
  input_dir <- tempfile("brazil_csv_")
  dir.create(file.path(input_dir, "brazil"), recursive = TRUE)
  on.exit(unlink(input_dir, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(input_dir, "brazil", "testdata_detailed.csv")
  file.copy(file.path(TEST_DATA, "testdata_detailed.csv"), path)

  data <- loadFRfile(
    path,
    id_pattern = "8883",
    subject_pattern = "brazil",
    use_full_path = TRUE
  )

  expect_false("id" %in% names(data))
  expect_true(all(data$subject == "brazil"))
})

test_that("both metadata values determine output name and collisions fail", {
  path <- file.path("testdata", "testdata_detailed.txt")
  output <- tempfile("metadata_complete_")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)

  test_started <- Sys.time()
  md <- convertFRFiles(
    path,
    outpath = file.path(output, "ignored.txt"),
    id = 12,
    subject = "Rebecca"
  )
  writeLines("stale output", md$outpath)
  age_files(md$outpath)
  test_started <- Sys.time()
  md <- convertFRFiles(
    path,
    outpath = file.path(output, "ignored.txt"),
    id = 12,
    subject = "Rebecca"
  )
  expect_true(file.exists(file.path(output, "12_Rebecca_ignored_detailed.csv")))
  expect_true(grepl("12_Rebecca_ignored_detailed.csv$", md$outpath))
  expect_files_modified_since(md$outpath, test_started)
  expect_false("stale output" %in% readLines(md$outpath))

  collision_path <- tempfile(fileext = ".csv")
  on.exit(unlink(collision_path), add = TRUE)
  readr::write_csv(data.frame(id = 1, value = 2), collision_path)
  expect_error(
    loadFRfile(collision_path, id = 12),
    "already exist"
  )
})

test_that("metadata output names retain distinct source stems", {
  output <- tempfile("metadata_output_")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)
  source <- file.path("testdata", "testdata_detailed.txt")

  test_started <- Sys.time()
  first <- convertFRFiles(
    source,
    outpath = file.path(output, "session_one.txt"),
    id = 12,
    subject = "Rebecca"
  )
  second <- convertFRFiles(
    source,
    outpath = file.path(output, "session_two.txt"),
    id = 12,
    subject = "Rebecca"
  )

  expect_false(identical(first$outpath, second$outpath))
  expect_true(all(file.exists(c(first$outpath, second$outpath))))
  expect_files_modified_since(c(first$outpath, second$outpath), test_started)
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
  output_dir <- tempfile("metadata_directory_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  test_started <- Sys.time()
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "^8895 mum FR9 Participant 1_00024.*detailed\\.xlsx$",
    id = function(path) 12,
    subject = function(path) "Rebecca",
    cores = 1L
  )
  writeLines("stale output", result$outpath[[1]])
  age_files(c(result$outpath, file.path(output_dir, "metadata.csv")))
  test_started <- Sys.time()
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "^8895 mum FR9 Participant 1_00024.*detailed\\.xlsx$",
    id = function(path) 12,
    subject = function(path) "Rebecca",
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(grepl(
    "^12_Rebecca_.*_detailed\\.csv$",
    basename(result$outpath)
  )))
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
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

  output <- tempfile("FR10_metadata_")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)
  test_started <- Sys.time()
  result <- convertFRDirectory(
    path,
    output,
    pattern = "_state\\.txt$",
    id = function(path) "fr10",
    subject = function(path) "fixture",
    cores = 1L
  )
  writeLines("stale output", result$outpath[[1]])
  age_files(c(result$outpath, file.path(output, "metadata.csv")))
  test_started <- Sys.time()
  result <- convertFRDirectory(
    path,
    output,
    pattern = "_state\\.txt$",
    id = function(path) "fr10",
    subject = function(path) "fixture",
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(grepl(
    "^fr10_fixture_.*_state\\.csv$",
    basename(result$outpath)
  )))
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
})
