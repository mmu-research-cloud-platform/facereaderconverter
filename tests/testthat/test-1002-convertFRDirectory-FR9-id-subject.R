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

test_that("convertFRDirectory writes the verified FR9 id-subject output", {
  skip_if_not_installed("readxl")

  input_dir <- file.path(TEST_DATA, "FR9")
  output_dir <- file.path(TEST_DATA, "converted", "FR9_id_subject")
  skip_if(
    !dir.exists(input_dir),
    "The FR9 directory fixture is not available"
  )

  input_files <- list.files(
    input_dir,
    pattern = "\\.xlsx$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  skip_if(
    length(input_files) == 0L,
    "The FR9 directory contains no Excel files"
  )

  metadata <- lapply(input_files, extract_subject_id_metadata)
  valid <- vapply(
    metadata,
    function(x) !is.na(x$id) && !is.na(x$subject),
    logical(1)
  )
  expect_true(any(valid))
  expect_true(all(
    vapply(metadata[valid], `[[`, character(1), "subject") %in% c("mum", "teen")
  ))
  expect_true(all(
    grepl("^[0-9]{4}$", vapply(metadata[valid], `[[`, character(1), "id"))
  ))

  # The two 1218 files intentionally lack a mum/teen subject.
  expect_true(any(!valid))
  expect_true(all(is.na(vapply(
    metadata[!valid],
    `[[`,
    character(1),
    "subject"
  ))))

  selected <- input_files[
    grepl(
      "8895.*(detailed|state)\\.xlsx$",
      basename(input_files),
      ignore.case = TRUE
    )
  ]
  expect_length(selected, 2L)
  expect_setequal(
    basename(selected),
    c(
      "8895 mum FR9 Participant 1_00024 mum_Analysis 2_video_20260908_135003_detailed.xlsx",
      "8895 mum FR9_00024 mum_Analysis 2_video_20260908_135003_state.xlsx"
    )
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "8895.*(detailed|state)\\.xlsx$",
    id = function(path) extract_subject_id_metadata(path)$id,
    subject = function(path) extract_subject_id_metadata(path)$subject,
    cores = 1L,
    save_metadata = NULL
  )

  writeLines("stale output", result$outpath[[1]])
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "8895.*(detailed|state)\\.xlsx$",
    id = function(path) extract_subject_id_metadata(path)$id,
    subject = function(path) extract_subject_id_metadata(path)$subject,
    cores = 1L,
    save_metadata = NULL
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(file.exists(result$outpath)))
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
  expect_setequal(
    basename(result$inpath),
    basename(selected)
  )
  expect_setequal(
    basename(result$outpath),
    c("8895_mum_detailed.csv", "8895_mum_state.csv")
  )
  expect_true(all(file.exists(result$outpath)))

  converted <- lapply(result$outpath, readr::read_csv, show_col_types = FALSE)
  expect_true(all(vapply(
    converted,
    function(data) all(as.character(data$id) == "8895"),
    logical(1)
  )))
  expect_true(all(vapply(
    converted,
    function(data) all(data$subject == "mum"),
    logical(1)
  )))
  expect_setequal(
    list.files(output_dir),
    c("8895_mum_detailed.csv", "8895_mum_state.csv")
  )
})

test_that("convertFRDirectory rejects unsafe id-subject output components", {
  input_dir <- tempfile("fr_metadata_input_")
  output_dir <- tempfile("fr_metadata_output_")
  dir.create(input_dir)
  on.exit(
    unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  file.copy(
    testthat::test_path("testdata", "testdata_detailed.txt"),
    file.path(input_dir, "source.txt")
  )

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    id = "1001",
    subject = "mum/teen",
    cores = 1L,
    save_metadata = NULL
  )

  expect_identical(result$status, "Fail")
  expect_match(result$error, "safe filename components")
  expect_false(file.exists(file.path(output_dir, "1001_mum_teen.csv")))
})

test_that("convertFRDirectory rejects duplicate metadata-derived destinations", {
  input_dir <- tempfile("fr_duplicate_input_")
  output_dir <- tempfile("fr_duplicate_output_")
  dir.create(input_dir)
  on.exit(
    unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  source <- file.path(TEST_DATA, "testdata_detailed.csv")
  file.copy(source, file.path(input_dir, "first.csv"))
  file.copy(source, file.path(input_dir, "second.csv"))

  expect_error(
    convertFRDirectory(
      input_dir,
      output_dir,
      id = "1001",
      subject = "mum",
      cores = 1L,
      save_metadata = NULL
    ),
    "same output destination"
  )
  expect_false(file.exists(file.path(output_dir, "1001_mum_detailed.csv")))
})

test_that("FR10 directory conversion supports explicit id and subject metadata", {
  path <- file.path(Sys.getenv("TEST_DATA"), "FR10")
  skip_if(!dir.exists(path), "The FR10 directory fixture is not available")
  files <- list.files(
    path,
    pattern = "_state\\.txt$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(length(files) == 0L, "The FR10 directory contains no state TXT files")

  output <- file.path(TEST_DATA, "converted", "FR10_id_subject")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  result <- convertFRDirectory(
    path,
    output,
    pattern = "_state\\.txt$",
    id = "fr10",
    subject = "fixture",
    cores = 1L,
    save_metadata = NULL
  )
  writeLines("stale output", result$outpath[[1]])
  result <- convertFRDirectory(
    path,
    output,
    pattern = "_state\\.txt$",
    id = "fr10",
    subject = "fixture",
    cores = 1L,
    save_metadata = NULL
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(file.exists(result$outpath)))
  converted <- readr::read_csv(result$outpath[[1]], show_col_types = FALSE)
  expect_true(all(converted$id == "fr10"))
  expect_true(all(converted$subject == "fixture"))
})
