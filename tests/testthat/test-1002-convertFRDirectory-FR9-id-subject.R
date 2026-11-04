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

  input_dir <- tempfile("fr9_id_subject_input_")
  dir.create(input_dir)
  output_dir <- tempfile("fr9_id_subject_")
  on.exit(unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE), add = TRUE)
  input_files <- c(
    test_fixture_path("fr9_detailed_xlsx"),
    test_fixture_path("fr9_state_xlsx")
  )
  file.copy(input_files, input_dir)
  input_files <- file.path(input_dir, basename(input_files))

  metadata <- lapply(input_files, extract_subject_id_metadata)
  expect_identical(
    vapply(metadata, `[[`, character(1), "subject"),
    tools::file_path_sans_ext(basename(input_files))
  )
  expect_true(all(!is.na(vapply(metadata, `[[`, character(1), "id"))))

  selected <- input_files

  dir.create(output_dir, recursive = TRUE)

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "_(detailed|state)\\.xlsx$",
    id = function(path) extract_subject_id_metadata(path)$id,
    subject = function(path) extract_subject_id_metadata(path)$subject,
    cores = 1L,
    save_metadata = NULL
  )

  writeLines("stale output", result$outpath[[1]])
  age_files(result$outpath)
  test_started <- Sys.time()
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "_(detailed|state)\\.xlsx$",
    id = function(path) extract_subject_id_metadata(path)$id,
    subject = function(path) extract_subject_id_metadata(path)$subject,
    cores = 1L,
    save_metadata = NULL
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
  expect_setequal(
    basename(result$inpath),
    basename(selected)
  )
  expect_setequal(
    basename(result$outpath),
    vapply(
      selected,
      function(path) {
        data <- loadFRfile(path)
        basename(fr_output_path(
          file.path(output_dir, basename(path)),
          metadata_columns(data),
          fr_conversion_type(data)
        ))
      },
      character(1)
    )
  )
  expect_true(all(file.exists(result$outpath)))

  converted <- lapply(result$outpath, readr::read_csv, show_col_types = FALSE)
  expect_true(all(vapply(
    seq_along(converted),
    function(i) {
      all(
        as.character(converted[[i]]$id) ==
          fr_media_id(
            synchrony_fr_header_metadata(selected[[i]])$video_filename
          )
      )
    },
    logical(1)
  )))
  expect_true(all(vapply(
    seq_along(converted),
    function(i) {
      all(
        converted[[i]]$subject ==
          tools::file_path_sans_ext(basename(selected[[i]]))
      )
    },
    logical(1)
  )))
  expect_setequal(list.files(output_dir), basename(result$outpath))
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

test_that("convertFRDirectory retains source stems for shared metadata", {
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

  test_started <- Sys.time()
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    id = "1001",
    subject = "mum",
    cores = 1L,
    save_metadata = NULL
  )

  expect_true(all(result$status == "Success"))
  expect_length(unique(result$outpath), 2L)
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
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
  age_files(result$outpath)
  test_started <- Sys.time()
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
  expect_files_modified_since(result$outpath, test_started)
  converted <- readr::read_csv(result$outpath[[1]], show_col_types = FALSE)
  expect_true(all(converted$id == "fr10"))
  expect_true(all(converted$subject == "fixture"))
})
