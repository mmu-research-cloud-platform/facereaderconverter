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
  output_dir <- tempfile("fr9_id_subject_")
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
  expect_identical(
    vapply(metadata, `[[`, character(1), "subject"),
    tools::file_path_sans_ext(basename(input_files))
  )
  expect_true(all(!is.na(vapply(metadata, `[[`, character(1), "id"))))

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

  unlink(output_dir, recursive = TRUE, force = TRUE)
  dir.create(output_dir, recursive = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "8895.*(detailed|state)\\.xlsx$",
    cores = 1L,
    save_metadata = NULL
  )

  writeLines("stale output", result$outpath[[1]])
  age_files(result$outpath)
  test_started <- Sys.time()
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    pattern = "8895.*(detailed|state)\\.xlsx$",
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
