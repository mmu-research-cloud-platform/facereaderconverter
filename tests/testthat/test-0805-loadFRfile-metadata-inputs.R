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


test_that("loadFRfile adds scalar id and subject metadata", {
  path <- testthat::test_path("testdata", "testdata_detailed.txt")

  data <- loadFRfile(path, id = 1, subject = "child")

  expect_true(all(data$id == 1))
  expect_true(all(data$subject == "child"))
})

test_that("FR9 conversion derives media IDs and export filename subjects", {
  input_dir <- file.path(TEST_DATA, "FR9")
  skip_if(
    !dir.exists(input_dir),
    "The FR9 directory fixture is not available"
  )

  input_files <- list.files(
    input_dir,
    pattern = "\\.(txt|xlsx|csv)$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  skip_if(
    length(input_files) == 0L,
    "The FR9 directory contains no supported files"
  )

  metadata <- lapply(input_files, extract_subject_id_metadata)
  expected_subjects <- tools::file_path_sans_ext(basename(input_files))
  expected_ids <- vapply(
    input_files,
    function(path) {
      id <- fr_media_id(synchrony_fr_header_metadata(path)$video_filename)
      if (is.null(id)) NA_character_ else id
    },
    character(1)
  )
  expect_identical(
    vapply(metadata, `[[`, character(1), "subject"),
    expected_subjects
  )
  expect_identical(
    vapply(metadata, `[[`, character(1), "id"),
    unname(expected_ids)
  )

  converted <- Map(
    loadFRfile,
    input_files,
    MoreArgs = list()
  )
  expect_true(all(vapply(
    seq_along(converted),
    function(i) {
      if (is.na(expected_ids[[i]])) {
        !"id" %in% names(converted[[i]])
      } else {
        all(as.character(converted[[i]]$id) == expected_ids[[i]])
      }
    },
    logical(1)
  )))
  expect_true(all(vapply(
    seq_along(converted),
    function(i) all(converted[[i]]$subject == expected_subjects[[i]]),
    logical(1)
  )))
})
