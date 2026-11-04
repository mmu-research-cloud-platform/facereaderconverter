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

new_metadata_test_txt <- function() {
  directory <- tempfile("metadata-test-")
  dir.create(directory)
  path <- file.path(directory, "invented-export_detailed.txt")
  writeLines(
    c(
      "Video analysis detailed log",
      "Filename\tinvented-media-id.mp4",
      "Frame rate\t30",
      "",
      "Video Time\tNeutral",
      "00:00:00.000\t0.5"
    ),
    path
  )
  path
}

test_that("extract_subject_id_metadata separates media and FaceReader names", {
  path <- new_metadata_test_txt()
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  metadata <- extract_subject_id_metadata(path)

  expect_identical(metadata$id, "invented-media-id")
  expect_identical(metadata$subject, "invented-export_detailed")
})

test_that("extract_subject_id_metadata applies patterns to their source names", {
  path <- new_metadata_test_txt()
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  metadata <- extract_subject_id_metadata(
    path,
    id_pattern = "media",
    subject_pattern = "export"
  )

  expect_identical(metadata$id, "media")
  expect_identical(metadata$subject, "export")
})

test_that("extract_subject_id_metadata applies patterns to full paths", {
  path <- new_metadata_test_txt()
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  metadata <- extract_subject_id_metadata(
    path,
    id_pattern = "invented-media-id\\.mp4",
    subject_pattern = "invented-export",
    use_full_path = TRUE
  )

  expect_identical(metadata$id, "invented-media-id.mp4")
  expect_identical(metadata$subject, "invented-export")
})

test_that("extract_subject_id_metadata returns NA for missing media filename", {
  directory <- tempfile("metadata-test-")
  dir.create(directory)
  path <- file.path(directory, "invented-export_detailed.csv")
  writeLines("video_time,neutral\n00:00:00.000,0.5", path)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  metadata <- extract_subject_id_metadata(path)

  expect_identical(metadata$id, NA_character_)
  expect_identical(metadata$subject, "invented-export_detailed")
})

test_that("extract_id_subject_metadata remains a compatibility alias", {
  path <- new_metadata_test_txt()
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  expect_identical(
    extract_id_subject_metadata(path),
    list(id = "invented-media-id", subject = "invented-export_detailed")
  )
})
