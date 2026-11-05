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

test_that("extract_subject_id_metadata separates media and FaceReader names", {
  path <- test_fixture_path("c2e_primary_txt")
  metadata <- extract_subject_id_metadata(path)

  expect_identical(metadata$id, fixture_expected_primary_media_id())
  expect_identical(
    metadata$subject,
    tools::file_path_sans_ext(basename(path))
  )
})

test_that("extract_subject_id_metadata applies patterns to their source names", {
  path <- test_fixture_path("c2e_primary_txt")
  metadata <- extract_subject_id_metadata(
    path,
    id_pattern = test_fixture_id("8892"),
    subject_pattern = "detailed"
  )

  expect_identical(metadata$id, test_fixture_id("8892"))
  expect_identical(metadata$subject, "detailed")
})

test_that("extract_subject_id_metadata applies patterns to full paths", {
  path <- test_fixture_path("brazil_detailed_xlsx")
  id_pattern <- paste0("Downloads[/\\\\]#", test_fixture_id("8883"))
  subject_pattern <- "brazil[/\\\\]Participant 10"
  expect_match(path, subject_pattern)
  expect_false(grepl(subject_pattern, basename(path)))
  metadata <- extract_subject_id_metadata(
    path,
    id_pattern = id_pattern,
    subject_pattern = subject_pattern,
    use_full_path = TRUE
  )

  expect_match(metadata$id, id_pattern)
  expect_match(metadata$subject, subject_pattern)
})

test_that("extract_subject_id_metadata returns NA for missing media filename", {
  path <- test_fixture_path("detailed_csv")
  metadata <- extract_subject_id_metadata(path)

  expect_true(is.na(metadata$id))
  expect_identical(metadata$subject, tools::file_path_sans_ext(basename(path)))
})

test_that("extract_id_subject_metadata remains a compatibility alias", {
  expect_identical(
    extract_id_subject_metadata(test_fixture_path("c2e_primary_txt")),
    extract_subject_id_metadata(test_fixture_path("c2e_primary_txt"))
  )
})
