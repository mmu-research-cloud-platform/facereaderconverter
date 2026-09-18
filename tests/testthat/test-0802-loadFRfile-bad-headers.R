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

skip_if_not_installed("readxl")

test_that("loadFRfile handles bad headers", {
  expect_message(
    bad_header <- loadFRfile(file.path(
      "testdata",
      "testdata_detailed_fail.txt"
    )),
    "FaceReader header row not found"
  )
  expect_null(bad_header)
})
