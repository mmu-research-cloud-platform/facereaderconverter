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

test_that("loadFRfile rejects unsupported extensions", {
  tmp <- tempfile(fileext = ".rds")
  file.copy(file.path("testdata", "testdata_detailed.txt"), tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_error(
    loadFRfile(tmp),
    "Unsupported file extension: \\.rds"
  )
})
