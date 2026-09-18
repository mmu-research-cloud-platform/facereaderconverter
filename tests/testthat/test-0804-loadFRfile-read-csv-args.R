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

test_that("loadFRfile passes read_csv args without leaking them into clean_names", {
  expect_no_error(
    loadFRfile(
      file.path("testdata", "testdata_detailed.csv"),
      locale = readr::locale()
    )
  )
})
