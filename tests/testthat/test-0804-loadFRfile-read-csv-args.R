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

test_that("loadFRfile keeps CSV and name-repair arguments separate", {
  data <- loadFRfile(
    file.path("testdata", "testdata_detailed.csv"),
    csv_args = list(locale = readr::locale()),
    clean_names_args = list(case = "upper_camel")
  )

  expect_true("VideoTime" %in% names(data))
})
