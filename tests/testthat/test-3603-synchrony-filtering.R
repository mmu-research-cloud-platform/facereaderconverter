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

library(testthat)
library(data.table)

test_that("synchrony internals drop excluded emotions", {
  result <- test_coding |>
    convert_to_episodes() |>
    synchrony(missing_threshold = 0, exclude_emotions = "neutral")

  expect_false(any(result$coding$emotion == "neutral"))
  expect_false(any(result$episodes$emotion == "neutral"))
})
