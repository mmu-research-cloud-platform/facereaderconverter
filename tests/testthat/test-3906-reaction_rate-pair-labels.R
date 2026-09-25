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

test_that("reaction_rate includes pair labels like synchrony", {
  result <- test_coding |>
    convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30) |>
    reaction_rate()

  expect_true(all(c("denominator", "numerator") %in% names(result)))
  expect_true(all(c("n_episodes", "n_reactions") %in% names(result)))
  expect_true(all(result$denominator != result$numerator))
})
