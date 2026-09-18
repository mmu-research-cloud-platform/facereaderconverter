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

test_data_delta <- test_coding |>
  convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30)

test_that("reaction_rate inherits fps from fr_coding input", {
  expect_s3_class(test_data_delta, "fr_coding")
  expect_true(nrow(test_data_delta$coding) > 0)
  expect_identical(test_data_delta$metadata$fps, 30L)
  expect_no_error(reaction_rate(test_data_delta, fps = 12L))
})
