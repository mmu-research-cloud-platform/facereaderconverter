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

test_that("reaction_rate respects episode and start limits", {
  long_window <- reaction_rate(
    test_data_delta,
    time_limit = 3,
    exclude_start = 0.1
  )
  short_window <- reaction_rate(
    test_data_delta,
    time_limit = 1,
    exclude_start = 0.1
  )
  delayed_window <- reaction_rate(
    test_data_delta,
    time_limit = 3,
    exclude_start_frames = 30L
  )

  expect_equal(long_window$n_episodes, short_window$n_episodes)
  expect_true(
    sum(delayed_window$n_reactions, na.rm = TRUE) <=
      sum(long_window$n_reactions, na.rm = TRUE)
  )
})
