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

test_that("reaction-rate internals retain infinite limits for episode mode", {
  inputs <- facereaderconverter:::prepare_reaction_rate_inputs(
    test_coding |>
      convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30),
    time_limit = Inf,
    constraint_method = "episode"
  )

  expect_identical(inputs$constraint_method, "episode")
  expect_identical(inputs$limit_frames, Inf)
})
