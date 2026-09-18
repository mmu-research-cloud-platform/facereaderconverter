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


test_that("reaction_rate doesn't break with incomplete structure", {
  test_data_reaction <- test_coding |>
    convert_to_episodes(delta_window = 0.2, delta = 0.075, fps = 30)

  expect_no_error({
    react1 <- reaction_rate(
      test_data_reaction,
      time_limit_frames = 90L,
      exclude_start_frames = 0L,
      constraint_method = "frames",
      exclude_emotions = NULL
    )
  })

  expect_no_error({
    react2 <- reaction_rate(
      test_data_reaction[["coding"]],
      time_limit_frames = 90L,
      exclude_start_frames = 0L,
      constraint_method = "frames",
      exclude_emotions = NULL
    )
  })

  expect_equal(react1, react2)
})
