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


test_that("episode matches strict when the episode limit is infinite", {
  test_data_delta <- test_coding |>
    convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30)

  strict_result <- reaction_rate_by_episode(
    test_data_delta,
    time_limit = Inf,
    exclude_start_frames = 3,
    constraint_method = "strict",
    exclude_emotions = NULL
  )
  episode_result <- reaction_rate_by_episode(
    test_data_delta,
    time_limit = Inf,
    exclude_start_frames = 3,
    constraint_method = "episode",
    exclude_emotions = NULL
  )

  expect_equal(strict_result, episode_result, ignore_attr = TRUE)
})
