TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_data_sync <- test_coding |>
  convert_to_episodes()

test_that("synchrony requires a limit for constrained methods", {
  for (method in c("strict", "loose", "frames")) {
    expect_error(
      synchrony(
        test_data_sync,
        time_limit = NULL,
        time_limit_frames = NULL,
        constraint_method = method
      ),
      "`time_limit` or `time_limit_frames` must be set when `constraint_method` is not \"episode\"."
    )
    expect_error(
      synchrony_by_episode(
        test_data_sync,
        time_limit = NULL,
        time_limit_frames = NULL,
        constraint_method = method
      ),
      "`time_limit` or `time_limit_frames` must be set when `constraint_method` is not \"episode\"."
    )
  }
})

test_that("reaction rate requires a limit for constrained methods", {
  for (method in c("strict", "loose", "frames")) {
    expect_error(
      reaction_rate(
        test_data_sync,
        time_limit = NULL,
        time_limit_frames = NULL,
        constraint_method = method
      ),
      "`time_limit` or `time_limit_frames` must be set when `constraint_method` is not \"episode\"."
    )
    expect_error(
      reaction_rate_by_episode(
        test_data_sync,
        time_limit = NULL,
        time_limit_frames = NULL,
        constraint_method = method
      ),
      "`time_limit` or `time_limit_frames` must be set when `constraint_method` is not \"episode\"."
    )
  }
})

test_that("episode constraint does not require an explicit limit", {
  expect_no_error(
    synchrony(
      test_data_sync,
      time_limit = NULL,
      time_limit_frames = NULL,
      constraint_method = "episode"
    )
  )
  expect_no_error(
    synchrony_by_episode(
      test_data_sync,
      time_limit = NULL,
      time_limit_frames = NULL,
      constraint_method = "episode"
    )
  )
  expect_no_error(
    reaction_rate(
      test_data_sync,
      time_limit = NULL,
      time_limit_frames = NULL,
      constraint_method = "episode"
    )
  )
  expect_no_error(
    reaction_rate_by_episode(
      test_data_sync,
      time_limit = NULL,
      time_limit_frames = NULL,
      constraint_method = "episode"
    )
  )
})
