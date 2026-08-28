TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)


test_that("episode matches strict when the episode limit is infinite", {
  test_data_delta <- test_coding |>
    convert_to_episodes() |>
    add_delta_column(
      delta_window = 0.2,
      delta = 0.1,
      fps = 30,
    )

  strict_result <- reaction_rate_by_episode(
    test_data_delta,
    episode_limit = Inf,
    exclude_start_frames = 3,
    constraint_method = "strict",
    exclude_emotions = NULL
  )
  episode_result <- reaction_rate_by_episode(
    test_data_delta,
    episode_limit = Inf,
    exclude_start_frames = 3,
    constraint_method = "episode",
    exclude_emotions = NULL
  )

  expect_equal(strict_result, episode_result, ignore_attr = TRUE)
})

# test_that("constraint_method changes reaction classification by window reach", {
#   test_data_delta <- test_coding |>
#     convert_to_episodes() |>
#     add_delta_column(delta_window = 0.2, delta = 0.1, fps = 30)

#   strict_result <- reaction_rate_by_episode(
#     test_data_delta,
#     episode_limit_frames = 90L,
#     exclude_start_frames = 0L,
#     constraint_method = "strict",
#     exclude_emotions = NULL
#   )
#   episode_result <- reaction_rate_by_episode(
#     test_data_delta,
#     episode_limit_frames = 90L,
#     exclude_start_frames = 0L,
#     constraint_method = "episode",
#     exclude_emotions = NULL
#   )
#   loose_result <- reaction_rate_by_episode(
#     test_data_delta,
#     episode_limit_frames = 90L,
#     exclude_start_frames = 0L,
#     constraint_method = "loose",
#     exclude_emotions = NULL
#   )
#   frames_result <- reaction_rate_by_episode(
#     test_data_delta,
#     episode_limit_frames = 90L,
#     exclude_start_frames = 0L,
#     constraint_method = "frames",
#     exclude_emotions = NULL
#   )
# })
