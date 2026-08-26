TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

make_constraint_data <- function() {
  data.table(
    id = c(rep(1L, 5), rep(2L, 4), rep(3L, 2), rep(4L, 2)),
    subject = "child",
    emotion = "happy",
    frame = c(1:5, 1:4, 1:2, 1:2),
    value = 0.5,
    delta = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L)
  )
}

test_that("constraint_method changes reaction classification as requested", {
  coded_data <- make_constraint_data()

  strict_result <- reaction_rate_by_episode(
    coded_data,
    episode_limit_frames = 4,
    exclude_start_frames = 3,
    constraint_method = "strict",
    exclude_emotions = NULL
  )
  episode_result <- reaction_rate_by_episode(
    coded_data,
    episode_limit_frames = 4,
    exclude_start_frames = 3,
    constraint_method = "episode",
    exclude_emotions = NULL
  )
  loose_result <- reaction_rate_by_episode(
    coded_data,
    episode_limit_frames = 4,
    exclude_start_frames = 3,
    constraint_method = "loose",
    exclude_emotions = NULL
  )
  frames_result <- reaction_rate_by_episode(
    coded_data,
    episode_limit_frames = 4,
    exclude_start_frames = 3,
    constraint_method = "frames",
    exclude_emotions = NULL
  )

  expect_identical(strict_result$reaction, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(episode_result$reaction, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(loose_result$reaction, c(TRUE, TRUE, TRUE, TRUE))
  expect_identical(frames_result$reaction, c(TRUE, TRUE, TRUE, TRUE))
})

test_that("episode matches strict when the episode limit is infinite", {
  coded_data <- make_constraint_data()

  strict_result <- reaction_rate_by_episode(
    coded_data,
    episode_limit = Inf,
    exclude_start_frames = 3,
    constraint_method = "strict",
    exclude_emotions = NULL
  )
  episode_result <- reaction_rate_by_episode(
    coded_data,
    episode_limit = Inf,
    exclude_start_frames = 3,
    constraint_method = "episode",
    exclude_emotions = NULL
  )

  expect_equal(strict_result, episode_result, ignore_attr = TRUE)
})

test_that("reaction_rate_by_episode with episode constraint matches fr_coding episodes", {
  coding_df <- data.table(
    id = 1L,
    subject = "child",
    frame = 1:8,
    happy = c(0.1, 0.3, 0.4, 0.1, 0.1, 0.35, 0.4, 0.1)
  )

  coded_data <- convert_to_episodes(
    coding_df,
    T_up = 0.25,
    T_down = 0.15,
    delta = 0.05,
    delta_window = 0.1,
    min_dur_sec = 0.1,
    fps = 10L
  )
  coded_data$coding[, delta := as.integer(in_state)]

  by_episode <- reaction_rate_by_episode(
    coded_data,
    constraint_method = "episode",
    exclude_emotions = NULL
  )
  summary_result <- reaction_rate(
    coded_data,
    constraint_method = "episode",
    exclude_emotions = NULL
  )

  expect_equal(nrow(by_episode), nrow(coded_data$episodes))
  expect_equal(sum(summary_result$n_episodes), nrow(coded_data$episodes))
})
