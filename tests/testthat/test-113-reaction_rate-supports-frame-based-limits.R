TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)
test_data_delta <- test_coding |>
  convert_to_episodes() |>
  add_delta_column(delta_window = 0.2, delta = 0.1, fps = 30)
test_that("reaction_rate supports frame-based limits", {
  seconds_limit <- reaction_rate(
    test_data_delta,
    episode_limit = 3,
    exclude_start = 0.1
  )
  frames_limit <- reaction_rate(
    test_data_delta,
    episode_limit_frames = 90,
    exclude_start_frames = 3
  )

  expect_equal(
    seconds_limit[, .(
      id,
      denominator,
      numerator,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )],
    frames_limit[, .(
      id,
      denominator,
      numerator,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )]
  )
})
