TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate validates constraint_method and infinite limits", {
  test_data_delta <- test_coding |>
    convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30)

  expect_error(
    reaction_rate(test_data_delta, constraint_method = "bad"),
    '`constraint_method` must be one of: "strict", "episode", "loose", "frames".',
    fixed = TRUE
  )

  expect_error(
    reaction_rate(
      test_data_delta,
      episode_limit = Inf,
      constraint_method = "loose"
    ),
    paste0(
      "`episode_limit` or `episode_limit_frames` cannot be infinite when ",
      '`constraint_method` is "loose" or "frames".'
    ),
    fixed = TRUE
  )

  expect_error(
    reaction_rate(
      test_data_delta,
      episode_limit_frames = Inf,
      constraint_method = "frames"
    ),
    paste0(
      "`episode_limit` or `episode_limit_frames` cannot be infinite when ",
      '`constraint_method` is "loose" or "frames".'
    ),
    fixed = TRUE
  )
})
