TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate_by_episode frames constraint uses inclusive frame window", {
  expect_no_error({
    test_data_reaction <- test_coding |>
      convert_to_episodes() |>
      add_delta_column(
        delta_window = 0.2,
        delta = 0.1,
        fps = 30,
      ) |>
      reaction_rate_by_episode(
        episode_limit_frames = 90L,
        exclude_start_frames = 0L,
        constraint_method = "frames",
        exclude_emotions = NULL
      )
  })

  test_data_reaction |>
    mutate(net = end_frame - start_frame) |>
    pull(net) |>
    unique() |>
    expect_equal(89L)
})
