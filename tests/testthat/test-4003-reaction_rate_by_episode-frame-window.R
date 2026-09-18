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

test_that("reaction_rate_by_episode frames constraint uses inclusive frame window", {
  expect_no_error({
    test_data_reaction <- test_coding |>
      convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30) |>
      reaction_rate_by_episode(
        time_limit_frames = 90L,
        exclude_start_frames = 0L,
        constraint_method = "frames",
        exclude_emotions = NULL
      )
  })

  test_data_reaction |>
    dplyr::mutate(net = end_frame - start_frame) |>
    dplyr::pull(net) |>
    unique() |>
    expect_equal(89L)
})
