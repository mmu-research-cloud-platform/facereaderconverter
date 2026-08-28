TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate constraint methods keep episode counts stable on test data", {
  methods <- c("strict", "episode", "frames", "loose")
  test_reaction <- test_coding |>
    convert_to_episodes(T_up = 0.15, T_down = 0.05, delta = 1) |>
    add_delta_column(delta = 0.75, fps = 30, delta_window = 0.2)

  results <- lapply(methods, function(method) {
    reaction_rate(
      test_reaction,
      episode_limit = 3L,
      exclude_start_frames = 4L,
      constraint_method = method,
      exclude_emotions = NULL
    )
  })

  expect_equal(results[[1L]][["n_episodes"]], results[[2L]][["n_episodes"]])
  expect_equal(results[[1L]][["n_episodes"]], results[[3L]][["n_episodes"]])
  expect_equal(results[[1L]][["n_episodes"]], results[[4L]][["n_episodes"]])

  expect_false(any(is.na(results[[1L]][["denominator"]])))
  expect_false(any(is.na(results[[1L]][["numerator"]])))
  expect_true(all(nzchar(results[[1L]][["denominator"]])))
  expect_true(all(nzchar(results[[1L]][["numerator"]])))

  expect_true(all(
    results[[1L]][["n_reactions"]] <= results[[2L]][["n_reactions"]]
  ))
  expect_true(all(
    results[[1L]][["n_reactions"]] <= results[[3L]][["n_reactions"]]
  ))
  expect_true(all(
    results[[1L]][["n_reactions"]] <= results[[4L]][["n_reactions"]]
  ))
  expect_true(any(results[[2L]][["reaction_rate"]] < 1))

  results2 <- lapply(methods, function(method) {
    reaction_rate_by_episode(
      test_reaction,
      episode_limit = 3L,
      exclude_start_frames = 4L,
      constraint_method = method,
      exclude_emotions = NULL
    )
  })

  expect_true(all(vapply(results2, inherits, logical(1), what = "data.table")))
})
