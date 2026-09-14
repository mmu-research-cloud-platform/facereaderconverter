TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)
test_data_sync <- test_coding |>
  convert_to_episodes()
test_that("synchrony missing threshold is applied", {
  test_data_sync1 <- synchrony(test_data_sync, missing_threshold = 1)
  test_data_sync0 <- synchrony(test_data_sync, missing_threshold = 0)

  expect_equal(nrow(test_data_sync1), nrow(test_data_sync0))

  comparison <- merge(
    test_data_sync1[, .(
      id,
      denominator,
      numerator,
      emotion,
      n_episodes_1 = n_episodes,
      synchrony_1 = synchrony
    )],
    test_data_sync0[, .(
      id,
      denominator,
      numerator,
      emotion,
      n_episodes_0 = n_episodes,
      synchrony_0 = synchrony
    )],
    by = c("id", "denominator", "numerator", "emotion")
  )

  expect_true(all(comparison$n_episodes_1 <= comparison$n_episodes_0))
  expect_true(any(comparison$n_episodes_1 == 0L))
  expect_true(all(is.na(comparison$synchrony_1[comparison$n_episodes_1 == 0L])))
})


test_that("synch rows", {
  test_data_sync_agg <- synchrony(test_data_sync, missing_threshold = 0)
  test_data_sync_row <- synchrony_by_episode(
    test_data_sync,
    missing_threshold = 0
  )

  expect_equal(sum(test_data_sync_agg$n_episodes), nrow(test_data_sync_row))

  test_data_sync_row1 <- synchrony_by_episode(
    test_data_sync,
    missing_threshold = 1
  )

  expect_gte(nrow(test_data_sync_row), nrow(test_data_sync_row1))
})


test_that("synchrony_by_episode frames constraint uses inclusive frame window", {
  expect_no_error({
    test_data_synchrony <- test_coding |>
      convert_to_episodes(T_up = 0.15, T_down = 0.05, fps = 30) |>
      synchrony_by_episode(
        time_limit_frames = 90L,
        constraint_method = "frames",
        exclude_emotions = NULL
      )
  })
  expect_equal(
    test_data_synchrony |>
      dplyr::mutate(net = end_frame - start_frame) |>
      dplyr::pull(net) |>
      unique(),
    89L
  )
})

test_that("synchrony constraint methods keep episode counts stable on test data", {
  methods <- c("strict", "episode", "frames", "loose")
  test_sync <- test_coding |>
    convert_to_episodes(
      T_up = 0.15,
      T_down = 0.05,
      delta = 1,
      fps = 30,
      delta_window = 0.2
    )

  results_sync <- lapply(methods, function(method) {
    synchrony(
      test_sync,
      time_limit = 3L,
      constraint_method = method,
      exclude_emotions = NULL
    ) |>
      mutate(synchronies = n_episodes * synchrony)
  })

  expect_false(any(is.na(results_sync[[1L]][["denominator"]])))
  expect_false(any(is.na(results_sync[[1L]][["numerator"]])))
  expect_true(all(nzchar(results_sync[[1L]][["denominator"]])))
  expect_true(all(nzchar(results_sync[[1L]][["numerator"]])))

  #strict < episode
  expect_true(all(
    results_sync[[1L]][["synchronies"]] <= results_sync[[2L]][["synchronies"]]
  ))
  #strict < frames
  expect_true(all(
    results_sync[[1L]][["synchronies"]] <= results_sync[[3L]][["synchronies"]]
  ))
  #strict < loose
  expect_true(all(
    results_sync[[1L]][["synchronies"]] <= results_sync[[4L]][["synchronies"]]
  ))
  #epsiode < loose
  expect_true(all(
    results_sync[[2L]][["synchronies"]] <= results_sync[[4L]][["synchronies"]]
  ))
  expect_true(any(results_sync[[2L]][["synchrony"]] < 1))

  results2 <- lapply(methods, function(method) {
    synchrony_by_episode(
      test_sync,
      time_limit = 3L,
      constraint_method = method,
      exclude_emotions = NULL
    )
  })

  expect_true(all(vapply(results2, inherits, logical(1), what = "data.table")))
})
