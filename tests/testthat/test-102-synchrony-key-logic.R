TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)
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
