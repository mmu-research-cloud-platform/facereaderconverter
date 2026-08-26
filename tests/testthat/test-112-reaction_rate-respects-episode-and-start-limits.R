TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

make_with_short_delta <- function(x) {
  out <- copy(x)
  out[, delta := 0L]
  out[1:2, delta := 1L]
  out
}

test_that("reaction_rate respects episode and start limits", {
  long_window <- reaction_rate(
    test_deltas,
    episode_limit = 3,
    exclude_start = 0.1
  )
  short_window <- reaction_rate(
    test_deltas,
    episode_limit = 1,
    exclude_start = 0.1
  )

  expect_true(
    sum(short_window$n_episodes, na.rm = TRUE) <=
      sum(long_window$n_episodes, na.rm = TRUE)
  )
})
