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

test_that("reaction_rate validates its inputs", {
  expect_error(
    reaction_rate(dplyr::select(test_deltas, -delta)),
    "`coded_data` is missing required columns: delta.",
    fixed = TRUE
  )
  expect_error(
    reaction_rate(test_deltas, fps = 0),
    "`fps` must be a positive integer scalar.",
    fixed = TRUE
  )
  expect_error(
    reaction_rate(test_deltas, episode_limit = 0),
    "`episode_limit` must be a numeric scalar > 0.",
    fixed = TRUE
  )
  expect_error(
    reaction_rate(test_deltas, exclude_start = -0.1),
    "`exclude_start` must be a numeric scalar >= 0.",
    fixed = TRUE
  )
})
