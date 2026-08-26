TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate validates constraint_method and infinite limits", {
  expect_snapshot(
    error = TRUE,
    reaction_rate(test_deltas, constraint_method = "bad")
  )

  expect_snapshot(
    error = TRUE,
    reaction_rate(
      test_deltas,
      episode_limit = Inf,
      constraint_method = "loose"
    )
  )

  expect_snapshot(
    error = TRUE,
    reaction_rate(
      test_deltas,
      episode_limit_frames = Inf,
      constraint_method = "frames"
    )
  )
})
