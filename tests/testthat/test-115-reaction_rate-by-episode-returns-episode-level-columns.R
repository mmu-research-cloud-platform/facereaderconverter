TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate_by_episode returns episode-level columns", {
  result <- reaction_rate_by_episode(test_deltas)

  expect_s3_class(result, "data.table")
  expect_identical(
    names(result),
    c(
      "id",
      "subject",
      "emotion",
      "episode_id",
      "start_frame",
      "end_frame",
      "n_frames",
      "present_prop",
      "reaction"
    )
  )
  expect_type(result$reaction, "logical")
  expect_type(result$present_prop, "double")
  expect_true(all(result$present_prop >= 0 & result$present_prop <= 1))
  expect_false(any(result$emotion == "neutral"))
})
