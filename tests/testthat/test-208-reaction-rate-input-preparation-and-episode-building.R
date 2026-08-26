TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction-rate input preparation and episode building work directly", {
  inputs <- facereaderconverter:::prepare_reaction_rate_inputs(
    test_deltas,
    episode_limit_frames = 90L,
    exclude_start_frames = 3L
  )

  expect_s3_class(inputs$dt, "data.table")
  expect_identical(inputs$limit_frames, 90L)
  expect_identical(inputs$start_frames, 3L)
  expect_identical(inputs$minimum_threshold, 0)
  expect_true(all(
    c("id", "subject", "emotion", "frame", "delta") %in% names(inputs$dt)
  ))

  episode_table <- facereaderconverter:::build_reaction_rate_episode_table(
    inputs
  )

  expect_s3_class(episode_table, "data.table")
  expect_true(all(
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
    ) %in%
      names(episode_table)
  ))
  expect_type(episode_table$reaction, "logical")
  expect_true(all(episode_table$n_frames >= 1L))
  expect_true(all(
    episode_table$present_prop >= 0 & episode_table$present_prop <= 1
  ))
})
