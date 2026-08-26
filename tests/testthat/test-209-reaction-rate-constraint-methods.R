TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction-rate internals retain infinite limits for episode mode", {
  inputs <- facereaderconverter:::prepare_reaction_rate_inputs(
    test_deltas,
    episode_limit = Inf,
    constraint_method = "episode"
  )

  expect_identical(inputs$constraint_method, "episode")
  expect_identical(inputs$limit_frames, Inf)
})
