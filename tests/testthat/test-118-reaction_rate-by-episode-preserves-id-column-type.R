TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate_by_episode preserves id column type", {
  result <- reaction_rate_by_episode(test_deltas)

  expect_identical(typeof(result$id), typeof(test_deltas$id))
})
