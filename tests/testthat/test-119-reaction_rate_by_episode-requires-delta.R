TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)


test_that("reaction_rate_by_episode requires a delta column", {
  expect_error(
    reaction_rate_by_episode(dplyr::select(test_deltas, -delta)),
    "`coded_data` is missing required columns: delta.",
    fixed = TRUE
  )
})
