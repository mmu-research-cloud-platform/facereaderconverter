TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("synchrony preserves id column type", {
  result <- synchrony(test_data_sync, missing_threshold = 0)

  expect_identical(typeof(result$id), typeof(test_data_sync$coding$id))
})
