TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("synchrony internals drop excluded emotions", {
  result <- facereaderconverter:::prepare_synchrony_inputs(
    test_data_sync,
    missing_threshold = 0,
    exclude_emotions = "neutral"
  )

  expect_false(any(result$coding$emotion == "neutral"))
  expect_false(any(result$episodes$emotion == "neutral"))
})
