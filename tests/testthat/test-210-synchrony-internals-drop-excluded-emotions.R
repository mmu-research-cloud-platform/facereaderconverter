TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("synchrony internals drop excluded emotions", {
  result <- test_coding |>
    convert_to_episodes() |>
    synchrony(missing_threshold = 0, exclude_emotions = "neutral")

  expect_false(any(result$coding$emotion == "neutral"))
  expect_false(any(result$episodes$emotion == "neutral"))
})
