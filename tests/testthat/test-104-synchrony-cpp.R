TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("synchrony returns stable results through compiled backend", {
  result <- synchrony(test_data_sync, missing_threshold = 0)

  expect_s3_class(result, "data.table")
  expect_setequal(
    names(result),
    c("id", "denominator", "numerator", "emotion", "n_episodes", "synchrony")
  )
  expect_true(all(result$denominator != result$numerator))
})

test_that("synchrony handles excluded emotions with compiled backend", {
  result <- synchrony(
    test_data_sync,
    missing_threshold = 0,
    exclude_emotions = c("neutral", "happy")
  )

  expect_false(any(result$emotion %in% c("neutral", "happy")))
})
