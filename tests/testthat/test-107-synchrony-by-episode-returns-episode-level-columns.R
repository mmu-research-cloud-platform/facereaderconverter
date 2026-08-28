TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)
test_data_sync <- test_coding |>
  convert_to_episodes()
test_that("synchrony_by_episode returns episode-level columns", {
  result <- synchrony_by_episode(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0
  )

  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "run_id",
      "present_prop",
      "synchrony"
    ) %in%
      names(result)
  ))
  expect_true(is.logical(result$synchrony))
  expect_true(is.numeric(result$present_prop))
  expect_true(all(result$present_prop >= 0 & result$present_prop <= 1))
  expect_true(all(result$denominator != result$numerator))
  expect_false(any(result$emotion == "neutral"))
  expect_true(all(result$run_id %in% test_data_sync$episodes$run_id))
})
