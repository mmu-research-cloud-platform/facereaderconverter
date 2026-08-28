TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate_by_episode returns synchrony-shaped episode columns", {
  test_data_delta <- test_coding |>
    convert_to_episodes() |>
    add_delta_column(delta_window = 0.2, delta = 0.1, fps = 30)

  result <- reaction_rate_by_episode(test_data_delta)
  expect_s3_class(result, "data.table")
  expect_identical(
    names(result),
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "run_id",
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
  expect_true(all(result$denominator != result$numerator))
  expect_false(any(result$emotion == "neutral"))
})
