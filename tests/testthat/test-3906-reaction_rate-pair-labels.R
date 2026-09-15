TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate includes pair labels like synchrony", {
  result <- test_coding |>
    convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30) |>
    reaction_rate()

  expect_true(all(c("denominator", "numerator") %in% names(result)))
  expect_true(all(c("n_episodes", "n_reactions") %in% names(result)))
  expect_true(all(result$denominator != result$numerator))
})
