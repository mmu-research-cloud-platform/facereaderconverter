TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_data_delta <- test_coding |>
  convert_to_episodes() |>
  add_delta_column(delta_window = 0.2, delta = 0.1, fps = 30)

test_that("reaction_rate returns synchrony-shaped summary columns", {
  expect_no_error({
    test_reaction_rate <- reaction_rate(test_data_delta)
  })
  expect_s3_class(test_reaction_rate, "data.table")
  expect_true(all(
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "n_episodes",
      "n_reactions",
      "reaction_rate"
    ) %in%
      names(test_reaction_rate)
  ))
  expect_true(all(
    test_reaction_rate$denominator != test_reaction_rate$numerator
  ))
  expect_true(all(test_reaction_rate$n_episodes >= 0L))
  expect_true(all(test_reaction_rate$n_reactions >= 0L))
  expect_true(all(
    test_reaction_rate$reaction_rate >= 0 |
      is.na(test_reaction_rate$reaction_rate)
  ))
  expect_true(all(test_data_delta$denominator != test_data_delta$numerator))
  expect_true(all(test_data_delta$n_episodes >= 0L))
  expect_true(all(test_data_delta$n_reactions >= 0L))
  expect_true(all(
    test_data_delta$reaction_rate >= 0 | is.na(test_data_delta$reaction_rate)
  ))
})
