TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate uses fr_coding deltas as the reaction source", {
  converted <- convert_to_episodes(
    test_coding,
    delta_window = 0.2,
    delta = 0.1,
    fps = 30
  )

  baseline <- reaction_rate(converted, exclude_emotions = NULL)

  converted$deltas <- converted$deltas[0]
  no_reactions <- reaction_rate(converted, exclude_emotions = NULL)

  expect_equal(no_reactions$n_reactions, rep.int(0L, nrow(no_reactions)))
  expect_true(any(baseline$n_reactions > no_reactions$n_reactions))
})
