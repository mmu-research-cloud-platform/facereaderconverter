TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)

test_that("reaction_rate_by_episode requires delta in coded_data$coding", {
  test_data_reaction <- convert_to_episodes(
    test_coding,
    delta_window = 0.2,
    delta = 0.1,
    fps = 30
  )
  test_data_reaction$coding <-
    test_data_reaction$coding[, !names(test_data_reaction$coding) %in% "delta"]

  expect_error(
    reaction_rate_by_episode(test_data_reaction),
    "`coded_data$coding` is missing required columns: delta.",
    fixed = TRUE
  )
})

test_that("reaction_rate_by_episode requires deltas in fr_coding input", {
  test_data_reaction <- convert_to_episodes(
    test_coding,
    delta_window = 0.2,
    delta = 0.1,
    fps = 30
  )
  test_data_reaction$deltas <-
    test_data_reaction$deltas[,
      setdiff(names(test_data_reaction$deltas), "delta_id"),
      with = FALSE
    ]

  expect_error(
    reaction_rate_by_episode(test_data_reaction),
    "`coded_data$deltas` is missing required columns: delta_id.",
    fixed = TRUE
  )
})
