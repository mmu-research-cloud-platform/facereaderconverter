TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)

test_that("reaction_rate_by_episode requires delta in coded_data$coding", {
  test_data_reaction <- test_coding |>
    convert_to_episodes() |>
    add_delta_column(
      delta_window = 0.2,
      delta = 0.1,
      fps = 30,
    ) |>
    dplyr::select(-delta)

  expect_error(
    reaction_rate_by_episode(test_data_reaction),
    "`coded_data` is missing required column(s): delta",
    fixed = TRUE
  )
})
