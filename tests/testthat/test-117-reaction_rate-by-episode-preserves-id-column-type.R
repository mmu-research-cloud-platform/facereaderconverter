TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate_by_episode preserves id column type", {
  test_data_delta <- test_coding |>
    convert_to_episodes() |>
    add_delta_column(delta_window = 0.2, delta = 0.1, fps = 30)

  expect_identical(typeof(test_data_delta$id), typeof(test_coding$id))
})
