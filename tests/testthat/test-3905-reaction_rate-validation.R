TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)
test_data_delta <- test_coding |>
  convert_to_episodes(delta_window = 0.2, delta = 0.1, fps = 30)

test_that("reaction_rate validates its inputs", {
  expect_error(
    reaction_rate(test_data_delta$coding[,
      !names(test_data_delta$coding) %in% "delta"
    ]),
    "`coded_data` is missing required column(s): delta.",
    fixed = TRUE
  )

  expect_error(
    reaction_rate(test_data_delta, time_limit = 0),
    "`time_limit` must be a numeric scalar > 0.",
    fixed = TRUE
  )
  expect_error(
    reaction_rate(test_data_delta, exclude_start = -0.1),
    "`exclude_start` must be a numeric scalar >= 0.",
    fixed = TRUE
  )
})
