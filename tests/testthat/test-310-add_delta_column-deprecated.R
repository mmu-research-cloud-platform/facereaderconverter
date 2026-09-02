TEST_DATA <- Sys.getenv("TEST_DATA")

load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)

test_that("add_delta_column is deprecated and still computes delta", {
  expect_snapshot({
    result <- add_delta_column(
      test_coding,
      delta_window = 0.1,
      delta = 0.1,
      fps = 30
    )
  })

  expect_true("delta" %in% names(result))
  expect_true(all(c(0, 1) %in% result$delta, na.rm = TRUE))
})

test_that("add_delta_column overwrites an existing delta column", {
  result <- suppressWarnings(add_delta_column(
    test_coding |> dplyr::mutate(delta = 999L),
    delta_window = 0.1,
    delta = 0.1,
    fps = 30
  ))

  expect_true("delta" %in% names(result))
  expect_false(any(result$delta == 999L, na.rm = TRUE))
})

test_that("add_delta_column overwrites convert_to_episodes delta", {
  converted <- convert_to_episodes(test_coding, delta = 0.1, delta_window = 0.2)
  original_delta <- converted$coding$delta

  result <- suppressWarnings(add_delta_column(
    converted,
    delta = 0.4,
    delta_window = 0.2,
    fps = 30
  ))

  expect_false(identical(original_delta, result$delta))
})
