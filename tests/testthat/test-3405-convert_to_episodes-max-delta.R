library(testthat)
library(data.table)

test_that("convert_to_episodes calculates max_delta over the expanded event window", {
  coding <- data.table(
    id = 1L,
    subject = "participant",
    emotion = "happy",
    frame = 1:6,
    value = c(0, 0.05, 0.2, 0.25, 0.4, 0.45)
  )

  converted <- convert_to_episodes(
    coding,
    delta = 0.1,
    delta_window = 2,
    fps = 1L
  )

  expect_true("max_delta" %in% names(converted$deltas))
  expect_equal(converted$deltas$start_frame, 1L)
  expect_equal(converted$deltas$end_frame, 6L)
  expect_equal(converted$deltas$max_delta, 0.45)
  expect_true(all(converted$deltas$max_delta >= 0.1))
})

test_that("convert_to_episodes calculates max_delta independently by group", {
  coding <- data.table(
    id = rep(1:2, each = 6),
    subject = "participant",
    emotion = "happy",
    frame = rep(1:6, 2),
    value = c(
      0,
      NA,
      0.2,
      0.25,
      0.4,
      0.45,
      0.9,
      0.9,
      0.2,
      0.25,
      0.4,
      0.5
    )
  )

  converted <- convert_to_episodes(
    coding,
    delta = 0.1,
    delta_window = 2,
    fps = 1L
  )

  expect_equal(converted$deltas[id == 1L, max_delta], c(0.2, 0.45))
  expect_equal(converted$deltas[id == 2L, max_delta], 0.7)
  expect_true(all(converted$deltas$max_delta >= 0.1))
})

test_that("convert_to_episodes uses source positions for duplicated frames", {
  coding <- data.table(
    id = 1L,
    subject = "participant",
    emotion = "happy",
    frame = c(1L, 1L, 2L, 3L, 4L),
    value = c(0, 0.9, 0.2, 0.3, 0.4)
  )

  converted <- convert_to_episodes(
    coding,
    delta = 0.1,
    delta_window = 1,
    fps = 1L
  )

  expect_equal(converted$deltas$max_delta, 0.2)
})
