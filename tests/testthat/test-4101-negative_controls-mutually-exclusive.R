TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

exclusive_coding <- data.table(
  id = rep(1L, 8L),
  subject = rep(c("teen", "parent"), each = 4L),
  emotion = "happy",
  video_time = rep(1:4, 2L),
  value = 1,
  in_state = FALSE,
  run_id = 1L
)
exclusive_data <- structure(
  list(coding = exclusive_coding, episodes = exclusive_coding[0]),
  class = c("fr_coding", "list")
)

test_that("negative_controls permits overlap by default", {
  episodes <- data.table(
    id = 1L,
    subject = "teen",
    emotion = "happy",
    run_id = 1L,
    start_frame = 1L,
    end_frame = 4L
  )

  result <- negative_controls(
    exclusive_data,
    episodes,
    exclude_emotions = NULL,
    max_tries = 1L
  )

  expect_equal(unique(result$control_status), "matched")
  expect_equal(result$control_start_frame, 1L)
  expect_equal(result$control_end_frame, 4L)
})

test_that("negative_controls rejects overlapping controls when requested", {
  episodes <- data.table(
    id = 1L,
    subject = "teen",
    emotion = "happy",
    run_id = 1L,
    start_frame = 1L,
    end_frame = 4L
  )

  result <- negative_controls(
    exclusive_data,
    episodes,
    mutually_exclusive = TRUE,
    exclude_emotions = NULL,
    max_tries = 2L
  )

  expect_equal(unique(result$control_status), "unmatched")
  expect_true(is.na(result$control_start_frame))
})
