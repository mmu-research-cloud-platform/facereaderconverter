TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

excluded_range_coding <- data.table(
  id = rep(1L, 8L),
  subject = rep(c("teen", "parent"), each = 4L),
  emotion = "happy",
  video_time = rep(1:4, 2L),
  value = 1,
  in_state = FALSE,
  run_id = rep(1:2, each = 4L)
)
excluded_range_data <- structure(
  list(
    coding = excluded_range_coding,
    episodes = data.table(
      id = 1L,
      subject = "teen",
      emotion = c("happy", "neutral"),
      run_id = c(1L, 2L),
      start_frame = c(1L, 2L),
      end_frame = c(3L, 4L)
    )
  ),
  class = c("fr_coding", "list")
)


test_that("mutually exclusive controls avoid excluded emotion episodes", {
  result <- negative_controls(
    excluded_range_data,
    excluded_range_data$episodes,
    mutually_exclusive = TRUE,
    max_tries = 1L
  )

  expect_equal(unique(result$control_status), "unmatched")
})
