TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

control_coding <- data.table(
  id = rep(1L, 16L),
  subject = rep(c("teen", "parent"), each = 8L),
  emotion = "happy",
  video_time = rep(1:8, 2L),
  value = 1,
  in_state = c(rep(FALSE, 8L), FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  run_id = rep(1L, 16L)
)
control_data <- structure(
  list(coding = control_coding, episodes = control_coding[0]),
  class = c("fr_coding", "list")
)
control_episodes <- data.table(
  id = 1L,
  subject = "teen",
  emotion = "happy",
  run_id = 10L,
  start_frame = 2L,
  end_frame = 3L
)

test_that("negative_controls matches interval lengths and calculates synchrony", {
  set.seed(7349)
  result <- negative_controls(
    control_data,
    control_episodes,
    exclude_emotions = NULL
  )

  expect_s3_class(result, "data.table")
  expect_setequal(names(result), c(
    "id", "denominator", "numerator", "emotion", "control_run_id",
    "start_frame", "end_frame", "present_prop", "synchrony", "source_run_id",
    "control_start_frame", "control_end_frame", "control_tries_used",
    "control_status"
  ))
  expect_equal(unique(result$control_status), "matched")
  expect_equal(
    unique(result$control_end_frame - result$control_start_frame + 1L),
    2L
  )
  expected <- synchrony_by_episode(
    control_data,
    episodes = result[, .(
      id,
      subject = denominator,
      emotion,
      run_id = control_run_id,
      start_frame = control_start_frame,
      end_frame = control_end_frame
    )],
    exclude_emotions = NULL
  )
  expect_equal(
    result[, .(id, denominator, numerator, emotion, control_run_id,
      start_frame, end_frame, present_prop, synchrony)],
    data.table::setnames(data.table::copy(expected), "run_id", "control_run_id")
  )
})
