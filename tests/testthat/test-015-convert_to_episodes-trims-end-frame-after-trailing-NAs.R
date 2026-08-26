test_that("convert_to_episodes trims end_frame but not end_time after trailing NAs", {
  coding_df <- tibble::tibble(
    id = 1L,
    subject = "parent",
    emotion = "happy",
    frame = 1:6,
    video_time = c(
      "00:00:00.000",
      "00:00:00.033",
      "00:00:00.066",
      "00:00:00.100",
      "00:00:00.133",
      "00:00:00.166"
    ),
    value = c(0, 0.3, 0.4, NA, NA, 0)
  )

  x <- convert_to_episodes(
    coding_df,
    T_up = 0.2,
    T_down = 0.1,
    delta = 1,
    delta_window = 0.1,
    min_dur_sec = 1 / 30,
    consecutive_missing = 10L,
    fps = 30L
  )

  expect_equal(nrow(x$episodes), 1L)
  expect_equal(x$episodes$start_frame[[1]], 2L)
  expect_equal(x$episodes$end_frame[[1]], 3L)
  expect_equal(x$episodes$start_time[[1]], "00:00:00.033")
  expect_equal(x$episodes$end_time[[1]], "00:00:00.133")
  expect_equal(x$episodes$n_frames[[1]], 2L)
  expect_equal(x$episodes$duration_s[[1]], 2 / 30)

  expect_equal(x$coding$in_state, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(
    x$coding$status,
    c(NA_integer_, 1L, 0L, NA_integer_, NA_integer_, NA_integer_)
  )
  expect_equal(
    x$coding$run_id,
    c(NA_integer_, 1L, 1L, NA_integer_, NA_integer_, NA_integer_)
  )
})
