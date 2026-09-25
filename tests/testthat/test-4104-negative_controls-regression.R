library(data.table)
library(testthat)

control_coding <- data.table(
  id = rep(1L, 16L),
  subject = rep(c("teen", "parent"), each = 8L),
  emotion = "happy",
  video_time = rep(1:8, 2L),
  value = 1,
  in_state = c(
    rep(FALSE, 8L),
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  ),
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
  expect_setequal(
    names(result),
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "control_run_id",
      "start_frame",
      "end_frame",
      "present_prop",
      "synchrony",
      "source_run_id",
      "control_start_frame",
      "control_end_frame",
      "control_tries_used",
      "control_status"
    )
  )
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
    result[, .(
      id,
      denominator,
      numerator,
      emotion,
      control_run_id,
      start_frame,
      end_frame,
      present_prop,
      synchrony
    )],
    data.table::setnames(data.table::copy(expected), "run_id", "control_run_id")
  )
})


test_that("negative_controls supports custom ID and subject columns", {
  custom_coding <- data.table::copy(control_coding)
  data.table::setnames(custom_coding, c("id", "subject"), c("dyad", "person"))
  custom_data <- structure(
    list(coding = custom_coding, episodes = custom_coding[0]),
    class = c("fr_coding", "list")
  )
  custom_episodes <- data.table::copy(control_episodes)
  data.table::setnames(custom_episodes, c("id", "subject"), c("dyad", "person"))

  set.seed(7349)
  result <- negative_controls(
    custom_data,
    custom_episodes,
    id = "dyad",
    subject = "person",
    exclude_emotions = NULL
  )

  expect_equal(unique(result$control_status), "matched")
})

test_that("negative controls do not reuse frames for an ID", {
  coding <- data.table(
    id = rep(1L, 16L),
    subject = rep(c("teen", "parent"), each = 8L),
    emotion = "happy",
    video_time = rep(1:8, 2L),
    value = 1,
    in_state = FALSE,
    run_id = 1L
  )
  data <- structure(
    list(coding = coding, episodes = coding[0]),
    class = c("fr_coding", "list")
  )
  episodes <- data.table(
    id = 1L,
    subject = "teen",
    emotion = "happy",
    run_id = 1:2,
    start_frame = c(1L, 3L),
    end_frame = c(2L, 4L)
  )

  set.seed(8357L)
  result <- negative_controls(
    data,
    episodes,
    exclude_emotions = NULL,
    max_tries = 100L
  )
  matched <- result[control_status == "matched"]

  expect_equal(nrow(matched), 2L)
  matched[, control_row := .I]
  overlaps <- matched[
    matched,
    on = .(
      id,
      control_start_frame <= control_end_frame,
      control_end_frame >= control_start_frame
    ),
    allow.cartesian = TRUE,
    nomatch = 0L,
    .(
      control_row_x = x.control_row,
      control_row_y = i.control_row,
      start_x = x.control_start_frame,
      end_x = x.control_end_frame,
      start_y = i.control_start_frame,
      end_y = i.control_end_frame
    )
  ][
    control_row_x < control_row_y &
      start_x <= end_y &
      end_x >= start_y
  ]
  expect_equal(nrow(overlaps), 0L)
})

test_that("negative controls report when maximum tries are exceeded", {
  coding <- data.table(
    id = rep(1L, 10L),
    subject = rep(c("teen", "parent"), each = 5L),
    emotion = "happy",
    video_time = rep(1:5, 2L),
    value = 1,
    in_state = FALSE,
    run_id = 1L
  )
  data <- structure(
    list(coding = coding, episodes = coding[0]),
    class = c("fr_coding", "list")
  )
  episodes <- data.table(
    id = 1L,
    subject = "teen",
    emotion = "happy",
    run_id = 1:2,
    start_frame = c(1L, 2L),
    end_frame = c(3L, 4L)
  )

  set.seed(8357L)
  result <- negative_controls(
    data,
    episodes,
    exclude_emotions = NULL,
    max_tries = 2L
  )

  expect_equal(result$control_status, c("unmatched", "unmatched"))
  expect_equal(result$control_tries_used, c(2L, 2L))
  expect_true(all(is.na(result$control_start_frame)))
})
