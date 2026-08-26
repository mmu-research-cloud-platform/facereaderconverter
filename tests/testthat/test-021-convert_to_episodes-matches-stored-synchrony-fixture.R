TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

make_multi_subject <- function(x) {
  out <- copy(x)
  sibling_coding <- out$coding[id == 1]
  sibling_coding[, subject := "sibling"]

  sibling_episodes <- out$episodes[id == 1]
  sibling_episodes[, subject := "sibling"]

  out$coding <- rbind(out$coding, sibling_coding, fill = TRUE)
  out$episodes <- rbind(out$episodes, sibling_episodes, fill = TRUE)
  out
}

make_singleton_id <- function(x) {
  out <- copy(x)
  out$coding <- out$coding[!(id == 3 & subject == "parent")]
  out
}

sort_coded_data <- function(x) {
  out <- copy(x)
  data.table::setorder(out$coding, id, subject, emotion, video_time, run_id)
  data.table::setorder(
    out$episodes,
    id,
    subject,
    emotion,
    start_frame,
    end_frame,
    run_id
  )
  out
}

test_that("convert_to_episodes matches the stored synchrony fixture", {
  converted <- sort_coded_data(
    convert_to_episodes(
      test_coding,
      fps = 30L,
      delta_window = 0.1,
      T_up = 0.15,
      T_down = 0.05,
      delta = 1,
      min_dur_sec = 0.1,
      consecutive_missing = 90L
    )
  )
  expected <- sort_coded_data(test_data_sync)

  expect_equal(
    converted$episodes |>
      dplyr::select(
        id,
        subject,
        emotion,
        start_frame,
        end_frame,
        duration_s,
        n_frames
      ),
    expected$episodes |>
      dplyr::select(
        id,
        subject,
        emotion,
        start_frame,
        end_frame,
        duration_s,
        n_frames
      )
  )
  expect_equal(converted$episodes, expected$episodes)
  expect_equal(converted, expected)
})
