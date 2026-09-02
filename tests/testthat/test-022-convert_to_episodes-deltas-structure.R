TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("convert_to_episodes returns deltas and delta_id links", {
  converted <- convert_to_episodes(
    test_coding,
    delta_window = 0.2,
    delta = 0.1,
    fps = 30
  )

  expect_true("deltas" %in% names(converted))
  expect_true("delta_id" %in% names(converted$coding))
  expect_true(all(
    c(
      "id",
      "subject",
      "emotion",
      "start_frame",
      "end_frame",
      "start_time",
      "end_time",
      "duration_s",
      "delta_id",
      "n_frames"
    ) %in%
      names(converted$deltas)
  ))

  expect_equal(
    converted$coding[delta == 1L, sum(!is.na(delta_id))],
    converted$coding[delta == 1L, .N]
  )
  expect_equal(
    converted$coding[delta != 1L | is.na(delta), sum(!is.na(delta_id))],
    0L
  )
  expect_equal(anyDuplicated(converted$deltas$delta_id), 0L)
  expect_equal(
    converted$deltas[, all(n_frames == end_frame - start_frame + 1L)],
    TRUE
  )
  expect_equal(
    converted$deltas[, all(duration_s == n_frames / 30)],
    TRUE
  )

  linked <- converted$coding[
    !is.na(delta_id),
    .(
      min_frame = min(frame),
      max_frame = max(frame),
      n_frames_coding = .N,
      delta_values = list(unique(delta))
    ),
    by = .(id, subject, emotion, delta_id)
  ]

  comparison <- merge(
    converted$deltas,
    linked,
    by = c("id", "subject", "emotion", "delta_id"),
    all = TRUE,
    sort = FALSE
  )

  expect_equal(nrow(comparison), nrow(converted$deltas))
  expect_equal(comparison$start_frame, comparison$min_frame)
  expect_equal(comparison$end_frame, comparison$max_frame)
  expect_equal(comparison$n_frames, comparison$n_frames_coding)
  expect_equal(unique(unlist(comparison$delta_values)), 1L)
})
