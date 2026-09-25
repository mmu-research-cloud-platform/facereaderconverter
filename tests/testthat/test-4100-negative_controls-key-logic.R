TEST_DATA <- Sys.getenv("TEST_DATA")

test_data_path <- file.path(TEST_DATA, "test_data.RDa")
test_data_error <- tryCatch(
  {
    load(test_data_path)
    NULL
  },
  error = identity
)
if (inherits(test_data_error, "error")) {
  testthat::skip(sprintf(
    "Could not load test data fixtures: %s",
    test_data_error$message
  ))
}

library(data.table)
library(testthat)

test_that("negative_controls excludes supplied neutral fixture ranges", {
  source_episode <- test_data_sync2$episodes[emotion == "happy"][1]
  neutral_episode <- test_data_sync2$episodes[
    id == source_episode$id & emotion == "neutral"
  ][1]
  episodes <- rbind(source_episode, neutral_episode)

  set.seed(8357L)
  result <- negative_controls(
    test_data_sync2,
    episodes,
    mutually_exclusive = TRUE,
    max_tries = 100L
  )

  overlapping_ranges <- episodes[
    id == result$id &
      start_frame <= result$control_end_frame &
      end_frame >= result$control_start_frame
  ]

  expect_equal(result$control_status, "matched")
  expect_equal(nrow(overlapping_ranges), 0L)
  expect_equal(
    result$control_end_frame - result$control_start_frame,
    source_episode$end_frame - source_episode$start_frame
  )
})

test_that("negative_controls returns zero synchrony for fixture control", {
  source_episode <- test_data_sync2$episodes[emotion == "happy"][1]
  neutral_episode <- test_data_sync2$episodes[
    id == source_episode$id & emotion == "neutral"
  ][1]

  set.seed(8357L)
  result <- negative_controls(
    test_data_sync2,
    rbind(source_episode, neutral_episode),
    mutually_exclusive = TRUE,
    max_tries = 100L
  )

  expect_equal(as.integer(result$synchrony), 0L)
})

test_that("all negative controls have zero synchrony", {
  set.seed(8357L)
  controls <- negative_controls(
    test_data_sync2,
    test_data_sync2$episodes,
    mutually_exclusive = TRUE
  )
  control_episodes <- controls[
    control_status == "matched",
    .(
      id,
      subject = denominator,
      emotion,
      run_id = control_run_id,
      start_frame = control_start_frame,
      end_frame = control_end_frame
    )
  ]

  result <- synchrony(test_data_sync2, episodes = control_episodes)

  expect_gt(nrow(control_episodes), 0L)
  expect_equal(unique(result$synchrony), 0)
})

test_that("negative controls have some synchrony when not mutually exclusive", {
  set.seed(8357L)
  controls <- negative_controls(
    test_data_sync2,
    test_data_sync2$episodes,
    mutually_exclusive = FALSE
  )
  control_episodes <- controls[
    control_status == "matched",
    .(
      id,
      subject = denominator,
      emotion,
      run_id = control_run_id,
      start_frame = control_start_frame,
      end_frame = control_end_frame
    )
  ]

  result <- synchrony(
    test_data_sync2,
    episodes = control_episodes,
    missing_threshold = 0
  ) |>
    mutate(syncs = synchrony * n_episodes)
  result_pos <- synchrony(test_data_sync2, missing_threshold = 0) |>
    mutate(syncs = synchrony * n_episodes)
  expect_gt(nrow(control_episodes), 0L)

  comparison <- merge(
    result[, .(id, emotion, denominator, numerator, syncs_neg = syncs)],
    result_pos[, .(id, emotion, denominator, numerator, syncs_pos = syncs)],
    by = c("id", "emotion", "denominator", "numerator")
  )
  expect_gt(nrow(comparison), 0L)
  expect_true(all(comparison$syncs_neg <= comparison$syncs_pos))
})
