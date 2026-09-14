TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

test_that("shared_synchronous_episodes maps fixture synchronies to episodes", {
  coded_data <- test_coding |>
    convert_to_episodes()
  result <- shared_synchronous_episodes(coded_data)
  episode_synchrony <- synchrony_by_episode(coded_data)[synchrony == TRUE]

  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0L)
  expect_equal(
    anyDuplicated(result[, .(
      id,
      emotion,
      subject1,
      subject2,
      subject1_run_id,
      subject2_run_id
    )]),
    0L
  )
  expect_equal(result$start_frame <= result$end_frame, rep(TRUE, nrow(result)))
  expect_equal(
    result$max_value,
    pmax(result$subject1_max_value, result$subject2_max_value)
  )
  expect_equal(
    result$combined_value,
    result$subject1_max_value + result$subject2_max_value
  )

  valid_as_subject1 <- merge(
    result,
    episode_synchrony[, .(
      id,
      emotion,
      subject1 = denominator,
      subject2 = numerator,
      subject1_run_id = run_id,
      comparison_start_frame = start_frame,
      comparison_end_frame = end_frame
    )],
    by = c("id", "emotion", "subject1", "subject2", "subject1_run_id"),
    allow.cartesian = TRUE
  )[start_frame <= comparison_end_frame & end_frame >= comparison_start_frame]
  valid_as_subject2 <- merge(
    result,
    episode_synchrony[, .(
      id,
      emotion,
      subject1 = numerator,
      subject2 = denominator,
      subject2_run_id = run_id,
      comparison_start_frame = start_frame,
      comparison_end_frame = end_frame
    )],
    by = c("id", "emotion", "subject1", "subject2", "subject2_run_id"),
    allow.cartesian = TRUE
  )[start_frame <= comparison_end_frame & end_frame >= comparison_start_frame]
  expected_as_subject1 <- episode_synchrony[
    denominator < numerator,
    .(
      id,
      emotion,
      subject1 = denominator,
      subject2 = numerator,
      subject1_run_id = run_id
    )
  ]
  expected_as_subject2 <- episode_synchrony[
    denominator > numerator,
    .(
      id,
      emotion,
      subject1 = numerator,
      subject2 = denominator,
      subject2_run_id = run_id
    )
  ]
  mapped_as_subject1 <- unique(valid_as_subject1[, .(
    id,
    emotion,
    subject1,
    subject2,
    subject1_run_id
  )])
  mapped_as_subject2 <- unique(valid_as_subject2[, .(
    id,
    emotion,
    subject1,
    subject2,
    subject2_run_id
  )])

  expect_equal(
    nrow(data.table::fsetdiff(expected_as_subject1, mapped_as_subject1)),
    0L
  )
  expect_equal(
    nrow(data.table::fsetdiff(expected_as_subject2, mapped_as_subject2)),
    0L
  )

  valid_pairs <- unique(rbindlist(
    list(
      valid_as_subject1,
      valid_as_subject2
    ),
    use.names = TRUE
  )[, .(
    id,
    emotion,
    subject1,
    subject2,
    subject1_run_id,
    subject2_run_id
  )])
  result_pairs <- result[, .(
    id,
    emotion,
    subject1,
    subject2,
    subject1_run_id,
    subject2_run_id
  )]
  data.table::setorder(
    valid_pairs,
    id,
    emotion,
    subject1,
    subject2,
    subject1_run_id,
    subject2_run_id
  )
  data.table::setorder(
    result_pairs,
    id,
    emotion,
    subject1,
    subject2,
    subject1_run_id,
    subject2_run_id
  )

  expect_equal(valid_pairs, result_pairs, ignore_attr = TRUE)
})

test_that("shared_synchronous_episodes reverses source values with subject order", {
  coded_data <- test_coding |>
    convert_to_episodes()
  reversed_data <- list(
    coding = data.table::copy(coded_data$coding),
    episodes = data.table::copy(coded_data$episodes)
  )
  class(reversed_data) <- class(coded_data)
  reversed_data$coding[,
    subject := fifelse(subject == "parent", "teen", "parent")
  ]
  reversed_data$episodes[,
    subject := fifelse(subject == "parent", "teen", "parent")
  ]

  result <- shared_synchronous_episodes(coded_data)
  reversed_result <- shared_synchronous_episodes(reversed_data)
  data.table::setorder(
    result,
    id,
    emotion,
    subject1,
    subject2,
    start_frame,
    end_frame,
    subject1_run_id,
    subject2_run_id
  )
  data.table::setorder(
    reversed_result,
    id,
    emotion,
    subject1,
    subject2,
    start_frame,
    end_frame,
    subject1_run_id,
    subject2_run_id
  )

  expect_equal(
    reversed_result[, .(
      id,
      emotion,
      subject1,
      subject2,
      subject1_run_id,
      subject2_run_id,
      start_frame,
      end_frame,
      subject1_max_value,
      subject2_max_value,
      max_value,
      combined_value
    )],
    result[, .(
      id,
      emotion,
      subject1,
      subject2,
      subject2_run_id,
      subject1_run_id,
      start_frame,
      end_frame,
      subject2_max_value,
      subject1_max_value,
      max_value,
      combined_value
    )],
    ignore_attr = TRUE
  )
})
