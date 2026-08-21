TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("synchrony_by_episode returns episode-level columns", {
  result <- synchrony_by_episode(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0
  )

  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "run_id",
      "present_prop",
      "synchrony"
    ) %in%
      names(result)
  ))
  expect_true(is.logical(result$synchrony))
  expect_true(is.numeric(result$present_prop))
  expect_true(all(result$present_prop >= 0 & result$present_prop <= 1))
  expect_true(all(result$denominator != result$numerator))
  expect_false(any(result$emotion == "neutral"))
  expect_true(all(result$run_id %in% test_data_sync$episodes$run_id))
})

test_that("synchrony_by_episode matches synchrony after aggregation", {
  episode_result <- synchrony_by_episode(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0
  )

  summary_from_episode <- episode_result[,
    .(
      n_episodes = .N,
      numerator_count = sum(synchrony)
    ),
    by = .(id, denominator, numerator, emotion)
  ]

  summary_from_episode[,
    synchrony := ifelse(n_episodes > 0L, numerator_count / n_episodes, NA_real_)
  ]
  summary_from_episode[, `:=`(
    n_episodes = as.integer(n_episodes),
    synchrony = as.numeric(synchrony)
  )]

  control <- merge(
    unique(episode_result[, .(id, denominator, numerator)]),
    unique(episode_result[, .(id, denominator, emotion)]),
    by = c("id", "denominator"),
    allow.cartesian = TRUE
  )
  control <- merge(
    control,
    summary_from_episode,
    by = c("id", "denominator", "numerator", "emotion"),
    all.x = TRUE
  )
  control <- control[, .(
    id,
    denominator,
    numerator,
    emotion,
    n_episodes,
    synchrony
  )]
  data.table::setorder(control, id, denominator, numerator, emotion)

  expect_equal(
    synchrony(
      test_data_sync,
      subject = "subject",
      id = "id",
      missing_threshold = 0
    ),
    control,
    ignore_attr = TRUE
  )
})

test_that("synchrony_by_episode preserves id column type", {
  result <- synchrony_by_episode(test_data_sync, missing_threshold = 0)

  expect_identical(typeof(result$id), typeof(test_data_sync$coding$id))
})
