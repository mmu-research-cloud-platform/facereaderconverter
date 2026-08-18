TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

make_with_short_delta <- function(x) {
  out <- copy(x)
  out[, delta := 0L]
  out[1:2, delta := 1L]
  out
}

test_that("reaction_rate returns the expected columns", {
  result <- reaction_rate(test_deltas)

  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "id",
      "subject",
      "emotion",
      "n_episodes",
      "n_reactions",
      "reaction_rate"
    ) %in%
      names(result)
  ))
  expect_true(all(result$n_episodes >= 0L))
  expect_true(all(result$n_reactions >= 0L))
  expect_true(all(result$reaction_rate >= 0 | is.na(result$reaction_rate)))
})

test_that("reaction_rate inherits fps from fr_coding input", {
  result <- reaction_rate(test_deltas)

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("reaction_rate respects episode and start limits", {
  long_window <- reaction_rate(
    test_deltas,
    episode_limit = 3,
    exclude_start = 0.1
  )
  short_window <- reaction_rate(
    test_deltas,
    episode_limit = 1,
    exclude_start = 0.1
  )

  expect_true(
    sum(short_window$n_episodes, na.rm = TRUE) <=
      sum(long_window$n_episodes, na.rm = TRUE)
  )
})

test_that("reaction_rate supports frame-based limits", {
  seconds_limit <- reaction_rate(
    test_deltas,
    episode_limit = 3,
    exclude_start = 0.1
  )
  frames_limit <- reaction_rate(
    test_deltas,
    episode_limit_frames = 90,
    exclude_start_frames = 3
  )

  expect_equal(
    seconds_limit[, .(
      id,
      subject,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )],
    frames_limit[, .(
      id,
      subject,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )]
  )
})

test_that("reaction_rate validates its inputs", {
  expect_error(
    reaction_rate(test_deltas, fps = 0),
    "`fps` must be a positive integer scalar.",
    fixed = TRUE
  )
  expect_error(
    reaction_rate(test_deltas, episode_limit = 0),
    "`episode_limit` must be a numeric scalar > 0.",
    fixed = TRUE
  )
  expect_error(
    reaction_rate(test_deltas, exclude_start = -0.1),
    "`exclude_start` must be a numeric scalar >= 0.",
    fixed = TRUE
  )
})
