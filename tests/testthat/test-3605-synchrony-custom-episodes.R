TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

custom_coding <- data.table(
  id = rep(1L, 12L),
  subject = rep(c("teen", "parent"), each = 6L),
  emotion = "happy",
  video_time = rep(1:6, 2L),
  value = 1,
  in_state = c(rep(FALSE, 6L), FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  run_id = 1L
)
custom_episodes <- data.table(
  id = 1L,
  subject = "teen",
  emotion = "happy",
  run_id = 8L,
  start_frame = 2L,
  end_frame = 3L
)
custom_data <- structure(
  list(coding = custom_coding, episodes = custom_episodes),
  class = c("fr_coding", "list")
)

test_that("synchrony_by_episode uses supplied inclusive episode ranges", {
  result <- synchrony_by_episode(
    custom_data,
    episodes = custom_episodes,
    exclude_emotions = NULL
  )

  expect_equal(result$start_frame, 2L)
  expect_equal(result$end_frame, 3L)
  expect_equal(result$synchrony, TRUE)
  expect_equal(result$present_prop, 1)
})

test_that("synchrony accepts synchrony_by_episode output as episodes", {
  episode_result <- synchrony_by_episode(
    custom_data,
    episodes = custom_episodes,
    exclude_emotions = NULL
  )

  result <- synchrony_by_episode(
    custom_data,
    episodes = episode_result,
    exclude_emotions = NULL
  )

  expect_equal(result, episode_result)
})

test_that("synchrony validates malformed supplied episode ranges", {
  expect_error(
    synchrony_by_episode(
      custom_data,
      episodes = custom_episodes[,
        setdiff(names(custom_episodes), "end_frame"),
        with = FALSE
      ],
      exclude_emotions = NULL
    ),
    "missing required columns"
  )
  non_integer_episodes <- data.table::copy(custom_episodes)
  non_integer_episodes[, start_frame := 2.5]
  expect_error(
    synchrony_by_episode(
      custom_data,
      episodes = non_integer_episodes,
      exclude_emotions = NULL
    ),
    "inclusive frame bounds"
  )
})
