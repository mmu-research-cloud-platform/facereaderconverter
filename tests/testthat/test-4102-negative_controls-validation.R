TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

validation_coding <- data.table(
  id = rep(1L, 4L),
  subject = rep(c("teen", "parent"), each = 2L),
  emotion = "happy",
  video_time = rep(1:2, 2L),
  value = 1,
  in_state = FALSE,
  run_id = 1L
)
validation_data <- structure(
  list(coding = validation_coding, episodes = validation_coding[0]),
  class = c("fr_coding", "list")
)
validation_episodes <- data.table(
  id = 1L,
  subject = "teen",
  emotion = "happy",
  run_id = 1L,
  start_frame = 1L,
  end_frame = 1L
)

test_that("negative_controls validates controls and range bounds", {
  expect_snapshot(error = TRUE, {
    negative_controls(
      validation_data,
      validation_episodes,
      mutually_exclusive = NA
    )
  })
  expect_snapshot(error = TRUE, {
    negative_controls(validation_data, validation_episodes, max_tries = 0L)
  })
  expect_error(
    negative_controls(
      validation_data,
      validation_episodes[,
        setdiff(names(validation_episodes), "end_frame"),
        with = FALSE
      ]
    ),
    "missing required columns"
  )
  reversed_episodes <- data.table::copy(validation_episodes)
  reversed_episodes[, `:=`(start_frame = 2L, end_frame = 1L)]
  expect_error(
    negative_controls(validation_data, reversed_episodes),
    "inclusive frame bounds"
  )
})

test_that("negative_controls accepts synchrony_by_episode output", {
  synchrony_episodes <- synchrony_by_episode(
    validation_data,
    episodes = validation_episodes,
    exclude_emotions = NULL
  )

  result <- negative_controls(
    validation_data,
    synchrony_episodes,
    exclude_emotions = NULL
  )

  expect_equal(unique(result$control_status), "matched")
})
