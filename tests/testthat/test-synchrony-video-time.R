library(testthat)
library(data.table)

test_that("synchrony handles character video_time values", {
  coding <- data.table(
    id = rep(1L, 6),
    subject = rep(c("teen", "parent"), each = 3),
    emotion = "happy",
    video_time = rep(c("00:00:00.000", "00:00:00.033", "00:00:00.066"), 2),
    value = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
    in_state = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
    run_id = c(1L, 1L, 1L, 2L, 2L, 2L)
  )

  episodes <- data.table(
    id = 1L,
    subject = c("teen", "parent"),
    emotion = "happy",
    run_id = c(1L, 2L),
    start_frame = c(2L, 3L),
    end_frame = c(2L, 3L)
  )

  coded_data <- structure(
    list(coding = coding, episodes = episodes),
    class = c("fr_coding", "list")
  )

  result <- synchrony(coded_data, missing_threshold = 0)
  setorder(result, id, denominator, numerator, emotion)

  expected <- data.table(
    id = c(1L, 1L),
    denominator = c("parent", "teen"),
    numerator = c("teen", "parent"),
    emotion = c("happy", "happy"),
    n_episodes = c(1L, 1L),
    synchrony = c(0, 0)
  )
  setorder(expected, id, denominator, numerator, emotion)

  expect_equal(result, expected, ignore_attr = TRUE)
})
