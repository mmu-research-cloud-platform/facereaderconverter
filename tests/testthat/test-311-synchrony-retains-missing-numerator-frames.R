library(testthat)
library(data.table)

test_that("synchrony counts missing numerator frames in present_prop", {
  coding <- data.table(
    id = 1L,
    subject = c("denominator", "denominator", "numerator"),
    emotion = "happy",
    video_time = c(1, 2, 1),
    value = c(0.3, 0.4, 0.2),
    in_state = c(TRUE, TRUE, FALSE),
    run_id = c(1L, 1L, NA_integer_)
  )
  episodes <- data.table(
    id = 1L,
    subject = "denominator",
    emotion = "happy",
    run_id = 1L,
    start_frame = 1L,
    end_frame = 2L
  )
  coded_data <- structure(
    list(coding = coding, episodes = episodes),
    class = c("fr_coding", "list")
  )

  result <- synchrony_by_episode(coded_data, exclude_emotions = NULL)

  expect_equal(result$present_prop, 0.5)
  expect_identical(result$denominator, "denominator")
  expect_identical(result$numerator, "numerator")
})
