TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("synchrony internals rename columns and build episode tables", {
  coding <- data.table(
    pair_id = rep(1L, 6),
    person = rep(c("teen", "parent"), each = 3),
    emotion = "happy",
    video_time = rep(1:3, 2),
    value = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
    in_state = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
    run_id = c(1L, 1L, 1L, 2L, 2L, 2L)
  )
  episodes <- data.table(
    pair_id = c(1L, 1L),
    person = c("teen", "parent"),
    emotion = "happy",
    run_id = c(1L, 2L),
    start_frame = c(2L, 3L),
    end_frame = c(2L, 3L)
  )
  coded_data <- structure(
    list(coding = coding, episodes = episodes),
    class = c("fr_coding", "list")
  )

  inputs <- facereaderconverter:::prepare_synchrony_inputs(
    coded_data,
    subject = "person",
    id = "pair_id",
    missing_threshold = 0
  )

  expect_s3_class(inputs$coding, "data.table")
  expect_s3_class(inputs$episodes, "data.table")
  expect_true(all(
    c(
      "id",
      "subject",
      "emotion",
      "video_time",
      "value",
      "in_state",
      "run_id"
    ) %in%
      names(inputs$coding)
  ))
  expect_true(all(
    c("id", "subject", "emotion", "run_id", "start_frame", "end_frame") %in%
      names(inputs$episodes)
  ))
  expect_identical(inputs$missing_threshold, 0)

  episode_table <- facereaderconverter:::build_synchrony_episode_table(inputs)

  expect_s3_class(episode_table, "data.table")
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
      names(episode_table)
  ))
  expect_true(all(episode_table$denominator != episode_table$numerator))
  expect_true(all(
    episode_table$present_prop >= 0 & episode_table$present_prop <= 1
  ))
  expect_type(episode_table$synchrony, "logical")
})
