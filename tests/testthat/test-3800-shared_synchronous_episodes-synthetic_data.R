TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

make_shared_episode_data <- function() {
  coding <- data.table(
    id = 1L,
    subject = c(rep("parent", 6L), rep("teen", 6L)),
    emotion = "happy",
    video_time = rep(1:6, 2L),
    value = c(rep(0.4, 6L), 0.3, 0.6, 0.5, 0.2, 0.8, 0.7),
    in_state = c(rep(TRUE, 6L), FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    run_id = c(rep(1L, 6L), NA_integer_, 2L, 2L, NA_integer_, 3L, 3L)
  )
  episodes <- data.table(
    id = 1L,
    subject = c("parent", "teen", "teen"),
    emotion = "happy",
    run_id = c(1L, 2L, 3L),
    start_frame = c(1L, 2L, 5L),
    end_frame = c(6L, 3L, 6L),
    max_value = c(0.4, 0.6, 0.8)
  )

  structure(
    list(coding = coding, episodes = episodes),
    class = c("fr_coding", "list")
  )
}

test_that("shared_synchronous_episodes maps every overlapping episode pair", {
  result <- shared_synchronous_episodes(
    make_shared_episode_data(),
    exclude_emotions = NULL
  )

  expect_s3_class(result, "data.table")
  expect_named(
    result,
    c(
      "id",
      "emotion",
      "subject1",
      "subject2",
      "subject1_run_id",
      "subject2_run_id",
      "start_frame",
      "end_frame",
      "subject1_max_value",
      "subject2_max_value",
      "max_value",
      "combined_value"
    )
  )
  expect_equal(nrow(result), 2L)
  expect_equal(result$subject1, c("parent", "parent"))
  expect_equal(result$subject2, c("teen", "teen"))
  expect_equal(result$subject1_run_id, c(1L, 1L))
  expect_equal(result$subject2_run_id, c(2L, 3L))
  expect_equal(result$start_frame, c(2L, 5L))
  expect_equal(result$end_frame, c(3L, 6L))
  expect_equal(result$subject1_max_value, c(0.4, 0.4))
  expect_equal(result$subject2_max_value, c(0.6, 0.8))
  expect_equal(result$max_value, c(0.6, 0.8))
  expect_equal(result$combined_value, c(1, 1.2))
  expect_equal(
    uniqueN(result[, .(subject1_run_id, subject2_run_id)]),
    nrow(result)
  )
})

test_that("shared_synchronous_episodes is invariant to source row order", {
  coded_data <- make_shared_episode_data()
  reversed_data <- structure(
    list(
      coding = coded_data$coding[.N:1L],
      episodes = coded_data$episodes[.N:1L]
    ),
    class = class(coded_data)
  )

  expect_identical(
    shared_synchronous_episodes(coded_data, exclude_emotions = NULL),
    shared_synchronous_episodes(reversed_data, exclude_emotions = NULL)
  )
})

test_that("shared_synchronous_episodes supports custom ID and subject columns", {
  coded_data <- make_shared_episode_data()
  setnames(coded_data$coding, c("id", "subject"), c("dyad", "person"))
  setnames(coded_data$episodes, c("id", "subject"), c("dyad", "person"))

  result <- shared_synchronous_episodes(
    coded_data,
    id = "dyad",
    subject = "person",
    exclude_emotions = NULL
  )

  expect_equal(nrow(result), 2L)
  expect_identical(typeof(result$id), "integer")
})

test_that("shared_synchronous_episodes requires source episode maxima", {
  coded_data <- make_shared_episode_data()
  coded_data$episodes[, max_value := NULL]

  expect_snapshot(
    shared_synchronous_episodes(coded_data, exclude_emotions = NULL),
    error = TRUE
  )
})
