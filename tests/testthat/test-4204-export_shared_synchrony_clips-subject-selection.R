TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

make_subject_clip_coding <- function() {
  structure(
    list(
      metadata = list(fps = 10L),
      episodes = data.table(
        id = c(1L, 1L, 1L),
        subject = c("parent", "parent", "child"),
        emotion = c("happy", "sad", "happy"),
        start_frame = c(10L, 30L, 50L),
        end_frame = c(19L, 39L, 59L),
        run_id = c(1L, 2L, 3L),
        max_value = c(0.7, 0.9, 0.8)
      )
    ),
    class = c("fr_coding", "list")
  )
}

make_subject_clip_intervals <- function() {
  data.table(
    id = c(1L, 1L, 1L),
    emotion = c("happy", "happy", "sad"),
    subject1 = c("parent", "child", "parent"),
    subject2 = c("child", "parent", "child"),
    subject1_run_id = c(1L, 2L, 3L),
    subject2_run_id = c(1L, 2L, 3L),
    start_frame = c(10L, 20L, 30L),
    end_frame = c(19L, 29L, 39L),
    subject1_max_value = c(0.4, 0.9, 0.8),
    subject2_max_value = c(0.9, 0.2, 0.3),
    combined_value = c(1.3, 1.1, 1.1)
  )
}

test_that("export_shared_synchrony_clips ranks synchronies by one subject", {
  video <- tempfile(fileext = ".mp4")
  file.create(video)

  result <- facereaderconverter:::prepare_shared_synchrony_clips(
    coded_data = make_subject_clip_coding(),
    shared_synchrony = make_subject_clip_intervals(),
    video_paths = c("1" = video),
    n = 0L,
    emotion = NULL,
    optimised_subject = "^parent$"
  )

  expect_equal(result$subject1_run_id, c(3L, 1L, 2L))
  expect_equal(result$selection_value, c(0.8, 0.4, 0.2))
  expect_equal(result$selection_rank, 1:3)
})

test_that("export_shared_synchrony_clips selects individual subject episodes", {
  video <- tempfile(fileext = ".mp4")
  file.create(video)

  result <- facereaderconverter:::prepare_shared_synchrony_clips(
    coded_data = make_subject_clip_coding(),
    shared_synchrony = make_subject_clip_intervals(),
    video_paths = c("1" = video),
    n = 0,
    emotion = NULL,
    optimised_subject = "^parent$",
    only_synchronies = FALSE
  )

  expect_equal(result$subject1, c("parent", "parent"))
  expect_equal(result$subject1_run_id, c(2L, 1L))
  expect_equal(result$selection_value, c(0.9, 0.7))
  expect_identical(all(is.na(result$subject2)), TRUE)
  expect_match(
    result$clip_filename[[1L]],
    "^001_id-1_sad_subject-parent_run-2_frames-30-39\\.mp4$"
  )
})

test_that("export_shared_synchrony_clips ignores episode selection for both", {
  video <- tempfile(fileext = ".mp4")
  file.create(video)

  result <- facereaderconverter:::prepare_shared_synchrony_clips(
    coded_data = make_subject_clip_coding(),
    shared_synchrony = make_subject_clip_intervals(),
    video_paths = c("1" = video),
    n = 1L,
    emotion = NULL,
    optimised_subject = "both",
    only_synchronies = "ignored"
  )

  expect_equal(result$subject1_run_id, 1L)
  expect_equal(result$combined_value, 1.3)
})

test_that("export_shared_synchrony_clips requires one matched subject", {
  video <- tempfile(fileext = ".mp4")
  file.create(video)

  expect_snapshot(error = TRUE, {
    facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = make_subject_clip_coding(),
      shared_synchrony = make_subject_clip_intervals(),
      video_paths = c("1" = video),
      n = 1L,
      emotion = NULL,
      optimised_subject = "guardian"
    )
  })
  expect_snapshot(error = TRUE, {
    facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = make_subject_clip_coding(),
      shared_synchrony = make_subject_clip_intervals(),
      video_paths = c("1" = video),
      n = 1L,
      emotion = NULL,
      optimised_subject = "parent|child"
    )
  })
})
