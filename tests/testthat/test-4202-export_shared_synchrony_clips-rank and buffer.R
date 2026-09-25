TEST_DATA <- Sys.getenv("TEST_DATA")

test_data_path <- file.path(TEST_DATA, "test_data.RDa")
test_data_error <- tryCatch(
  {
    load(test_data_path)
    NULL
  },
  error = identity
)
if (inherits(test_data_error, "error")) {
  testthat::skip(sprintf(
    "Could not load test data fixtures: %s",
    test_data_error$message
  ))
}

library(data.table)
library(testthat)

make_clip_coding <- function() {
  structure(
    list(metadata = list(fps = 10L)),
    class = c("fr_coding", "list")
  )
}

make_clip_intervals <- function() {
  data.table(
    id = c(2L, 1L, 1L),
    emotion = c("sad", "happy", "happy"),
    subject1 = c("parent", "parent", "parent"),
    subject2 = c("teen", "teen", "teen"),
    subject1_run_id = c(1L, 2L, 1L),
    subject2_run_id = c(1L, 2L, 1L),
    start_frame = c(5L, 10L, 20L),
    end_frame = c(8L, 19L, 29L),
    combined_value = c(1.4, 1.9, 1.8)
  )
}

test_that("export_shared_synchrony_clips ranks filtered intervals and buffers frames", {
  video <- tempfile(fileext = ".mp4")
  on.exit(unlink(video, force = TRUE), add = TRUE)
  file.create(video)

  result <- facereaderconverter:::prepare_shared_synchrony_clips(
    coded_data = make_clip_coding(),
    shared_synchrony = make_clip_intervals(),
    video_paths = c("1" = video, "2" = video),
    n = 1L,
    emotion = "happy",
    buffer_frames = 15L,
    buffer_seconds = 0
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$id, 1L)
  expect_equal(result$original_start_frame, 10L)
  expect_equal(result$start_frame, 0L)
  expect_equal(result$end_frame, 34L)
  expect_equal(result$start_seconds, 0)
  expect_equal(result$duration_seconds, 3.5)
  expect_match(
    result$clip_filename,
    "^001_id-1_happy_runs-2-2_frames-0-34\\.mp4$"
  )
})

test_that("export_shared_synchrony_clips caps buffered end at source EOF", {
  video <- tempfile(fileext = ".mp4")
  on.exit(unlink(video, force = TRUE), add = TRUE)
  file.create(video)

  result <- facereaderconverter:::prepare_shared_synchrony_clips(
    coded_data = make_clip_coding(),
    shared_synchrony = data.table(
      id = 1L,
      emotion = "happy",
      subject1 = "parent",
      subject2 = "teen",
      subject1_run_id = 1L,
      subject2_run_id = 1L,
      start_frame = 8L,
      end_frame = 9L,
      combined_value = 1.8
    ),
    video_paths = c("1" = video),
    n = 1L,
    emotion = "happy",
    buffer_frames = 10L,
    video_durations = stats::setNames(2, normalizePath(video, winslash = "/"))
  )

  expect_equal(result$start_frame, 0L)
  expect_equal(result$end_frame, 19L)
  expect_equal(result$duration_seconds, 2)
})

test_that("export_shared_synchrony_clips rejects intervals beyond source EOF", {
  video <- tempfile(fileext = ".mp4")
  on.exit(unlink(video, force = TRUE), add = TRUE)
  file.create(video)

  expect_error(
    facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = make_clip_coding(),
      shared_synchrony = data.table(
        id = 1L,
        emotion = "happy",
        subject1 = "parent",
        subject2 = "teen",
        subject1_run_id = 1L,
        subject2_run_id = 1L,
        start_frame = 20L,
        end_frame = 21L,
        combined_value = 1.8
      ),
      video_paths = c("1" = video),
      n = 1L,
      emotion = "happy",
      buffer = 0,
      video_durations = stats::setNames(
        2,
        normalizePath(video, winslash = "/")
      )
    ),
    "outside the source video duration"
  )
})
