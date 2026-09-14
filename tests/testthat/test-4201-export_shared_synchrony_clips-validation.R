TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)
library(testthat)

test_that("export_shared_synchrony_clips validates selection inputs", {
  video <- tempfile(fileext = ".mp4")
  file.create(video)
  coding <- structure(
    list(metadata = list(fps = 10L)),
    class = c("fr_coding", "list")
  )
  intervals <- data.table(
    id = 1L,
    emotion = "happy",
    subject1 = "parent",
    subject2 = "teen",
    subject1_run_id = 1L,
    subject2_run_id = 1L,
    start_frame = 10L,
    end_frame = 19L,
    combined_value = 1.8
  )

  expect_error(
    facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = coding,
      shared_synchrony = intervals,
      video_paths = c("1" = video),
      n = 1L,
      emotion = NULL,
      buffer_frames = 1L,
      buffer_seconds = 0.1
    ),
    "either `buffer_frames` or `buffer_seconds`"
  )
  expect_error(
    facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = coding,
      shared_synchrony = intervals,
      video_paths = c("2" = video),
      n = 1L,
      emotion = NULL,
      buffer = 0
    ),
    "missing one or more selected IDs"
  )
  expect_error(
    export_shared_synchrony_clips(
      coding,
      intervals,
      c("1" = video),
      n = 1L,
      ffmpeg = "not-an-ffmpeg-command"
    ),
    "https://ffmpeg.org/download.html"
  )
})
