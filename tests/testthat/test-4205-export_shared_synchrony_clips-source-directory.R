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

test_that("folder output cannot contain a selected source video", {
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")

  source_dir <- tempfile("shared-synchrony-source-")
  dir.create(source_dir)
  video <- file.path(source_dir, "source.mp4")
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
    export_shared_synchrony_clips(
      coding,
      intervals,
      video_paths = c("1" = video),
      n = 1L,
      output = "folder",
      output_path = source_dir,
      overwrite = TRUE,
      ffmpeg = Sys.which("ffmpeg")
    ),
    "cannot contain a selected source video"
  )
  expect_true(file.exists(video))
})
