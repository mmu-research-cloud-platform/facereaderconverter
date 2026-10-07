TEST_DATA <- Sys.getenv("TEST_DATA")

library(data.table)
library(testthat)

test_that("parallel clip export preserves manifest order and matches serial output", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  video <- file.path(brazil_dir, "ID100024_side_by_side.mp4")
  skip_if_not(file.exists(video))
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is unavailable")
  skip_if(Sys.which("ffprobe") == "", "FFprobe is unavailable")

  coded_data <- structure(
    list(metadata = list(fps = 30L)),
    class = c("fr_coding", "list")
  )
  shared <- data.table(
    id = rep(1L, 2L),
    emotion = rep("happy", 2L),
    subject1 = rep("mum", 2L),
    subject2 = rep("child", 2L),
    subject1_run_id = 1:2,
    subject2_run_id = 1:2,
    start_frame = c(60L, 120L),
    end_frame = c(89L, 149L),
    combined_value = c(1, 2)
  )
  output_dir <- tempfile("parallel-clips-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  serial <- export_shared_synchrony_clips(
    coded_data,
    shared,
    c("1" = video),
    n = 2L,
    buffer = 0,
    output = "folder",
    output_path = file.path(output_dir, "serial"),
    cores = 1L
  )
  parallel <- export_shared_synchrony_clips(
    coded_data,
    shared,
    c("1" = video),
    n = 2L,
    buffer = 0,
    output = "folder",
    output_path = file.path(output_dir, "parallel"),
    cores = 2L
  )
  expect_equal(parallel$clip_filename, serial$clip_filename)
  expect_equal(parallel$selection_rank, serial$selection_rank)
  expect_equal(parallel$clip_order, serial$clip_order)
  expect_equal(length(parallel$clip_path), 2L)
  expect_equal(file.exists(parallel$clip_path), rep(TRUE, 2L))
  expect_equal(file.info(parallel$clip_path)$size > 0, rep(TRUE, 2L))
  expect_equal(
    data.table::fread(file.path(
      output_dir,
      "parallel",
      "manifest.csv"
    ))$clip_filename,
    parallel$clip_filename
  )
})

test_that("clip export validates core counts", {
  coded_data <- structure(
    list(metadata = list(fps = 30L)),
    class = c("fr_coding", "list")
  )
  video <- tempfile(fileext = ".mp4")
  on.exit(unlink(video), add = TRUE)
  file.create(video)
  shared <- data.table(
    id = 1L,
    emotion = "happy",
    subject1 = "mum",
    subject2 = "child",
    subject1_run_id = 1L,
    subject2_run_id = 1L,
    start_frame = 0L,
    end_frame = 1L,
    combined_value = 1
  )
  expect_snapshot(
    error = TRUE,
    export_shared_synchrony_clips(
      coded_data,
      shared,
      c("1" = video),
      cores = -1L
    )
  )
})
