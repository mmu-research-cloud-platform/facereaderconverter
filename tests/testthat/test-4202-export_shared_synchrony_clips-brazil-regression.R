TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)

test_that("export_shared_synchrony_clips exports Brazil fixture intervals", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  video_files <- list.files(
    brazil_dir,
    pattern = "\\.(mp4|mov|avi|mkv|webm)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  coding_files <- list.files(
    brazil_dir,
    pattern = "\\.(csv|txt|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(length(video_files) != 1L, "Brazil fixture must contain one video.")
  output <- file.path(
    dirname(video_files[[1L]]),
    paste0(
      tools::file_path_sans_ext(basename(video_files[[1L]])),
      "-shared-synchrony-clips.zip"
    )
  )
  output_dir <- file.path(
    dirname(video_files[[1L]]),
    paste0(
      tools::file_path_sans_ext(basename(video_files[[1L]])),
      "-shared-synchrony-clips"
    )
  )
  unlink(c(output, output_dir), recursive = TRUE, force = TRUE)
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two FaceReader outputs."
  )
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")

  emotions <- c(
    "neutral",
    "happy",
    "sad",
    "angry",
    "surprised",
    "scared",
    "disgusted"
  )
  coding <- dplyr::bind_rows(lapply(coding_files, function(path) {
    loadFRfile(path) |>
      dplyr::transmute(
        id = 1L,
        subject = tools::file_path_sans_ext(basename(path)),
        video_time,
        dplyr::across(dplyr::all_of(emotions))
      )
  }))
  coded_data <- convert_to_episodes(coding, cores = 1L)
  shared <- shared_synchronous_episodes(coded_data)
  skip_if(
    nrow(shared) == 0L,
    "Brazil fixture has no shared synchronous intervals."
  )
  ids <- unique(as.character(shared$id))
  skip_if(length(ids) != 1L, "Brazil fixture must represent one video ID.")
  manifest <- export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(video_files, ids),
    n = 1L,
    overwrite = TRUE
  )

  expected_archive <- file.path(
    dirname(video_files[[1L]]),
    paste0(
      tools::file_path_sans_ext(basename(video_files[[1L]])),
      "-shared-synchrony-clips.zip"
    )
  )
  expect_s3_class(manifest, "data.table")
  expect_equal(
    manifest$archive_path,
    normalizePath(expected_archive, winslash = "/")
  )
  expect_true(file.exists(expected_archive))
  expect_equal(
    utils::unzip(output, list = TRUE)$Name,
    c(manifest$clip_filename, "manifest.csv")
  )
})

test_that("export_shared_synchrony_clips exports Brazil fixture intervals to a folder", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  video_files <- list.files(
    brazil_dir,
    pattern = "\\.(mp4|mov|avi|mkv|webm)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  coding_files <- list.files(
    brazil_dir,
    pattern = "\\.(csv|txt|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(length(video_files) != 1L, "Brazil fixture must contain one video.")
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  ffprobe <- Sys.which("ffprobe")
  skip_if(ffprobe == "", "FFprobe is not available.")
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two FaceReader outputs."
  )
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")

  emotions <- c(
    "neutral",
    "happy",
    "sad",
    "angry",
    "surprised",
    "scared",
    "disgusted"
  )
  coding <- dplyr::bind_rows(lapply(coding_files, function(path) {
    loadFRfile(path) |>
      dplyr::transmute(
        id = 1L,
        subject = tools::file_path_sans_ext(basename(path)),
        video_time,
        dplyr::across(dplyr::all_of(emotions))
      )
  }))
  coded_data <- convert_to_episodes(coding, cores = 1L)
  shared <- shared_synchronous_episodes(coded_data)
  skip_if(
    nrow(shared) == 0L,
    "Brazil fixture has no shared synchronous intervals."
  )
  ids <- unique(as.character(shared$id))
  skip_if(length(ids) != 1L, "Brazil fixture must represent one video ID.")
  output_dir <- file.path(
    dirname(video_files[[1L]]),
    paste0(
      tools::file_path_sans_ext(basename(video_files[[1L]])),
      "-shared-synchrony-clips"
    )
  )
  unlink(output_dir, recursive = TRUE, force = TRUE)
  video_duration <- function(path) {
    as.numeric(system2(
      ffprobe,
      args = c(
        "-v",
        "error",
        "-show_entries",
        "format=duration",
        "-of",
        "default=noprint_wrappers=1:nokey=1",
        shQuote(path)
      ),
      stdout = TRUE,
      stderr = FALSE
    ))
  }

  unbuffered_manifest <- export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(video_files, ids),
    n = 1L,
    output_path = output_dir,
    output = "folder",
    overwrite = TRUE
  )
  unbuffered_duration <- video_duration(
    file.path(output_dir, unbuffered_manifest$clip_filename)
  )
  expect_equal(
    unbuffered_duration,
    unbuffered_manifest$duration_seconds,
    tolerance = 0.1
  )

  buffered_manifest <- export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(video_files, ids),
    n = 1L,
    buffer = 1,
    output_path = output_dir,
    output = "folder",
    overwrite = TRUE
  )
  buffered_duration <- video_duration(
    file.path(output_dir, buffered_manifest$clip_filename)
  )
  expect_equal(
    buffered_duration,
    buffered_manifest$duration_seconds,
    tolerance = 0.1
  )
  expect_equal(
    buffered_manifest$duration_seconds,
    (buffered_manifest$original_end_frame -
      buffered_manifest$original_start_frame +
      1L +
      2L * round(buffered_manifest$fps)) /
      buffered_manifest$fps
  )

  asymmetric_manifest <- export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(video_files, ids),
    n = 1L,
    buffer = c(before = 2, after = 1),
    output_path = output_dir,
    output = "folder",
    overwrite = TRUE
  )
  asymmetric_duration <- video_duration(
    file.path(output_dir, asymmetric_manifest$clip_filename)
  )
  expect_equal(
    asymmetric_duration,
    asymmetric_manifest$duration_seconds,
    tolerance = 0.1
  )
  expect_equal(
    asymmetric_manifest$duration_seconds,
    (asymmetric_manifest$original_end_frame -
      asymmetric_manifest$original_start_frame +
      1L +
      3L * round(asymmetric_manifest$fps)) /
      asymmetric_manifest$fps
  )

  manifest <- export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(video_files, ids),
    n = 10L,
    output_path = output_dir,
    output = "folder",
    overwrite = TRUE
  )

  expect_s3_class(manifest, "data.table")
  expect_true(dir.exists(output_dir))
  expect_equal(dirname(output_dir), dirname(video_files[[1L]]))
  expect_true(file.exists(file.path(output_dir, "manifest.csv")))
  exported_video_files <- list.files(
    output_dir,
    pattern = "\\.(mp4|mov|avi|mkv|webm)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  expect_length(exported_video_files, 10L)
  expect_true(all(file.exists(file.path(output_dir, manifest$clip_filename))))
  expect_equal(
    unname(vapply(
      file.path(output_dir, manifest$clip_filename),
      video_duration,
      numeric(1)
    )),
    manifest$duration_seconds,
    tolerance = 0.1
  )
  expect_true(all(is.na(manifest$archive_path)))
})
