TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

test_data_path <- file.path(TEST_DATA, "test_data.RDa")
test_data_error <- tryCatch(
  {
    load(test_data_path)
    NULL
  },
  error = identity
)
if (inherits(test_data_error, "error")) {
  skip(sprintf(
    "Could not load test data fixtures: %s",
    test_data_error$message
  ))
}

test_that("export_shared_synchrony_clips exports Brazil fixture intervals", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  video_files <- list.files(
    brazil_dir,
    pattern = "^ID100024_side_by_side\\.mp4$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  coding_files <- list.files(
    brazil_dir,
    pattern = "^00024_.*_detailed\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(video_files) != 1L,
    "Brazil fixture must contain the ID100024 video."
  )
  output_dir <- tempfile("synchrony_inspection_4200-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  output <- file.path(output_dir, "ID100024-shared-synchrony-clips.zip")
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two ID100024 FaceReader outputs."
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
    output_path = output,
    overwrite = TRUE
  )

  expected_archive <- output
  expect_s3_class(manifest, "data.table")
  test_started <- Sys.time()
  age_files(output)
  expect_invisible(export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(video_files, ids),
    n = 1L,
    output_path = output,
    overwrite = TRUE
  ))
  expect_equal(
    manifest$archive_path,
    normalizePath(expected_archive, winslash = "/")
  )
  expect_true(file.exists(expected_archive))
  expect_files_modified_since(expected_archive, test_started)
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
    pattern = "^ID100024_side_by_side\\.mp4$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  coding_files <- list.files(
    brazil_dir,
    pattern = "^00024_.*_detailed\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(video_files) != 1L,
    "Brazil fixture must contain the ID100024 video."
  )
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  ffprobe <- Sys.which("ffprobe")
  skip_if(ffprobe == "", "FFprobe is not available.")
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two ID100024 FaceReader outputs."
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
  output_dir <- tempfile("synchrony_inspection_4200-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
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

  test_started <- Sys.time()
  age_files(
    c(
      file.path(output_dir, "manifest.csv"),
      file.path(output_dir, unbuffered_manifest$clip_filename),
      file.path(output_dir, buffered_manifest$clip_filename),
      file.path(output_dir, asymmetric_manifest$clip_filename)
    )
  )
  test_started <- Sys.time()
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
  expect_true(file.exists(file.path(output_dir, "manifest.csv")))
  expect_files_modified_since(
    c(
      file.path(output_dir, "manifest.csv"),
      file.path(output_dir, manifest$clip_filename)
    ),
    test_started
  )
  exported_video_files <- list.files(
    output_dir,
    pattern = "^[0-9]{3}_id-1_.*\\.mp4$",
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

test_that("export_shared_synchrony_clips exports all Brazil happy intervals with n zero", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  video_files <- list.files(
    brazil_dir,
    pattern = "^ID100024_side_by_side\\.mp4$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  coding_files <- list.files(
    brazil_dir,
    pattern = "^00024_.*_detailed\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(video_files) != 1L,
    "Brazil fixture must contain the ID100024 video."
  )
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two ID100024 FaceReader outputs."
  )
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  skip_if(Sys.which("ffprobe") == "", "FFprobe is not available.")

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
  shared_happy <- shared[emotion == "happy"]
  skip_if(
    nrow(shared_happy) == 0L,
    "Brazil fixture has no shared happy intervals."
  )
  ids <- unique(as.character(shared$id))
  skip_if(length(ids) != 1L, "Brazil fixture must represent one video ID.")
  output_dir <- tempfile("synchrony_inspection_4200-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  test_started <- Sys.time()

  manifest <- export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(video_files, ids),
    n = 0L,
    emotion = "happy",
    output_path = output_dir,
    output = "folder",
    overwrite = TRUE
  )

  expect_equal(nrow(manifest), nrow(shared_happy))
  expect_files_modified_since(
    c(
      file.path(output_dir, "manifest.csv"),
      file.path(output_dir, manifest$clip_filename)
    ),
    test_started
  )
  expect_length(
    list.files(
      output_dir,
      pattern = "^[0-9]{3}_id-1_.*\\.mp4$",
      full.names = TRUE,
      ignore.case = TRUE
    ),
    nrow(shared_happy)
  )
  unlink(output_dir, recursive = TRUE, force = TRUE)
})
