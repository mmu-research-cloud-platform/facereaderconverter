TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)

make_brazil_clip_inputs <- function() {
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
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two FaceReader outputs."
  )
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")

  emotions <- c(
    "neutral", "happy", "sad", "angry", "surprised", "scared", "disgusted"
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
  sad_shared <- shared[emotion == "sad"]
  skip_if(nrow(sad_shared) == 0L, "Brazil fixture has no shared sad intervals.")

  list(
    brazil_dir = brazil_dir,
    coded_data = coded_data,
    shared = sad_shared,
    video_paths = stats::setNames(video_files, unique(as.character(sad_shared$id)))
  )
}

test_that("export_shared_synchrony_clips caps sad clips and overwrites ZIP output", {
  inputs <- make_brazil_clip_inputs()
  output <- file.path(inputs$brazil_dir, "sad-shared-synchrony-clips.zip")
  requested_n <- nrow(inputs$shared) + 1L

  manifest <- export_shared_synchrony_clips(
    inputs$coded_data,
    inputs$shared,
    video_paths = inputs$video_paths,
    n = requested_n,
    emotion = "sad",
    output_path = output,
    overwrite = TRUE
  )

  expect_equal(nrow(manifest), nrow(inputs$shared))
  expect_equal(manifest$emotion, rep("sad", nrow(manifest)))
  expect_equal(
    utils::unzip(output, list = TRUE)$Name,
    c(manifest$clip_filename, "manifest.csv")
  )
  expect_error(
    export_shared_synchrony_clips(
      inputs$coded_data,
      inputs$shared,
      video_paths = inputs$video_paths,
      n = requested_n,
      emotion = "sad",
      output_path = output
    ),
    "ZIP archive already exists"
  )
  expect_s3_class(
    export_shared_synchrony_clips(
      inputs$coded_data,
      inputs$shared,
      video_paths = inputs$video_paths,
      n = requested_n,
      emotion = "sad",
      output_path = output,
      overwrite = TRUE
    ),
    "data.table"
  )
})

test_that("export_shared_synchrony_clips caps sad clips and overwrites folder output", {
  inputs <- make_brazil_clip_inputs()
  output_dir <- file.path(inputs$brazil_dir, "sad-shared-synchrony-clips")
  requested_n <- nrow(inputs$shared) + 1L

  manifest <- export_shared_synchrony_clips(
    inputs$coded_data,
    inputs$shared,
    video_paths = inputs$video_paths,
    n = requested_n,
    emotion = "sad",
    output_path = output_dir,
    output = "folder",
    overwrite = TRUE
  )

  expect_equal(nrow(manifest), nrow(inputs$shared))
  expect_equal(length(list.files(output_dir, pattern = "\\.mp4$")), nrow(manifest))
  expect_true(file.exists(file.path(output_dir, "manifest.csv")))
  expect_error(
    export_shared_synchrony_clips(
      inputs$coded_data,
      inputs$shared,
      video_paths = inputs$video_paths,
      n = requested_n,
      emotion = "sad",
      output_path = output_dir,
      output = "folder"
    ),
    "output directory is not empty"
  )
  overwritten <- export_shared_synchrony_clips(
    inputs$coded_data,
    inputs$shared,
    video_paths = inputs$video_paths,
    n = requested_n,
    emotion = "sad",
    output_path = output_dir,
    output = "folder",
    overwrite = TRUE
  )
  expect_equal(length(list.files(output_dir, pattern = "\\.mp4$")), nrow(overwritten))
})
