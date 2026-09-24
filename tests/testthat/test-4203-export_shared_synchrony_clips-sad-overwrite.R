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

library(testthat)

make_brazil_clip_inputs <- function() {
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
  sad_shared <- shared[emotion == "sad"]
  skip_if(nrow(sad_shared) == 0L, "Brazil fixture has no shared sad intervals.")

  list(
    brazil_dir = brazil_dir,
    coded_data = coded_data,
    shared = sad_shared,
    video_paths = stats::setNames(
      video_files,
      unique(as.character(sad_shared$id))
    )
  )
}

test_that("export_shared_synchrony_clips caps sad clips and overwrites ZIP output", {
  inputs <- make_brazil_clip_inputs()
  output <- tempfile("ID100024-sad-shared-synchrony-clips-", fileext = ".zip")
  on.exit(unlink(output, force = TRUE), add = TRUE)
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
  age_files(output)
  test_started <- Sys.time()
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
  expect_files_modified_since(output, test_started)
})

test_that("export_shared_synchrony_clips caps sad clips and overwrites folder output", {
  inputs <- make_brazil_clip_inputs()
  output_dir <- tempfile("ID100024-sad-shared-synchrony-clips-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
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
  expect_equal(
    length(list.files(output_dir, pattern = "\\.mp4$")),
    nrow(manifest)
  )
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
  age_files(c(
    file.path(output_dir, "manifest.csv"),
    file.path(output_dir, manifest$clip_filename)
  ))
  test_started <- Sys.time()
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
  expect_equal(
    length(list.files(output_dir, pattern = "\\.mp4$")),
    nrow(overwritten)
  )
  expect_files_modified_since(
    c(
      file.path(output_dir, "manifest.csv"),
      file.path(output_dir, overwritten$clip_filename)
    ),
    test_started
  )
})
