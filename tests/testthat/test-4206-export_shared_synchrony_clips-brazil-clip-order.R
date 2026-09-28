TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

fixture_error <- tryCatch(
  {
    load(file.path(TEST_DATA, "test_data.RDa"))
    NULL
  },
  error = identity
)
if (inherits(fixture_error, "error")) {
  skip(sprintf("Could not load test data fixtures: %s", fixture_error$message))
}

test_that("Brazil synchrony filenames include value rank and video timestamp order", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  video <- file.path(brazil_dir, "ID100024_side_by_side.mp4")
  coding_files <- list.files(
    brazil_dir,
    pattern = "^100024_(child|mum)_.*_detailed\\.xlsx$",
    full.names = TRUE
  )
  skip_if_not(file.exists(video))
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture requires two detailed exports."
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
  skip_if(
    sum(shared$emotion == "happy") < 5L,
    "Brazil fixture requires at least five happy synchronies."
  )
  output <- tempfile("ID100024-ranked-clips-", fileext = ".zip")
  on.exit(unlink(output, force = TRUE), add = TRUE)

  manifest <- export_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = c("1" = video),
    n = 5L,
    emotion = "happy",
    buffer = 0,
    output_path = output
  )

  expected_order <- rank(
    manifest$original_start_frame,
    ties.method = "first"
  )
  expect_equal(manifest$selection_rank, seq_len(5L))
  expect_equal(manifest$clip_order, as.integer(expected_order))
  expect_gt(sum(manifest$clip_order != manifest$selection_rank), 0L)
  expect_true(all(startsWith(
    manifest$clip_filename,
    sprintf(
      "%03d_order-%03d_id-1_",
      manifest$selection_rank,
      manifest$clip_order
    )
  )))
  expect_equal(
    utils::unzip(output, list = TRUE)$Name,
    c(manifest$clip_filename, "manifest.csv")
  )
})
