TEST_DATA <- Sys.getenv("TEST_DATA")

test_that("pipeline export matches export_shared_synchrony_clips", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  skip_if(Sys.which("ffprobe") == "", "FFprobe is not available.")

  video_path <- file.path(brazil_dir, "ID100024_side_by_side.mp4")
  skip_if_not(file.exists(video_path))
  coding_files <- list.files(
    brazil_dir,
    pattern = "\\.(txt|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two FaceReader outputs."
  )

  output_pipeline <- tempfile("brazil-pipeline-")
  output_exporter <- tempfile("brazil-exporter-")
  on.exit(
    unlink(
      c(output_pipeline, output_exporter),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  export_args <- list(
    n = 10L,
    emotion = "happy",
    optimised_subject = "both",
    only_synchronies = TRUE,
    buffer = 0,
    buffer_units = "seconds",
    output = "folder",
    overwrite = TRUE,
    ffmpeg = "ffmpeg"
  )

  pipeline_args <- list(
    inpath = brazil_dir,
    output_dir = output_pipeline,
    video_path = video_path,
    subject_from_filename = TRUE,
    T_up = 0.20,
    T_down = 0.1,
    delta = 0.10,
    delta_window = 0.2,
    min_dur_sec = 0.1,
    consecutive_missing = 150L,
    cores = 1L,
    time_limit = 3,
    time_limit_frames = NULL,
    constraint_method = "episode",
    missing_threshold = 0,
    exclude_emotions = "neutral",
    output = export_args$output,
    overwrite = export_args$overwrite,
    ffmpeg = export_args$ffmpeg,
    n = export_args$n,
    emotion = export_args$emotion,
    optimised_subject = export_args$optimised_subject,
    only_synchronies = export_args$only_synchronies,
    buffer = export_args$buffer,
    buffer_units = export_args$buffer_units
  )
  test_started <- Sys.time()
  pipeline <- do.call(synchrony_moments_pipeline, pipeline_args)

  pipeline_manifest <- pipeline$results$video_001$clip_manifest
  pipeline_clip_dir <- pipeline$results$video_001$clip_output
  pipeline_started <- test_started
  test_started <- Sys.time()
  direct_manifest <- do.call(
    export_shared_synchrony_clips,
    c(
      list(
        coded_data = pipeline$results$video_001$coded_data,
        shared_synchrony = pipeline$results$video_001$shared_moments,
        video_paths = stats::setNames(video_path, "video_001"),
        output_path = output_exporter
      ),
      export_args
    )
  )

  normalise_manifest <- function(manifest) {
    manifest <- data.table::copy(manifest)
    manifest[, clip_path := basename(clip_path)]
    data.table::setorderv(manifest, names(manifest))
    manifest
  }

  expect_files_modified_since(
    c(
      file.path(pipeline_clip_dir, "manifest.csv"),
      file.path(pipeline_clip_dir, pipeline_manifest$clip_filename)
    ),
    pipeline_started
  )
  expect_files_modified_since(
    c(
      file.path(output_exporter, "manifest.csv"),
      file.path(output_exporter, direct_manifest$clip_filename)
    ),
    test_started
  )
  expect_equal(
    normalise_manifest(pipeline_manifest),
    normalise_manifest(direct_manifest)
  )

  pipeline_files <- list.files(
    pipeline_clip_dir,
    pattern = "\\.(mp4|mov|avi|mkv|webm)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  direct_files <- list.files(
    output_exporter,
    pattern = "\\.(mp4|mov|avi|mkv|webm)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  expect_length(direct_files, length(pipeline_files))
  expect_setequal(basename(direct_files), basename(pipeline_files))
})
