TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

write_readme_export <- function(path, video, participant) {
  writeLines(
    c(
      "Video analysis detailed log",
      "",
      "Face Model\tGeneral",
      "Calibration\t-",
      "Start time\t6/4/2026 13:31:06.331",
      paste("Filename", video, sep = "\t"),
      "Frame rate\t30.000000000",
      "",
      "Video Time\tNeutral\tHappy\tParticipant Name",
      paste("00:00:00.000\t0\t0.5", participant, sep = "\t"),
      paste("00:00:00.033\t0\t0.6", participant, sep = "\t")
    ),
    path
  )
}

test_that("README file-conversion examples use available fixtures", {
  skip_if_not_installed("readxl")

  txt_path <- testthat::test_path("testdata", "testdata_detailed.txt")
  excel_path <- testthat::test_path("testdata", "testdata_excel_detailed.xlsx")

  loaded <- loadFRfile(
    inpath = txt_path,
    values_as_numeric = TRUE,
    clean_names = TRUE
  )
  expect_gt(nrow(loaded), 0L)

  csv_path <- file.path(TEST_DATA, "testdata_detailed.csv")
  skip_if(!file.exists(csv_path), "The TEST_DATA detailed CSV is unavailable.")
  loaded_csv <- loadFRfile(
    inpath = csv_path,
    values_as_numeric = TRUE,
    clean_names = TRUE
  )
  expect_gt(nrow(loaded_csv), 0L)

  txt_output <- tempfile(fileext = ".csv")
  txt_metadata <- convertFRFiles(
    inpath = txt_path,
    outpath = txt_output,
    values_as_numeric = TRUE,
    clean_names = TRUE
  )
  expect_true(file.exists(txt_output))
  expect_equal(txt_metadata$type, "detailed")

  excel_data <- convertFRExcelFiles(
    inpath = excel_path,
    return_data = TRUE,
    values_as_numeric = TRUE,
    clean_names = TRUE
  )
  expect_gt(nrow(excel_data), 0L)

  output_dir <- tempfile("readme-directory-output-")
  on.exit(unlink(c(txt_output, output_dir), recursive = TRUE, force = TRUE))
  directory_metadata <- convertFRDirectory(
    inpath = testthat::test_path("testdata", "testdata2"),
    outpath = output_dir,
    values_as_numeric = TRUE,
    cores = 2L
  )
  expect_true(any(directory_metadata$status == "Success"))
  expect_true(all(file.exists(directory_metadata$outpath[
    directory_metadata$status == "Success"
  ])))
})

test_that("README synchrony pipeline examples run on a fixture study", {
  study_dir <- tempfile("readme-study-")
  dir.create(study_dir)
  on.exit(unlink(study_dir, recursive = TRUE, force = TRUE))

  video_path <- file.path(study_dir, "recording.mp4")
  file.create(video_path)
  write_readme_export(
    file.path(study_dir, "parent.txt"),
    "recording.mp4",
    "parent"
  )
  write_readme_export(file.path(study_dir, "teen.txt"), "recording.mp4", "teen")

  interactive_output <- tempfile("readme-pipeline-output-")
  options_output <- tempfile("readme-pipeline-options-")
  on.exit(
    unlink(
      c(interactive_output, options_output),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  interactive_result <- synchrony_moments_pipeline(
    inpath = study_dir,
    output_dir = interactive_output,
    n = 10L,
    emotion = "happy"
  )
  expect_s3_class(interactive_result, "synchrony_moments_pipeline")
  expect_equal(interactive_result$videos$status, "completed")
  expect_named(
    interactive_result$results[[1L]],
    c(
      "coding_input",
      "coded_data",
      "shared_moments",
      "clip_manifest",
      "clip_output"
    )
  )

  options_result <- synchrony_moments_pipeline(
    inpath = study_dir,
    output_dir = options_output,
    video_pattern = "\\.(mp4|mov|mkv|avi)$",
    video_path = NULL,
    subject_from_filename = FALSE,
    T_up = 0.20,
    T_down = 0.10,
    delta = 0.10,
    delta_window = 0.2,
    min_dur_sec = 0.1,
    consecutive_missing = 150L,
    cores = 0L,
    time_limit = 3,
    time_limit_frames = NULL,
    constraint_method = "episode",
    missing_threshold = 0,
    exclude_emotions = "neutral",
    n = 10L,
    emotion = "happy",
    optimised_subject = "both",
    only_synchronies = TRUE,
    buffer = c(before = 2, after = 1),
    buffer_units = "seconds",
    output = "zip",
    overwrite = FALSE,
    ffmpeg = "ffmpeg"
  )
  expect_equal(options_result$videos$status, "completed")
  expect_equal(nrow(options_result$results[[1L]]$clip_manifest), 0L)
})

test_that("README Brazil pipeline example uses available external fixtures", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  video_path <- file.path(brazil_dir, "ID100024_side_by_side.mp4")
  export_paths <- list.files(
    brazil_dir,
    pattern = "^100024_(child|mum)_.*_detailed\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if_not_installed("readxl")
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  skip_if(Sys.which("ffprobe") == "", "FFprobe is not available.")
  skip_if(
    !dir.exists(brazil_dir) ||
      !file.exists(video_path) ||
      length(export_paths) < 2L,
    "The Brazil video and two detailed export fixtures are required."
  )

  output_dir <- tempfile("readme-brazil-output-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE))
  result <- synchrony_moments_pipeline(
    inpath = brazil_dir,
    video_path = video_path,
    subject_from_filename = TRUE,
    output_dir = output_dir,
    n = 1L,
    overwrite = TRUE,
    cores = 1L
  )
  expect_s3_class(result, "synchrony_moments_pipeline")
  expect_equal(
    nrow(result$manifest[result$manifest$status == "validated", ]),
    2L
  )
  expect_equal(nrow(result$videos), 1L)
})

test_that("README CLI examples resolve and run without shell profile changes", {
  cli_path <- system.file(
    "scripts",
    "synchrony-moments",
    package = "facereaderconverter"
  )
  expect_true(file.exists(cli_path))

  script <- readLines(cli_path, warn = FALSE)
  entry_point <- which(script == "if (sys.nframe() == 0L) {")
  cli_env <- new.env(parent = globalenv())
  eval(parse(text = script[seq_len(entry_point[[1L]] - 1L)]), envir = cli_env)

  values <- cli_env$parse_args(c(
    "--input",
    "data/study",
    "--output-dir",
    "data/synchrony-clips",
    "--n",
    "10",
    "--emotion",
    "happy"
  ))
  expect_equal(values$n, 10L)
  expect_equal(values$emotion, "happy")

  study_dir <- tempfile("readme-cli-study-")
  output_dir <- tempfile("readme-cli-output-")
  dir.create(study_dir)
  on.exit(unlink(c(study_dir, output_dir), recursive = TRUE, force = TRUE))
  video_path <- file.path(study_dir, "recording.mp4")
  file.create(video_path)
  write_readme_export(
    file.path(study_dir, "parent.txt"),
    "recording.mp4",
    "parent"
  )
  write_readme_export(file.path(study_dir, "teen.txt"), "recording.mp4", "teen")
  cli_result <- cli_env$main(c(
    "--input",
    study_dir,
    "--output-dir",
    output_dir,
    "--n",
    "10",
    "--emotion",
    "happy"
  ))
  expect_equal(cli_result$videos$status, "completed")
  expect_true(file.exists(file.path(
    output_dir,
    "synchrony-moments-manifest.csv"
  )))
  expect_message(
    expect_null(cli_env$main("--help")),
    "Usage: synchrony-moments"
  )
})

test_that("README episode and LOCF examples use TEST_DATA CSV", {
  csv_path <- file.path(TEST_DATA, "testdata_detailed.csv")
  skip_if(!file.exists(csv_path), "The detailed CSV fixture is not available.")

  coding_df <- read.csv(csv_path) |>
    dplyr::mutate(id = 1, subject = "parent")
  coding_df2 <- coding_df |>
    tidyr::pivot_longer(
      cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
      names_to = "emotion",
      values_to = "value"
    )
  result <- convert_to_episodes(
    coding_df2,
    fps = 30L,
    T_up = 0.20,
    T_down = 0.18,
    delta = 0.10,
    delta_window = 0.1,
    min_dur_sec = 0.1,
    consecutive_missing = 150L
  )
  expect_s3_class(result, "fr_coding")
  expect_true(all(
    c("episodes", "deltas", "coding", "metadata") %in% names(result)
  ))
  expect_gt(nrow(result$episodes), 0L)
  expect_s3_class(locf(result), "fr_coding")

  test_data_path <- file.path(TEST_DATA, "test_data.RDa")
  skip_if(
    !file.exists(test_data_path),
    "The TEST_DATA RDA fixture is unavailable."
  )
  fixture_env <- new.env(parent = globalenv())
  load(test_data_path, envir = fixture_env)
  coded_data <- convert_to_episodes(fixture_env$test_coding_wide, fps = 30L)
  coding_with_delta <- suppressWarnings(add_delta_column(
    coded_data$coding,
    delta = 0.1,
    delta_window = 0.2,
    fps = 30L
  ))
  delta_alias <- suppressWarnings(delta(
    coded_data$coding,
    delta = 0.1,
    delta_window = 0.2,
    fps = 30L
  ))
  delta_event_episodes <- delta_episodes(coding_with_delta, fps = 30L)
  expect_true("delta" %in% names(coding_with_delta))
  expect_true("delta" %in% names(delta_alias))
  expect_gt(nrow(delta_event_episodes), 0L)
})

test_that("README downstream examples run on TEST_DATA coded fixture", {
  test_data_path <- file.path(TEST_DATA, "test_data.RDa")
  skip_if(
    !file.exists(test_data_path),
    "The TEST_DATA RDA fixture is not available."
  )
  fixture_env <- new.env(parent = globalenv())
  load(test_data_path, envir = fixture_env)
  skip_if(
    !exists("test_coding_wide", envir = fixture_env),
    "Wide coding fixture is unavailable."
  )

  coded_data <- convert_to_episodes(
    get("test_coding_wide", envir = fixture_env),
    fps = 30L
  )
  sync <- synchrony(coded_data, missing_threshold = 0.5)
  sync_by_episode <- synchrony_by_episode(coded_data, missing_threshold = 0.5)
  shared <- shared_synchronous_episodes(
    coded_data,
    exclude_emotions = "neutral"
  )
  reaction <- reaction_rate(coded_data, fps = 30)
  reaction_by_episode <- reaction_rate_by_episode(coded_data, fps = 30)
  set.seed(8731)
  controls <- negative_controls(coded_data, sync_by_episode)
  imputed <- locf(coded_data)

  expect_s3_class(coded_data, "fr_coding")
  expect_gt(nrow(sync), 0L)
  expect_gt(nrow(sync_by_episode), 0L)
  expect_gt(nrow(shared), 0L)
  expect_gt(nrow(reaction), 0L)
  expect_gt(nrow(reaction_by_episode), 0L)
  expect_true(all(c("control_status", "synchrony") %in% names(controls)))
  expect_s3_class(imputed, "fr_coding")

  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  video_path <- file.path(TEST_DATA, "brazil", "ID100024_side_by_side.mp4")
  skip_if(
    !file.exists(video_path),
    "The Brazil video fixture is not available."
  )
  shared_for_clip <- shared[
    id == 1 & emotion == "happy" & end_frame < 14600
  ][1L]
  skip_if(
    nrow(shared_for_clip) == 0L,
    "No compatible shared interval for clip export."
  )
  archive_path <- tempfile(fileext = ".zip")
  on.exit(unlink(archive_path))
  export_shared_synchrony_clips(
    coded_data,
    shared_for_clip,
    video_paths = c("1" = video_path),
    n = 1L,
    emotion = "happy",
    buffer = c(before = 2, after = 1),
    buffer_units = "seconds",
    output_path = archive_path
  )
  expect_true(file.exists(archive_path))
})

test_that("README utility examples run with valid mapped paths", {
  expect_equal(
    to_seconds(
      c("00:00:10", "00:00:10.500", "00:01:00"),
      digits = 0L
    ),
    c(10, 10, 60)
  )
  expect_equal(
    parse_time_to_frame(c("00:00:10.5", "1:23.5", "83.5"), fps = 30),
    c(315L, 2505L, 2505L)
  )

  input_dir <- tempfile("readme-map-input-")
  output_dir <- tempfile("readme-map-output-")
  dir.create(file.path(input_dir, "session1"), recursive = TRUE)
  dir.create(file.path(input_dir, "session2"), recursive = TRUE)
  file.create(file.path(input_dir, "session1", "a.txt"))
  file.create(file.path(input_dir, "session2", "b.txt"))
  on.exit(unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE))

  mapped <- map_paths(
    input_dir = input_dir,
    output_dir = output_dir,
    files = file.path(input_dir, c("session1", "session2", "a.txt", "b.txt"))
  )
  expect_equal(basename(mapped), c("session1", "session2", "a.txt", "b.txt"))
  expect_true(dir.exists(dirname(mapped[[3L]])))
})
