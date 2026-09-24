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

write_detailed_export <- function(path, video, participant) {
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

test_that("synchrony_moments_pipeline validates recursive FaceReader video matching", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(file.path(root, "videos"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(root, "exports", "nested"), recursive = TRUE)
  file.create(file.path(root, "videos", "Recording.MP4"))
  write_detailed_export(
    file.path(root, "exports", "nested", "parent.txt"),
    "C:/Source/recording.mp4",
    "parent"
  )
  write_detailed_export(
    file.path(root, "exports", "nested", "teen.txt"),
    "C:/Source/recording.mp4",
    "teen"
  )

  result <- synchrony_moments_pipeline(
    root,
    output_dir = file.path(root, "clips")
  )
  expect_s3_class(result, "synchrony_moments_pipeline")
  expect_equal(result$videos$video_filename, "Recording.MP4")
  expect_equal(result$videos$participant1, "parent")
  expect_equal(result$videos$participant2, "teen")
  expect_equal(nrow(result$results[[1]]$clip_manifest), 0L)
})

test_that("synchrony_moments_pipeline strictly filters explicit videos by metadata", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  video_path <- file.path(root, "side-by-side.mp4")
  file.create(video_path)
  write_detailed_export(file.path(root, "parent.txt"), "side-by-side.mp4", "")
  write_detailed_export(file.path(root, "teen.txt"), "side-by-side.mp4", "")
  write_detailed_export(file.path(root, "other.txt"), "other.mp4", "other")

  expect_no_warning(
    result <- synchrony_moments_pipeline(
      root,
      video_path = video_path,
      subject_from_filename = TRUE
    )
  )

  expect_equal(result$videos$video_filename, "side-by-side.mp4")
  expect_equal(result$videos$status, "completed")
  expect_equal(result$videos$n_matched_outputs, 2L)
  expect_equal(result$videos$participant1, "parent")
  expect_equal(result$videos$participant2, "teen")
  expect_equal(
    result$manifest[basename(fr_path) == "other.txt", status],
    "skipped"
  )
  expect_equal(
    result$manifest[basename(fr_path) == "other.txt", error],
    "Filename metadata does not match the selected video."
  )
})

test_that("synchrony_moments_pipeline rejects unmatched FaceReader video metadata", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "recording.mp4"))
  write_detailed_export(file.path(root, "parent.txt"), "other.mp4", "parent")

  expect_error(
    synchrony_moments_pipeline(root),
    "No discovered video matches FaceReader Filename metadata"
  )
})

test_that("synchrony_moments_pipeline reports matched and unmatched videos", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "matched.mp4"))
  file.create(file.path(root, "unmatched.mp4"))
  write_detailed_export(
    file.path(root, "parent.txt"),
    "matched.mp4",
    "parent"
  )
  write_detailed_export(
    file.path(root, "teen.txt"),
    "matched.mp4",
    "teen"
  )

  result <- NULL
  expect_message(
    result <- synchrony_moments_pipeline(root, verbose = TRUE),
    paste(
      "Videos matched to FaceReader files:",
      ".*matched.mp4",
      ".*Videos without matching FaceReader files:",
      ".*unmatched.mp4",
      sep = "\\n"
    )
  )
  expect_equal(nrow(result$videos), 2L)
  expect_equal(
    result$videos[video_filename == "matched.mp4", status],
    "completed"
  )
  expect_equal(
    result$videos[video_filename == "unmatched.mp4", status],
    "unmatched"
  )
})

test_that("synchrony_moments_pipeline warns and skips videos without two matched outputs", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "recording.mp4"))
  write_detailed_export(
    file.path(root, "parent.txt"),
    "recording.mp4",
    "parent"
  )

  expect_warning(
    result <- synchrony_moments_pipeline(root),
    "found 1 sufficiently matched outputs out of 1 detailed outputs"
  )
  expect_equal(nrow(result$videos), 1L)
  expect_equal(result$videos$status, "failed")
  expect_equal(result$videos$n_matched_outputs, 1L)
  expect_equal(result$manifest$status, "skipped")
})

test_that("synchrony_moments_pipeline skips extra distinct participant outputs", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "recording.mp4"))
  write_detailed_export(
    file.path(root, "parent.txt"),
    "recording.mp4",
    "parent"
  )
  write_detailed_export(file.path(root, "teen.txt"), "recording.mp4", "teen")
  write_detailed_export(
    file.path(root, "observer.txt"),
    "recording.mp4",
    "observer"
  )

  expect_warning(
    result <- synchrony_moments_pipeline(root),
    "found 3 sufficiently matched outputs out of 3 detailed outputs"
  )
  expect_equal(nrow(result$videos), 1L)
  expect_equal(result$videos$status, "failed")
  expect_equal(result$videos$n_matched_outputs, 3L)
  expect_true(all(result$manifest$status == "skipped"))
})

test_that("synchrony_moments_pipeline errors on duplicate participant outputs", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "recording.mp4"))
  write_detailed_export(
    file.path(root, "parent-1.txt"),
    "recording.mp4",
    "parent"
  )
  write_detailed_export(
    file.path(root, "parent-2.txt"),
    "recording.mp4",
    "parent"
  )

  expect_error(
    synchrony_moments_pipeline(root),
    "Multiple FaceReader outputs have the same Participant Name"
  )
})

test_that("synchrony_moments_pipeline processes Brazil video and exports", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  video_files <- list.files(
    brazil_dir,
    pattern = "\\.(mp4|mov|avi|mkv|webm)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  video_files <- video_files[dirname(video_files) == brazil_dir]
  video_file <- video_files[
    basename(video_files) == "ID100024_side_by_side.mp4"
  ]
  coding_files <- list.files(
    brazil_dir,
    pattern = "\\.(txt|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(video_file) != 1L,
    "Brazil fixture must contain ID100024_side_by_side.mp4."
  )

  output_dir <- tempfile("brazil-clips-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  result <- synchrony_moments_pipeline(
    brazil_dir,
    video_path = video_file[[1L]],
    subject_from_filename = TRUE,
    verbose = TRUE,
    output_dir = output_dir,
    emotion = "not_an_emotion"
  )

  expect_s3_class(result, "synchrony_moments_pipeline")
  expect_equal(
    nrow(result$manifest[result$manifest$status == "validated", ]),
    2L
  )
  expect_equal(nrow(result$videos), 1L)
  expect_equal(result$videos$video_filename, basename(video_file[[1L]]))

  expect_named(result$results, "video_001")
  expect_s3_class(result$results$video_001$coded_data, "fr_coding")
  expect_s3_class(result$results$video_001$shared_moments, "data.table")
  expect_equal(nrow(result$results$video_001$clip_manifest), 0L)
})

test_that("synchrony_moments_pipeline exports ten Brazil happy clips to a folder", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  video_files <- list.files(
    brazil_dir,
    pattern = "\\.(mp4|mov|avi|mkv|webm)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  video_files <- video_files[dirname(video_files) == brazil_dir]
  video_file <- video_files[
    basename(video_files) == "ID100024_side_by_side.mp4"
  ]
  coding_files <- list.files(
    brazil_dir,
    pattern = "\\.(txt|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(video_file) != 1L,
    "Brazil fixture must contain ID100024_side_by_side.mp4."
  )

  inspection_root <- file.path(TEST_DATA, "synchrony_moments_inspection")
  output_dir <- file.path(inspection_root, "brazil_happy_clips")
  dir.create(inspection_root, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(output_dir)) {
    age_files(list.files(output_dir, recursive = TRUE, full.names = TRUE))
  }
  test_started <- Sys.time()

  result <- synchrony_moments_pipeline(
    brazil_dir,
    video_path = video_file[[1L]],
    subject_from_filename = TRUE,
    output_dir = output_dir,
    n = 10L,
    emotion = "happy",
    output = "folder",
    overwrite = TRUE
  )

  manifest <- result$results$video_001$clip_manifest
  clip_dir <- result$results$video_001$clip_output
  expect_false(startsWith(
    normalizePath(clip_dir, winslash = "/", mustWork = FALSE),
    paste0(normalizePath(brazil_dir, winslash = "/"), "/")
  ))
  expect_equal(nrow(manifest), 10L)
  expect_true(file.exists(file.path(clip_dir, "manifest.csv")))
  expect_files_modified_since(
    file.path(clip_dir, "manifest.csv"),
    test_started
  )
  expect_length(
    list.files(
      clip_dir,
      pattern = "\\.(mp4|mov|avi|mkv|webm)$",
      full.names = TRUE,
      ignore.case = TRUE
    ),
    10L
  )
  expect_true(all(vapply(
    file.path(clip_dir, manifest$clip_filename),
    facereaderconverter:::shared_synchrony_file_exists,
    logical(1)
  )))
})

test_that("synchrony_moments_pipeline processes Brazil video directory and exports", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))

  output_dir <- tempfile("brazil-clips-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  result <- synchrony_moments_pipeline(
    brazil_dir,
    subject_from_filename = TRUE,
    verbose = FALSE,
    output_dir = output_dir,
    emotion = "happy"
  )

  expect_s3_class(result, "synchrony_moments_pipeline")
  expect_equal(
    nrow(result$manifest[result$manifest$status == "validated", ]),
    4L
  )
  expect_equal(nrow(result$videos[result$videos$status == "completed", ]), 2L)

  expect_named(result$results, c("video_001", "video_002"))
  expect_s3_class(result$results$video_001$coded_data, "fr_coding")
  expect_s3_class(result$results$video_001$shared_moments, "data.table")
})
