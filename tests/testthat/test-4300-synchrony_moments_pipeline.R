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
  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("synchrony_moments_pipeline supports explicit video and filename subjects", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  file.create(file.path(root, "side-by-side.mp4"))
  write_detailed_export(file.path(root, "parent.txt"), "parent.mp4", "")
  write_detailed_export(file.path(root, "teen.txt"), "teen.mp4", "")

  result <- synchrony_moments_pipeline(
    root,
    video_path = file.path(root, "side-by-side.mp4"),
    subject_from_filename = TRUE
  )
  expect_equal(result$videos$participant1, "parent")
  expect_equal(result$videos$participant2, "teen")
  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("synchrony_moments_pipeline permits missing metadata with video override", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  video_path <- file.path(root, "side-by-side.mp4")
  file.create(video_path)
  write_detailed_export(file.path(root, "parent.txt"), "", "parent")
  write_detailed_export(file.path(root, "teen.txt"), "", "teen")

  expect_no_message(
    result <- synchrony_moments_pipeline(root, video_path = video_path)
  )

  expect_equal(result$videos$video_filename, "side-by-side.mp4")
  expect_equal(result$videos$participant1, "parent")
  expect_equal(result$videos$participant2, "teen")
})

test_that("synchrony_moments_pipeline rejects unmatched FaceReader video metadata", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  file.create(file.path(root, "recording.mp4"))
  write_detailed_export(file.path(root, "parent.txt"), "other.mp4", "parent")

  expect_error(
    synchrony_moments_pipeline(root),
    "No discovered video matches FaceReader Filename metadata"
  )
  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("synchrony_moments_pipeline reports matched and unmatched videos", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
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
  expect_equal(nrow(result$videos), 1L)
  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("synchrony_moments_pipeline requires two distinct participants", {
  root <- tempfile("synchrony-pipeline-")
  dir.create(root)
  file.create(file.path(root, "recording.mp4"))
  write_detailed_export(
    file.path(root, "parent.txt"),
    "recording.mp4",
    "parent"
  )

  expect_error(
    synchrony_moments_pipeline(root),
    "exactly two detailed outputs"
  )
  unlink(root, recursive = TRUE, force = TRUE)
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
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two FaceReader outputs."
  )

  result <- synchrony_moments_pipeline(
    brazil_dir,
    video_path = video_file[[1L]],
    subject_from_filename = TRUE,
    verbose = TRUE,
    output_dir = tempfile("brazil-clips-"),
    emotion = "not_an_emotion"
  )

  expect_s3_class(result, "synchrony_moments_pipeline")
  expect_equal(nrow(result$manifest), 2L)
  expect_equal(nrow(result$videos), 1L)
  expect_equal(result$videos$video_filename, basename(video_file[[1L]]))
  expect_equal(
    data.table::uniqueN(
      unlist(result$videos[, c("participant1", "participant2")])
    ),
    2L
  )
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
  skip_if(
    length(coding_files) != 2L,
    "Brazil fixture must contain two FaceReader outputs."
  )
  output_dir <- file.path(TEST_DATA, "brazil_output")

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
  expect_equal(nrow(manifest), 10L)
  expect_true(file.exists(file.path(clip_dir, "manifest.csv")))
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
