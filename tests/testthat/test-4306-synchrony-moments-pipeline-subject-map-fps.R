library(testthat)

write_pipeline_subject_fps_export <- function(path, participant, fps) {
  writeLines(
    c(
      "Video analysis detailed log",
      "",
      "Face Model\tGeneral",
      "Calibration\t-",
      "Start time\t6/4/2026 13:31:06.331",
      "Filename\tsession.mp4",
      paste("Frame rate", fps, sep = "\t"),
      "",
      "Video Time\tNeutral\tHappy\tParticipant Name",
      paste("00:00:00.000\t0\t0.5", participant, sep = "\t"),
      paste("00:00:00.033\t0\t0.6", participant, sep = "\t")
    ),
    path
  )
}

test_that("pipeline subject map overrides export participant names", {
  root <- tempfile("synchrony-pipeline-subject-map-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "session.mp4"))
  write_pipeline_subject_fps_export(
    file.path(root, "first.txt"),
    "",
    "30.000000000"
  )
  write_pipeline_subject_fps_export(
    file.path(root, "second.txt"),
    "other",
    "30.000000000"
  )

  result <- synchrony_moments_pipeline(
    root,
    subject_map = c(
      "first.txt" = "mapped-parent",
      "second.txt" = "mapped-teen"
    )
  )

  expect_equal(result$videos$participant1, "mapped-parent")
  expect_equal(result$videos$participant2, "mapped-teen")
})

test_that("XLSX frame-rate metadata is rounded to an integer", {
  metadata <- data.frame(
    V1 = "Frame rate",
    V2 = "29.97",
    stringsAsFactors = FALSE
  )

  expect_equal(synchrony_fr_xlsx_fps(metadata), 30)
})

test_that("pipeline skips videos when export frame rates disagree", {
  root <- tempfile("synchrony-pipeline-fps-mismatch-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "session.mp4"))
  write_pipeline_subject_fps_export(
    file.path(root, "first.txt"),
    "parent",
    "24"
  )
  write_pipeline_subject_fps_export(
    file.path(root, "second.txt"),
    "teen",
    "30"
  )

  expect_warning(
    result <- synchrony_moments_pipeline(root),
    "valid frame rates across 2 unique values"
  )
  expect_equal(result$videos$status, "failed")
  expect_length(result$results, 0L)
})

test_that("pipeline inherits rounded frame rate from export metadata", {
  root <- tempfile("synchrony-pipeline-fps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "session.mp4"))
  write_pipeline_subject_fps_export(
    file.path(root, "first.txt"),
    "parent",
    "29.97"
  )
  write_pipeline_subject_fps_export(
    file.path(root, "second.txt"),
    "teen",
    "29.97"
  )

  result <- synchrony_moments_pipeline(root)

  expect_equal(result$videos$fps, 30)
  expect_equal(result$results[[1]]$coded_data$metadata$fps, 30L)
})
