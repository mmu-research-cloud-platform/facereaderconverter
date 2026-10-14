TEST_DATA <- Sys.getenv("TEST_DATA")

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

test_that("header rates differing only by floating-point noise count as one rate", {
  fps <- c(29.97, 29.97 + 4.6e-14)
  expect_identical(data.table::uniqueN(fps), 2L)

  root <- tempfile("pipeline-fps-noise-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "session.mp4"))
  write_pipeline_subject_fps_export(
    file.path(root, "first.txt"),
    "parent",
    format(fps[[1L]], digits = 17)
  )
  write_pipeline_subject_fps_export(
    file.path(root, "second.txt"),
    "teen",
    format(fps[[2L]], digits = 17)
  )

  expect_no_warning(result <- synchrony_moments_pipeline(root))
  expect_equal(result$videos$status, "completed")
  expect_equal(result$videos$fps, 29.97, tolerance = 1e-6)
})

test_that("genuinely different header rates are still rejected", {
  root <- tempfile("pipeline-fps-different-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "session.mp4"))
  write_pipeline_subject_fps_export(
    file.path(root, "first.txt"),
    "parent",
    "29.97"
  )
  write_pipeline_subject_fps_export(file.path(root, "second.txt"), "teen", "30")

  expect_warning(
    result <- synchrony_moments_pipeline(root),
    "valid frame rates across 2 unique values"
  )
  expect_equal(result$videos$status, "failed")
})
