library(testthat)

write_header_metadata_export <- function(
  path,
  type,
  video,
  participant = NULL
) {
  lines <- c(
    paste("Video analysis", type, "log"),
    "",
    "Face Model\tGeneral",
    "Calibration\t-",
    "Start time\t6/4/2026 13:31:06.331",
    paste("Filename", video, sep = "\t"),
    "Frame rate\t30.000000000"
  )
  if (identical(type, "detailed")) {
    lines <- c(
      lines,
      "",
      "Video Time\tNeutral\tHappy\tParticipant Name",
      paste("00:00:00.000\t0\t0.5", participant, sep = "\t"),
      paste("00:00:00.033\t0\t0.6", participant, sep = "\t")
    )
  }
  writeLines(lines, path)
}

test_that("synchrony_moments_pipeline matches detailed headers before loading data", {
  root <- tempfile("synchrony-header-metadata-")
  dir.create(file.path(root, "exports"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  video_name <- "ID100024_side_by_side.mp4"
  file.create(file.path(root, video_name))
  write_header_metadata_export(
    file.path(root, "exports", "parent-unrelated-name.txt"),
    "detailed",
    paste0("C:/FaceReader/source/", video_name),
    "parent"
  )
  write_header_metadata_export(
    file.path(root, "exports", "teen-unrelated-name.txt"),
    "detailed",
    paste0("C:/FaceReader/source/", video_name),
    "teen"
  )
  write_header_metadata_export(
    file.path(root, "exports", "state-output.txt"),
    "state",
    paste0("C:/FaceReader/source/", video_name)
  )

  result <- synchrony_moments_pipeline(root)

  expect_equal(result$videos$video_filename, video_name)
  expect_setequal(
    result$manifest[type == "detailed", participant],
    c("parent", "teen")
  )
  expect_equal(result$manifest[type == "state", status], "discovered")
})
