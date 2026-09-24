library(testthat)

test_that("synchrony_moments_pipeline reports completed failed and unmatched videos", {
  root <- tempfile("synchrony-video-status-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  file.create(
    file.path(root, "completed.mp4"),
    file.path(root, "failed.mp4"),
    file.path(root, "unmatched.mp4")
  )
  write_detailed_export(
    file.path(root, "completed-parent.txt"),
    "completed.mp4",
    "parent"
  )
  write_detailed_export(
    file.path(root, "completed-teen.txt"),
    "completed.mp4",
    "teen"
  )
  write_detailed_export(
    file.path(root, "failed-parent.txt"),
    "failed.mp4",
    "parent"
  )

  expect_warning(
    result <- synchrony_moments_pipeline(root),
    "found 1 sufficiently matched outputs out of 1 detailed outputs"
  )

  expect_equal(
    result$videos[, .(video_filename, n_matched_outputs, status)],
    data.table::data.table(
      video_filename = c("completed.mp4", "failed.mp4", "unmatched.mp4"),
      n_matched_outputs = c(2L, 1L, 0L),
      status = c("completed", "failed", "unmatched")
    )
  )
  expect_equal(names(result$results), "video_001")
})
