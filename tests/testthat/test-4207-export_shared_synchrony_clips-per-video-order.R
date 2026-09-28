library(data.table)
library(testthat)

test_that("clip order follows original timestamps independently for each video", {
  videos <- c(tempfile(fileext = ".mp4"), tempfile(fileext = ".mp4"))
  on.exit(unlink(videos, force = TRUE), add = TRUE)
  file.create(videos)
  coded_data <- structure(
    list(metadata = list(fps = 10L)),
    class = c("fr_coding", "list")
  )
  shared <- data.table(
    id = c(1L, 1L, 2L, 2L),
    emotion = rep("happy", 4L),
    subject1 = rep("parent", 4L),
    subject2 = rep("child", 4L),
    subject1_run_id = seq_len(4L),
    subject2_run_id = seq_len(4L),
    start_frame = c(40L, 10L, 30L, 5L),
    end_frame = c(44L, 14L, 34L, 9L),
    combined_value = c(2, 1, 4, 3)
  )

  result <- facereaderconverter:::prepare_shared_synchrony_clips(
    coded_data,
    shared,
    video_paths = stats::setNames(videos, c("1", "2")),
    n = 0L,
    emotion = "happy",
    buffer = 5,
    buffer_units = "frames"
  )

  expect_equal(result$id, c(2L, 2L, 1L, 1L))
  expect_equal(result$selection_rank, seq_len(4L))
  expect_equal(result$clip_order, c(2L, 1L, 2L, 1L))
  expect_true(all(startsWith(
    result$clip_filename,
    sprintf("%03d_order-%03d_", result$selection_rank, result$clip_order)
  )))
})
