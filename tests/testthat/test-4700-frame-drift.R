TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

drift_input <- function(fps, n = 600L) {
  one <- test_coding_wide[test_coding_wide$id == 1, ][seq_len(n), ]
  data.frame(
    id = one$id,
    video_time = one$video_time,
    frame = parse_time_to_frame(one$video_time, fps)
  )
}

test_that("parse_time_to_seconds is unrounded and vectorised", {
  expect_equal(
    parse_time_to_seconds(c("00:01:23.5", "1:23.5", "83.5")),
    rep(83.5, 3)
  )
  expect_equal(parse_time_to_seconds("00:00:00.033"), 0.033)
  expect_true(is.na(parse_time_to_seconds(NA)))
  expect_error(parse_time_to_seconds("1:2:3:4"), "Unrecognized")
})

test_that("parse_time_to_frame still rounds seconds * fps once", {
  secs <- parse_time_to_seconds(test_coding_wide$video_time[1:50])
  expect_identical(
    parse_time_to_frame(test_coding_wide$video_time[1:50], 29.97),
    as.integer(round(secs * 29.97))
  )
})

test_that("assess_frame_drift is within half a frame for derived frames", {
  res <- expect_no_warning(assess_frame_drift(drift_input(30), fps = 30))
  expect_equal(nrow(res), 1L)
  expect_equal(res$id, "1")
  expect_equal(res$fps, 30)
  expect_lte(res$max_abs_drift_frames, 0.5 + 1e-8)
  expect_lte(res$max_abs_drift_s, 0.5 / 30 + 1e-8)
})

test_that("assess_frame_drift handles non-integer fps", {
  res <- expect_no_warning(assess_frame_drift(drift_input(29.97), fps = 29.97))
  expect_equal(res$fps, 29.97)
})

test_that("assess_frame_drift warns when fps is wrong", {
  # Frames derived at 30 fps but assessed at 25 fps
  expect_warning(
    res <- assess_frame_drift(drift_input(30), fps = 25),
    "Drift exceeds half a frame"
  )
  expect_gt(res$max_abs_drift_frames, 0.5)
})

test_that("assess_frame_drift works per id with mixed fps", {
  a <- drift_input(30)
  b <- drift_input(25)
  b$id <- 2
  res <- assess_frame_drift(
    rbind(a, b),
    fps = c("1" = 30, "2" = 25)
  )
  expect_setequal(res$id, c("1", "2"))
  expect_equal(res$fps[match(c("1", "2"), res$id)], c(30, 25))
})

test_that("assess_frame_drift accepts fr_coding objects", {
  x <- test_data_sync2
  x$deltas <- test_deltas[test_deltas$id %in% c(1, 3), ]
  res <- expect_no_warning(assess_frame_drift(x))
  expect_setequal(res$id, c("1", "3"))
  expect_true(all(res$fps == 30))
})

test_that("assess_frame_drift validates input", {
  expect_error(
    assess_frame_drift(data.frame(id = 1, frame = 0L)),
    "must have columns"
  )
})
