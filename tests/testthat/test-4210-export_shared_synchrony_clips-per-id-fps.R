TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)

clip_coding <- function(fps) {
  structure(
    list(metadata = list(fps = fps)),
    class = c("fr_coding", "list")
  )
}

clip_intervals <- function() {
  data.table(
    id = c(1L, 3L),
    emotion = c("happy", "happy"),
    subject1 = "parent",
    subject2 = "teen",
    subject1_run_id = 1L,
    subject2_run_id = 1L,
    start_frame = c(300L, 300L),
    end_frame = c(329L, 329L),
    combined_value = c(1.9, 1.8)
  )
}

with_videos <- function(code) {
  video <- tempfile(fileext = ".mp4")
  file.create(video)
  on.exit(unlink(video, force = TRUE), add = TRUE)
  code(c("1" = video, "3" = video))
}

test_that("clips use each id's own fps for buffers and times", {
  fps_tbl <- data.table(id = c("1", "3"), fps = c(30, 25))
  with_videos(function(paths) {
    res <- facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = clip_coding(fps_tbl),
      shared_synchrony = clip_intervals(),
      video_paths = paths,
      n = 0L,
      emotion = NULL,
      buffer = c(before = 2, after = 1),
      buffer_units = "seconds"
    )
    a <- res[id == 1]
    b <- res[id == 3]
    expect_equal(a$fps, 30)
    expect_equal(b$fps, 25)
    # 2 s before = 60 frames at 30 fps, 50 frames at 25 fps
    expect_equal(a$start_frame, 300L - 60L)
    expect_equal(b$start_frame, 300L - 50L)
    expect_equal(a$end_frame, 329L + 30L)
    expect_equal(b$end_frame, 329L + 25L)
    expect_equal(a$start_seconds, a$start_frame / 30)
    expect_equal(b$start_seconds, b$start_frame / 25)
    expect_equal(a$duration_seconds, (a$end_frame - a$start_frame + 1) / 30)
    expect_equal(b$duration_seconds, (b$end_frame - b$start_frame + 1) / 25)
  })
})

test_that("a scalar fps applies to every id", {
  with_videos(function(paths) {
    res <- facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = clip_coding(10L),
      shared_synchrony = clip_intervals(),
      video_paths = paths,
      n = 0L,
      emotion = NULL,
      buffer = 1,
      buffer_units = "seconds"
    )
    expect_equal(res$fps, c(10, 10))
    expect_equal(res$start_frame, c(290L, 290L))
  })
})

test_that("non-integer fps is used unrounded in clip times", {
  with_videos(function(paths) {
    res <- facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = clip_coding(c("1" = 29.97, "3" = 29.97)),
      shared_synchrony = clip_intervals(),
      video_paths = paths,
      n = 0L,
      emotion = NULL,
      buffer = 0,
      buffer_units = "seconds"
    )
    expect_equal(res$fps, c(29.97, 29.97))
    expect_equal(res$start_seconds, rep(300 / 29.97, 2))
    expect_equal(res$duration_seconds, rep(30 / 29.97, 2))
  })
})

test_that("frame buffers are not rescaled per id", {
  fps_tbl <- data.table(id = c("1", "3"), fps = c(30, 25))
  with_videos(function(paths) {
    res <- facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = clip_coding(fps_tbl),
      shared_synchrony = clip_intervals(),
      video_paths = paths,
      n = 0L,
      emotion = NULL,
      buffer = 10,
      buffer_units = "frames"
    )
    expect_equal(res$start_frame, c(290L, 290L))
    expect_equal(res$end_frame, c(339L, 339L))
  })
})

test_that("video durations are capped using each id's fps", {
  fps_tbl <- data.table(id = c("1", "3"), fps = c(30, 25))
  intervals <- clip_intervals()
  intervals$start_frame <- 200L
  intervals$end_frame <- 329L
  with_videos(function(paths) {
    durations <- setNames(11, normalizePath(paths[[1L]], winslash = "/"))
    res <- facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = clip_coding(fps_tbl),
      shared_synchrony = intervals,
      video_paths = paths,
      n = 0L,
      emotion = NULL,
      buffer = 0,
      buffer_units = "frames",
      video_durations = durations
    )
    # Last frame is ceiling(11 * fps) - 1: 329 at 30 fps, 274 at 25 fps
    expect_equal(res[res$id == 1, ]$end_frame, 329L)
    expect_equal(res[res$id == 3, ]$end_frame, 274L)
  })
})

test_that("ids with no fps in the table error clearly", {
  fps_tbl <- data.table(id = "1", fps = 30)
  with_videos(function(paths) {
    expect_error(
      facereaderconverter:::prepare_shared_synchrony_clips(
        coded_data = clip_coding(fps_tbl),
        shared_synchrony = clip_intervals(),
        video_paths = paths,
        n = 0L,
        emotion = NULL
      ),
      "No fps available for id"
    )
  })
})

test_that("invalid fps metadata is rejected", {
  with_videos(function(paths) {
    expect_error(
      facereaderconverter:::prepare_shared_synchrony_clips(
        coded_data = clip_coding(-5),
        shared_synchrony = clip_intervals(),
        video_paths = paths,
        n = 0L,
        emotion = NULL
      ),
      "positive numbers"
    )
  })
})

test_that("fr_coding from convert_to_episodes works with mixed-fps export prep", {
  dt <- as.data.table(test_coding_wide)[id %in% c(1, 3)]
  dt <- dt[, .SD[seq_len(min(.N, 3000L))], by = .(id, subject)]
  coded <- convert_to_episodes(
    dt,
    fps = c("1" = 30, "3" = 25),
    cores = 1L
  )
  shared <- suppressWarnings(shared_synchronous_episodes(
    coded,
    time_limit = 3,
    constraint_method = "strict"
  ))
  skip_if(nrow(shared) == 0L, "No shared episodes in this slice.")
  with_videos(function(paths) {
    res <- facereaderconverter:::prepare_shared_synchrony_clips(
      coded_data = coded,
      shared_synchrony = shared,
      video_paths = paths[intersect(names(paths), as.character(shared$id))],
      n = 0L,
      emotion = NULL,
      buffer = 1,
      buffer_units = "seconds"
    )
    expect_true(all(res$fps[res$id == 1] == 30))
    expect_true(all(res$fps[res$id == 3] == 25))
  })
})
