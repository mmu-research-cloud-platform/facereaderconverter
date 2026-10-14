TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

# Small slice of the wide fixture: two ids, first rows of each subject
slice_ids <- function(ids = c(1, 3), n = 3000L) {
  dt <- data.table::as.data.table(test_coding_wide)[id %in% ids]
  dt[, .SD[seq_len(min(.N, n))], by = .(id, subject)]
}

drop_ids <- function(x) {
  x[, setdiff(names(x), c("run_id", "delta_id")), with = FALSE]
}

test_that("non-integer scalar fps is accepted and stored unrounded", {
  res <- convert_to_episodes(slice_ids(1), fps = 29.97, cores = 1L)
  expect_equal(res$metadata$fps, 29.97)
  expect_equal(
    res$episodes$duration_s,
    res$episodes$n_frames / 29.97
  )
  expect_equal(
    res$coding$frame[1:5],
    parse_time_to_frame(res$coding$video_time[1:5], 29.97)
  )
})

test_that("integer-valued scalar fps is stored as an integer", {
  res <- convert_to_episodes(slice_ids(1), fps = 30, cores = 1L)
  expect_identical(res$metadata$fps, 30L)
})

test_that("invalid scalar fps still errors", {
  expect_error(
    convert_to_episodes(slice_ids(1), fps = 0),
    "`fps` must be a positive integer scalar."
  )
  expect_error(
    convert_to_episodes(slice_ids(1), fps = c(30, 25)),
    "single number"
  )
})

test_that("per-id fps matches converting each id separately", {
  dt <- slice_ids(c(1, 3))
  mixed <- convert_to_episodes(
    dt,
    fps = c("1" = 30, "3" = 25),
    cores = 1L
  )
  a <- convert_to_episodes(dt[id == 1], fps = 30, cores = 1L)
  b <- convert_to_episodes(dt[id == 3], fps = 25, cores = 1L)

  expect_equal(
    drop_ids(mixed$episodes[id == 1]),
    drop_ids(a$episodes)
  )
  expect_equal(
    drop_ids(mixed$episodes[id == 3]),
    drop_ids(b$episodes)
  )
  expect_equal(
    drop_ids(mixed$deltas[id == 3]),
    drop_ids(b$deltas)
  )
  expect_equal(mixed$coding[id == 3]$frame, b$coding$frame)
  expect_equal(mixed$coding[id == 1]$delta, a$coding$delta)
})

test_that("mixed fps is stored as a per-id table in metadata", {
  mixed <- convert_to_episodes(
    slice_ids(c(1, 3), n = 500L),
    fps = c("1" = 30, "3" = 29.97),
    cores = 1L
  )
  tbl <- mixed$metadata$fps
  expect_s3_class(tbl, "data.table")
  expect_equal(tbl$id[order(tbl$id)], c("1", "3"))
  expect_equal(tbl$fps[match(c("1", "3"), tbl$id)], c(30, 29.97))
})

test_that("durations use each id's own fps", {
  mixed <- convert_to_episodes(
    slice_ids(c(1, 3)),
    fps = c("1" = 30, "3" = 25),
    cores = 1L
  )
  ep <- mixed$episodes
  expect_gt(nrow(ep[id == 1]), 0L)
  expect_gt(nrow(ep[id == 3]), 0L)
  expect_equal(ep[id == 1]$duration_s, ep[id == 1]$n_frames / 30)
  expect_equal(ep[id == 3]$duration_s, ep[id == 3]$n_frames / 25)
})

test_that("stored metadata is inherited when reconverting an fr_coding", {
  mixed <- convert_to_episodes(
    slice_ids(c(1, 3), n = 1500L),
    fps = c("1" = 30, "3" = 25),
    cores = 1L
  )
  again <- convert_to_episodes(mixed, T_up = 0.3, cores = 1L)
  expect_equal(
    again$metadata$fps[order(id)],
    mixed$metadata$fps[order(id)]
  )
  expect_equal(
    again$episodes[id == 3]$duration_s,
    again$episodes[id == 3]$n_frames / 25
  )
})

test_that("an fps argument that conflicts with metadata warns and loses", {
  base <- convert_to_episodes(slice_ids(1, n = 1500L), fps = 25, cores = 1L)
  expect_warning(
    res <- convert_to_episodes(base, fps = 30, cores = 1L),
    "conflicts with stored metadata"
  )
  expect_identical(res$metadata$fps, 25L)
})

test_that("fps is inferred from time stamps when not supplied", {
  expect_message(
    res <- convert_to_episodes(slice_ids(1), cores = 1L),
    "inferred"
  )
  expect_identical(res$metadata$fps, 30L)
})

test_that("fps falls back to 30 when inference is impossible", {
  tiny <- slice_ids(1, n = 5L)
  res <- convert_to_episodes(tiny, cores = 1L)
  expect_identical(res$metadata$fps, 30L)
})

test_that("frame input without video_time uses the supplied or default fps", {
  dt <- slice_ids(1, n = 500L)
  dt[, frame := parse_time_to_frame(video_time, 30)]
  dt[, video_time := NULL]
  res <- convert_to_episodes(dt, fps = 30, cores = 1L)
  expect_identical(res$metadata$fps, 30L)
})

test_that("old scalar-metadata objects keep working", {
  x <- convert_to_episodes(slice_ids(1, n = 800L), fps = 30L, cores = 1L)
  expect_identical(x$metadata$fps, 30L)
  expect_no_warning(convert_to_episodes(x, cores = 1L))
})
