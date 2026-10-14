TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

slice_ids <- function(ids = c(1, 3), n = 3000L) {
  dt <- data.table::as.data.table(test_coding_wide)[id %in% ids]
  dt[, .SD[seq_len(min(.N, n))], by = .(id, subject)]
}

fps_map <- c("1" = 30, "3" = 25)
dt_mixed <- slice_ids(c(1, 3))
mixed <- convert_to_episodes(dt_mixed, fps = fps_map, cores = 1L)
single_1 <- convert_to_episodes(dt_mixed[id == 1], fps = 30, cores = 1L)
single_3 <- convert_to_episodes(dt_mixed[id == 3], fps = 25, cores = 1L)

strip <- function(x) {
  x[, setdiff(names(x), c("run_id", "delta_id")), with = FALSE]
}

# delta_episodes ------------------------------------------------------------

test_that("delta_episodes uses each id's fps from metadata", {
  res <- delta_episodes(mixed, cores = 1L)
  expect_gt(nrow(res[id == 1]), 0L)
  expect_gt(nrow(res[id == 3]), 0L)
  expect_equal(res[id == 1]$duration_s, res[id == 1]$n_frames / 30)
  expect_equal(res[id == 3]$duration_s, res[id == 3]$n_frames / 25)
})

test_that("delta_episodes accepts a per-id fps argument for plain tables", {
  coding <- data.table::copy(mixed$coding)
  coding[, c("run_id", "delta_id") := NULL]
  res <- delta_episodes(coding, fps = fps_map, cores = 1L)
  expect_equal(res[id == 3]$duration_s, res[id == 3]$n_frames / 25)
})

# locf ----------------------------------------------------------------------

test_that("locf durations use each id's fps and metadata is preserved", {
  res <- locf(mixed)
  ep <- res$episodes
  expect_equal(ep[id == 1]$duration_s, ep[id == 1]$n_frames / 30)
  expect_equal(ep[id == 3]$duration_s, ep[id == 3]$n_frames / 25)
  expect_s3_class(res$metadata$fps, "data.table")
})

test_that("locf on a plain table records the resolved fps in metadata", {
  plain <- data.table::copy(single_3$coding)
  res <- locf(plain, fps = 25, consecutive_missing = 90)
  expect_identical(res$metadata$fps, 25L)
  expect_equal(res$episodes$duration_s, res$episodes$n_frames / 25)
})

test_that("locf warns when fps argument conflicts with metadata", {
  expect_warning(locf(single_3, fps = 30), "conflicts with stored metadata")
})

# add_delta_column ----------------------------------------------------------

test_that("add_delta_column computes the lag per id", {
  # id 3 is treated as 60 fps so rounded frames stay unique
  cols <- c("id", "subject", "video_time", "emotion", "value")
  long <- data.table::rbindlist(list(
    data.table::copy(single_1$coding)[, .SD, .SDcols = cols],
    data.table::copy(single_3$coding)[, .SD, .SDcols = cols]
  ))
  per_id <- c("1" = 30, "3" = 60)
  res <- suppressWarnings(add_delta_column(
    long,
    fps = per_id,
    delta_window = 0.2,
    delta = 0.1,
    cores = 1L
  ))
  a <- suppressWarnings(add_delta_column(long[id == 1], fps = 30, cores = 1L))
  b <- suppressWarnings(add_delta_column(long[id == 3], fps = 60, cores = 1L))
  expect_equal(res[id == 1], a)
  expect_equal(res[id == 3], b)
  b30 <- suppressWarnings(add_delta_column(long[id == 3], fps = 30, cores = 1L))
  expect_false(identical(b, b30))
})

# synchrony -----------------------------------------------------------------

test_that("synchrony with mixed fps matches per-id results", {
  m <- synchrony(mixed, time_limit = 3, constraint_method = "strict")
  a <- synchrony(single_1, time_limit = 3, constraint_method = "strict")
  b <- synchrony(single_3, time_limit = 3, constraint_method = "strict")
  key <- c(
    "id",
    "denominator",
    "numerator",
    "emotion",
    "start_frame",
    "end_frame",
    "synchrony"
  )
  cols <- intersect(key, names(m))
  expect_equal(m[id == 1, ..cols], a[, .SD, .SDcols = cols], ignore_attr = TRUE)
  expect_equal(m[id == 3, ..cols], b[, .SD, .SDcols = cols], ignore_attr = TRUE)
})

test_that("time_limit becomes a different number of frames per id", {
  inputs <- prepare_synchrony_inputs(
    mixed,
    time_limit = 3,
    constraint_method = "strict"
  )
  expect_equal(inputs$limit_frames, c("1" = 90L, "3" = 75L))
})

test_that("synchrony with a scalar rate still gives a scalar frame limit", {
  inputs <- prepare_synchrony_inputs(single_1, time_limit = 3)
  expect_identical(inputs$limit_frames, 90L)
})

test_that("synchrony accepts non-integer fps", {
  x <- convert_to_episodes(slice_ids(1), fps = 29.97, cores = 1L)
  inputs <- prepare_synchrony_inputs(x, time_limit = 3)
  expect_identical(inputs$limit_frames, 90L)
  expect_no_error(synchrony(x, time_limit = 3))
})

test_that("synchrony errors on an invalid fps scalar", {
  expect_error(
    synchrony(single_1, fps = 0),
    "must be a positive numeric scalar"
  )
})

# reaction rate -------------------------------------------------------------

test_that("reaction_rate with mixed fps matches per-id results", {
  m <- reaction_rate(mixed, time_limit = 3, constraint_method = "strict")
  a <- reaction_rate(single_1, time_limit = 3, constraint_method = "strict")
  b <- reaction_rate(single_3, time_limit = 3, constraint_method = "strict")
  expect_equal(m[id == 1], a, ignore_attr = TRUE)
  expect_equal(m[id == 3], b, ignore_attr = TRUE)
})

test_that("reaction_rate_by_episode with mixed fps matches per-id results", {
  m <- reaction_rate_by_episode(
    mixed,
    time_limit = 3,
    constraint_method = "strict"
  )
  a <- reaction_rate_by_episode(
    single_1,
    time_limit = 3,
    constraint_method = "strict"
  )
  b <- reaction_rate_by_episode(
    single_3,
    time_limit = 3,
    constraint_method = "strict"
  )
  cols <- setdiff(names(m), "run_id")
  expect_equal(m[id == 3, ..cols], b[, .SD, .SDcols = cols], ignore_attr = TRUE)
  expect_equal(m[id == 1, ..cols], a[, .SD, .SDcols = cols], ignore_attr = TRUE)
})

test_that("exclude_start and time_limit are converted per id", {
  inputs <- prepare_reaction_rate_inputs(
    mixed,
    time_limit = 3,
    exclude_start = 0.2,
    constraint_method = "strict"
  )
  expect_equal(inputs$limit_frames, c("1" = 90L, "3" = 75L))
  expect_equal(inputs$start_frames, c("1" = 6L, "3" = 5L))
})

test_that("reaction_rate accepts non-integer fps", {
  x <- convert_to_episodes(slice_ids(c(1, 3)), fps = 29.97, cores = 1L)
  expect_no_error(reaction_rate(x, time_limit = 3))
})

test_that("explicit frame limits are not rescaled per id", {
  inputs <- prepare_reaction_rate_inputs(
    mixed,
    time_limit_frames = 60L,
    exclude_start_frames = 2L,
    constraint_method = "frames"
  )
  expect_identical(inputs$limit_frames, 60L)
  expect_identical(inputs$start_frames, 2L)
})

test_that("a conflicting fps argument warns and metadata wins for mixed ids", {
  expect_warning(
    inputs <- prepare_reaction_rate_inputs(
      mixed,
      time_limit = 3,
      fps = 60,
      constraint_method = "strict"
    ),
    "conflicts with stored metadata"
  )
  expect_equal(inputs$limit_frames, c("1" = 90L, "3" = 75L))
})
