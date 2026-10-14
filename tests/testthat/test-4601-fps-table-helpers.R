TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

test_that("as_fps_table expands a scalar to all ids", {
  tbl <- as_fps_table(30, ids = c("a", "b", "a"))
  expect_equal(tbl$id, c("a", "b"))
  expect_equal(tbl$fps, c(30, 30))
})

test_that("as_fps_table accepts named vectors and data frames", {
  expect_equal(as_fps_table(c(a = 30, b = 29.97))$fps, c(30, 29.97))
  tbl <- as_fps_table(data.frame(id = c(1, 2), fps = c(25, 29.97)))
  expect_equal(tbl$id, c("1", "2"))
  expect_equal(tbl$fps, c(25, 29.97))
  expect_equal(nrow(as_fps_table(NULL)), 0L)
})

test_that("as_fps_table rejects unsupported input", {
  expect_error(as_fps_table(c(30, 25)), "single number")
  expect_error(as_fps_table(data.frame(x = 1)), "columns `id` and `fps`")
})

test_that("validate_fps_table enforces positive finite fps and unique ids", {
  good <- data.table::data.table(id = c("a", "b"), fps = c(30, 29.97))
  expect_silent(validate_fps_table(good))
  expect_error(
    validate_fps_table(data.table::data.table(id = c("a", "a"), fps = 30)),
    "one row per"
  )
  expect_error(
    validate_fps_table(data.table::data.table(id = "a", fps = 0)),
    "finite and positive"
  )
  expect_error(
    validate_fps_table(data.table::data.table(id = "a", fps = NA_real_)),
    "finite and positive"
  )
})

test_that("resolve_fr_fps uses metadata, then argument, then default", {
  ids <- c(1, 3, 6)
  expect_equal(resolve_fr_fps(ids = ids)$fps, rep(30, 3))
  expect_equal(resolve_fr_fps(25, ids = ids)$fps, rep(25, 3))

  x <- test_data_sync2
  x$metadata$fps <- data.table::data.table(id = c("1", "3"), fps = c(29.97, 25))
  expect_warning(
    res <- resolve_fr_fps(60, x = x, ids = ids),
    "conflicts with stored metadata"
  )
  expect_equal(res$id, c("1", "3", "6"))
  expect_equal(res$fps, c(29.97, 25, 60))
})

test_that("resolve_fr_fps expands scalar metadata from older objects", {
  x <- test_data_sync2
  x$metadata$fps <- 30L
  ids <- unique(x$coding$id)
  res <- resolve_fr_fps(ids = ids, x = x)
  expect_equal(nrow(res), length(unique(ids)))
  expect_true(all(res$fps == 30))
})

test_that("resolve_fr_fps warns when argument conflicts with metadata", {
  x <- test_data_sync2
  x$metadata$fps <- c("1" = 30)
  expect_warning(
    res <- resolve_fr_fps(25, x = x, ids = 1),
    "conflicts with stored metadata"
  )
  expect_equal(res$fps, 30)
  expect_no_warning(resolve_fr_fps(30, x = x, ids = 1))
})

test_that("fps tables tolerate an NA id group", {
  res <- resolve_fr_fps(30, ids = c(1, NA))
  expect_equal(nrow(res), 2L)
  expect_equal(fps_for_ids(c(NA, 1), res), c(30, 30))
})

test_that("fps_for_ids looks up per-row fps and errors on missing ids", {
  tbl <- data.table::data.table(id = c("1", "2"), fps = c(30, 29.97))
  expect_equal(fps_for_ids(c(2, 1, 2), tbl), c(29.97, 30, 29.97))
  expect_error(fps_for_ids(c(1, 9), tbl), "No fps available for id")
})

test_that("parse_time_to_frame accepts non-integer and per-row fps", {
  expect_identical(parse_time_to_frame("00:01:00", fps = 29.97), 1798L)
  expect_identical(
    parse_time_to_frame(c("00:00:10", "00:00:10"), fps = c(30, 25)),
    c(300L, 250L)
  )
  wide <- test_coding_wide[test_coding_wide$id == 1, ][1:3, ]
  expect_identical(
    parse_time_to_frame(wide$video_time, fps = 30),
    c(0L, 1L, 2L)
  )
})
