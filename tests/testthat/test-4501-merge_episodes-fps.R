TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(data.table)

slice_part <- function(id_value, fps, n = 1500L) {
  dt <- as.data.table(test_coding_wide)[id == id_value]
  dt <- dt[, .SD[seq_len(min(.N, n))], by = .(id, subject)]
  convert_to_episodes(dt, fps = fps, cores = 1L)
}

test_that("merge_episodes combines different fps for different ids into a table", {
  a <- slice_part(1, 30)
  b <- slice_part(3, 25)

  merged <- merge_episodes(list(a, b))
  fps <- merged$metadata$fps

  expect_s3_class(fps, "data.table")
  expect_equal(fps$fps[match(c("1", "3"), fps$id)], c(30, 25))
  expect_equal(nrow(merged$coding), nrow(a$coding) + nrow(b$coding))
})

test_that("merge_episodes keeps a scalar when every id shares one rate", {
  a <- slice_part(1, 30)
  b <- slice_part(3, 30)

  merged <- merge_episodes(list(a, b))
  expect_identical(merged$metadata$fps, 30L)
})

test_that("merge_episodes keeps a non-integer shared rate unrounded", {
  a <- slice_part(1, 29.97)
  b <- slice_part(3, 29.97)

  expect_equal(merge_episodes(list(a, b))$metadata$fps, 29.97)
})

test_that("merge_episodes errors when the same id has different fps", {
  a <- slice_part(1, 30)
  b <- slice_part(1, 25)

  expect_error(
    merge_episodes(list(a, b)),
    "Conflicting frame rates for the same id.*id=1"
  )
})

test_that("merge_episodes can merge an fps table with a scalar object", {
  a <- slice_part(1, 30)
  b <- slice_part(3, 25)
  c_part <- slice_part(6, 30)

  merged <- merge_episodes(list(merge_episodes(list(a, b)), c_part))
  fps <- merged$metadata$fps
  expect_equal(fps$fps[match(c("1", "3", "6"), fps$id)], c(30, 25, 30))
})

test_that("other metadata differences still error", {
  a <- slice_part(1, 30)
  b <- slice_part(3, 30)
  b$metadata$T_up <- 0.5

  expect_error(merge_episodes(list(a, b)), "Metadata mismatch")
})

test_that("merged mixed-rate objects work downstream", {
  merged <- merge_episodes(list(slice_part(1, 30), slice_part(3, 25)))
  expect_no_error(delta_episodes(merged, cores = 1L))
  expect_no_error(suppressWarnings(
    synchrony(merged, time_limit = 3, constraint_method = "strict")
  ))
})
