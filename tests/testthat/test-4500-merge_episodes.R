TEST_DATA <- Sys.getenv("TEST_DATA")
test_data_path <- file.path(TEST_DATA, "test_data.RDa")
test_data_error <- tryCatch(
  {
    load(test_data_path)
    NULL
  },
  error = identity
)
if (inherits(test_data_error, "error")) {
  testthat::skip(sprintf(
    "Could not load test data fixtures: %s",
    test_data_error$message
  ))
}

library(data.table)
library(testthat)

make_part <- function(id_value, ...) {
  x <- convert_to_episodes(test_coding, fps = 12L, cores = 1L, ...)
  id_value <- as(id_value, class(x$coding$id)[1])
  x$coding[, id := id_value]
  x$episodes[, id := id_value]
  x$deltas[, id := id_value]
  x
}

test_that("merge_episodes binds tables and keeps metadata once", {
  a <- make_part(9001)
  b <- make_part(9002)

  merged <- merge_episodes(list(a, b))

  expect_s3_class(merged, "fr_coding")
  expect_identical(merged$metadata, a$metadata)
  expect_equal(nrow(merged$coding), nrow(a$coding) + nrow(b$coding))
  expect_equal(nrow(merged$episodes), nrow(a$episodes) + nrow(b$episodes))
  expect_equal(nrow(merged$deltas), nrow(a$deltas) + nrow(b$deltas))
})

test_that("merge_episodes reads objects from memory by name", {
  a <- make_part(9001)
  b <- make_part(9002)

  expect_equal(merge_episodes(c("a", "b")), merge_episodes(list(a, b)))
  expect_equal(
    merge_episodes(list(quote(a), "b")),
    merge_episodes(list(a, b))
  )
  expect_error(merge_episodes(c("a", "missing_obj")), "not found")
})

test_that("merge_episodes keeps run_id and delta_id unique", {
  a <- make_part(9001)
  b <- make_part(9002)
  merged <- merge_episodes(list(a, b))

  expect_false(anyDuplicated(merged$episodes$run_id) > 0L)
  expect_false(anyDuplicated(merged$deltas$delta_id) > 0L)
  expect_true(all(
    na.omit(unique(merged$coding$run_id)) %in% merged$episodes$run_id
  ))
})

test_that("merge_episodes errors on metadata mismatch", {
  a <- make_part(9001)
  b <- make_part(9002)
  b$metadata$fps <- 30L

  expect_error(merge_episodes(list(a, b)), "Metadata mismatch")
})

test_that("merge_episodes errors on duplicated subject/id/emotion", {
  a <- make_part(9001)
  b <- make_part(9001)

  expect_error(merge_episodes(list(a, b)), "Duplicated subject/id/emotion")
})

test_that("merge_episodes validates input", {
  a <- make_part(9001)

  expect_error(merge_episodes(a), "list of fr_coding")
  expect_error(merge_episodes(list(a, 1)), "not fr_coding")
  expect_error(merge_episodes(list()), "at least one")
})

test_that("merge_episodes real fixtures", {
  FR10 <- convert_directory_to_episodes(
    inpath = file.path(TEST_DATA, "FR10"),
    T_up = 0.15,
    T_down = 0.05,
    filter_name = "^.*8895.*$",
    overwrite = TRUE
  )
  FR9 <- convert_directory_to_episodes(
    inpath = file.path(TEST_DATA, "FR9"),
    T_up = 0.15,
    T_down = 0.05,
    filter_name = "^.*8895.*$",
    overwrite = TRUE
  )
  expect_no_error(merge_episodes(list(FR10, FR9)))
  expect_no_error(merge_episodes(c("FR10", "FR9")))
})
