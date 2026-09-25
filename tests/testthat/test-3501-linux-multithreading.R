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

test_that("threaded coding functions match one-thread Linux results", {
  skip_if(Sys.info()[["sysname"]] != "Linux", "Linux/WSL only")

  coding <- test_coding_wide[seq_len(5000L), ]
  original_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(original_threads), add = TRUE)

  observed_threads <- integer()
  set_threads <- data.table::setDTthreads
  testthat::local_mocked_bindings(
    setDTthreads = function(threads, ...) {
      observed_threads <<- c(observed_threads, threads)
      set_threads(threads, ...)
    },
    .package = "data.table"
  )

  converted_one <- convert_to_episodes(coding, cores = 1L)
  converted_two <- convert_to_episodes(coding, cores = 2L)
  expect_equal(converted_two$coding, converted_one$coding)
  expect_equal(converted_two$episodes, converted_one$episodes)
  expect_equal(converted_two$deltas, converted_one$deltas)

  delta_one <- suppressWarnings(add_delta_column(coding, cores = 1L))
  delta_two <- suppressWarnings(add_delta_column(coding, cores = 2L))
  expect_equal(delta_two, delta_one)

  episodes_one <- delta_episodes(delta_one, cores = 1L)
  episodes_two <- delta_episodes(delta_two, cores = 2L)
  expect_equal(episodes_two, episodes_one)
  expect_identical(
    observed_threads[c(1L, 3L, 5L, 7L, 9L, 11L)],
    c(1L, 2L, 1L, 2L, 1L, 2L)
  )
  expect_identical(data.table::getDTthreads(), original_threads)
})

test_that("delta forwards Linux thread settings", {
  skip_if(Sys.info()[["sysname"]] != "Linux", "Linux/WSL only")

  coding <- test_coding_wide[seq_len(5000L), ]
  original_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(original_threads), add = TRUE)

  observed_threads <- integer()
  set_threads <- data.table::setDTthreads
  testthat::local_mocked_bindings(
    setDTthreads = function(threads, ...) {
      observed_threads <<- c(observed_threads, threads)
      set_threads(threads, ...)
    },
    .package = "data.table"
  )

  result <- suppressWarnings(delta(coding, cores = 2L))

  expect_true("delta" %in% names(result))
  expect_identical(observed_threads[[1]], 2L)
  expect_identical(data.table::getDTthreads(), original_threads)
})
