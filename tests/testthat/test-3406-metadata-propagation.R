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

test_that("convert_to_episodes exports complete metadata", {
  coded_data <- convert_to_episodes(test_coding, fps = 12L, cores = 1L)

  expect_s3_class(coded_data, "fr_coding")
  expect_identical(coded_data$metadata$schema_version, 1L)
  expect_identical(coded_data$metadata$fps, 12L)
  expect_identical(coded_data$metadata$cores, 1L)
  expect_true(all(
    c(
      "consecutive_missing",
      "delta",
      "delta_window",
      "min_dur_sec",
      "T_down",
      "T_up"
    ) %in%
      names(coded_data$metadata)
  ))
})

test_that("metadata fps is authoritative for synchrony results", {
  coded_data <- convert_to_episodes(test_coding, fps = 12L)

  result <- synchrony_by_episode(
    coded_data,
    time_limit = 1,
    fps = 30L,
    exclude_emotions = NULL
  )

  expect_equal(
    result,
    synchrony_by_episode(
      coded_data,
      time_limit_frames = 12L,
      exclude_emotions = NULL
    )
  )
})

test_that("metadata fps is inherited by negative controls", {
  coded_data <- convert_to_episodes(test_coding, fps = 12L)
  episodes <- synchrony_by_episode(
    coded_data,
    constraint_method = "frames",
    time_limit_frames = 12L,
    exclude_emotions = NULL
  )

  set.seed(7349)
  inherited <- negative_controls(
    coded_data,
    episodes,
    constraint_method = "frames",
    time_limit = 1,
    fps = 30L,
    exclude_emotions = NULL
  )
  set.seed(7349)
  explicit <- negative_controls(
    coded_data,
    episodes,
    constraint_method = "frames",
    time_limit_frames = 12L,
    exclude_emotions = NULL
  )

  expect_equal(inherited, explicit)
})

test_that("metadata is preserved through coded-data transformations", {
  coded_data <- convert_to_episodes(test_coding, fps = 12L)
  locf_data <- locf(coded_data, fps = 30L)
  delta_data <- delta_episodes(coded_data, fps = 30L)

  expect_identical(locf_data$metadata$fps, 12L)
  expect_true(is.data.table(delta_data))
  expect_true(is.data.table(reaction_rate(coded_data, fps = 30L)))
})
