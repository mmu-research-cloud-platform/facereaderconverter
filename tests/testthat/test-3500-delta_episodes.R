TEST_DATA <- Sys.getenv("TEST_DATA")

load(file.path(TEST_DATA, "test_data.RDa"))

test_that("delta_episodes, csv", {
  coding_df <- read.csv("testdata/testdata_detailed.csv") |>
    dplyr::mutate(id = 1, subject = "parent")

  coding_df2 <- coding_df |>
    tidyr::pivot_longer(
      cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
      names_to = "emotion",
      values_to = "value"
    )
  coding <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.1,
    delta_window = 0.1,
    fps = 30
  ))

  expect_no_error(delta_episodes(coding))

  x1 <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.1,
    delta_window = 0.1,
    fps = 30
  )) |>
    delta_episodes() |>
    nrow()

  x2 <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.05,
    delta_window = 0.2,
    fps = 30
  )) |>
    delta_episodes() |>
    nrow()

  expect_false(x1 == x2)

  expected_cols <- c(
    "id",
    "subject",
    "emotion",
    "start_frame",
    "end_frame",
    "start_time",
    "end_time",
    "duration_s",
    "run_id",
    "n_frames"
  )

  x <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.05,
    delta_window = 0.2,
    fps = 30
  )) |>
    delta_episodes()
  expect_true(all(
    expected_cols %in%
      names(x)
  ))
})


test_that("delta_episodes, rda", {
  coding_df <- test_coding

  coding_df2 <- test_coding_wide
  coding <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.1,
    delta_window = 0.1,
    fps = 30
  ))

  expect_no_error(delta_episodes(coding))

  x1 <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.1,
    delta_window = 0.1,
    fps = 30
  )) |>
    delta_episodes() |>
    nrow()

  x2 <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.05,
    delta_window = 0.2,
    fps = 30
  )) |>
    delta_episodes() |>
    nrow()

  expect_false(x1 == x2)

  expected_cols <- c(
    "id",
    "subject",
    "emotion",
    "start_frame",
    "end_frame",
    "start_time",
    "end_time",
    "duration_s",
    "run_id",
    "n_frames"
  )

  x <- suppressWarnings(add_delta_column(
    coding_df2,
    delta = 0.05,
    delta_window = 0.2,
    fps = 30
  )) |>
    delta_episodes()
  expect_true(all(
    expected_cols %in%
      names(x)
  ))
})
