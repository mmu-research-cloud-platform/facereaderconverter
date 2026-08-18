test_that("delta_episodes", {
  coding_df <- read.csv("testdata/testdata_detailed.csv") |>
    dplyr::mutate(id = 1, subject = "parent")

  coding_df2 <- coding_df |>
    tidyr::pivot_longer(
      cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
      names_to = "emotion",
      values_to = "value"
    )
  coding <- coding_df2 |>
    add_delta_column(delta = 0.1, delta_window = 0.1, fps = 30)

  expect_no_error(delta_episodes(coding))

  x1 <- coding_df2 |>
    add_delta_column(delta = 0.1, delta_window = 0.1, fps = 30) |>
    delta_episodes() |>
    nrow()

  x2 <- coding_df2 |>
    add_delta_column(delta = 0.05, delta_window = 0.2, fps = 30) |>
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

  x <- coding_df2 |>
    add_delta_column(delta = 0.05, delta_window = 0.2, fps = 30) |>
    delta_episodes()
  expect_true(all(
    expected_cols %in%
      names(x)
  ))
})
