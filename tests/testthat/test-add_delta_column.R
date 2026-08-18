test_that("add_delta_column", {
  # Test with different delta_window and delta values
  coding_df = read.csv("junk/testdata_detailed.csv") |>
    dplyr::mutate(id = 1, subject = "teen")

  coding_df2 = coding_df |>
    tidyr::pivot_longer(
      cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
      names_to = "emotion",
      values_to = "value"
    )

  expect_no_error({
    result <- add_delta_column(
      coding_df,
      delta_window = 0.1,
      delta = 0.1,
      fps = 30
    )
  })
  expect_no_error({
    result <- add_delta_column(
      coding_df2,
      delta_window = 0.1,
      delta = 0.1,
      fps = 30
    )
  })

  # Test that the result contains the 'delta' column
  result <- add_delta_column(
    coding_df,
    delta_window = 0.1,
    delta = 0.1,
    fps = 30
  )
  expect_true("delta" %in% names(result))

  # Test with different delta_window and delta values
  expect_no_error({
    result <- add_delta_column(
      coding_df,
      delta_window = 0.2,
      delta = 0.2,
      fps = 30
    )
  })

  # Test with edge cases
  # Empty data frame
  empty_df <- tibble::tibble(
    id = integer(),
    subject = character(),
    emotion = character(),
    frame = integer(),
    value = numeric()
  )
  result <- add_delta_column(
    empty_df,
    delta_window = 0.1,
    delta = 0.1,
    fps = 30
  )
  expect_true(nrow(result) == 0)

  # Single row data frame
  single_row_df <- tibble::tibble(
    id = 1,
    subject = "A",
    emotion = "happy",
    frame = 1,
    value = 0.1
  )
  result <- add_delta_column(
    single_row_df,
    delta_window = 0.1,
    delta = 0.1,
    fps = 30
  )
  expect_true(nrow(result) == 1)
  expect_true("delta" %in% names(result))

  # Test invalid inputs
  expect_error(
    add_delta_column(coding_df, delta_window = -0.1, delta = 0.1, fps = 30),
    "`delta_window` must be a numeric scalar >= 0."
  )
  expect_error(
    add_delta_column(coding_df, delta_window = 0.1, delta = -0.1, fps = 30),
    "`delta` must be a numeric scalar > 0."
  )
  expect_error(
    add_delta_column(coding_df, delta_window = 0.1, delta = 0.1, fps = 0),
    "`fps` must be a positive integer scalar."
  )

  x = add_delta_column(coding_df2 |> dplyr::select(-id))
  expect_true("id" %in% names(x))
  expect_true(
    max(x$id, na.rm = TRUE) == 1
  )
  x = add_delta_column(coding_df2 |> dplyr::select(-subject))
  expect_true("subject" %in% names(x))

  expect_true(
    identical(
      x |> dplyr::count(subject) |> dplyr::pull(subject),
      "unknown"
    )
  )

  expect_no_error(add_delta_column(
    rbind.data.frame(
      coding_df2 |> dplyr::mutate(id = 1),
      coding_df2 |> dplyr::mutate(id = 2)
    )
  ))

  expect_error(
    {
      add_delta_column(
        rbind.data.frame(
          coding_df2 |> dplyr::mutate(id = 1),
          coding_df2 |> dplyr::mutate(id = 1)
        )
      )
    },
    "Each `frame` must be unique within each `id`/`subject`/`emotion` group."
  )

  expect_true(all(c(0, 1) %in% x$delta, na.rm = TRUE))
  expect_true(sum(x$delta == 0, na.rm = TRUE) > 0)
  expect_true(sum(x$delta == 1, na.rm = TRUE) > 0)

  #check deltas match
  y = convert_to_episodes(coding_df2, T_up = 1, delta_window = 0.2)
  x = add_delta_column(coding_df2)
  expect_true(all(y$episodes$start_frame %in% x$frame[x$delta == 1]))
})
