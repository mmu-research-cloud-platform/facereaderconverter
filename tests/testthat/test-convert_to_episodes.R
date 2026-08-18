test_that("convert_to_episodes", {
  coding_df = read.csv("junk/testdata_detailed.csv") |>
    dplyr::mutate(id = 1, subject = "teen")

  coding_df2 = coding_df |>
    tidyr::pivot_longer(
      cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
      names_to = "emotion",
      values_to = "value"
    )

  # long data
  expect_no_error({
    convert_to_episodes(coding_df2)
  })
  expect_no_error({
    convert_to_episodes(coding_df2, consecutive_missing = 0)
  })
  expect_no_error({
    convert_to_episodes(coding_df2, consecutive_missing = 1e4)
  })
  expect_error(
    {
      convert_to_episodes(coding_df2, consecutive_missing = Inf)
    },
    "`consecutive_missing` cannot be infinite."
  )

  duplicate_long_df = dplyr::bind_rows(
    coding_df2,
    coding_df2 |> dplyr::slice(1)
  )
  expect_error(
    convert_to_episodes(duplicate_long_df),
    "Duplicate `video_time` values found within `id`/`subject`/`emotion` groups."
  )

  # wide data
  expect_no_error({
    convert_to_episodes(coding_df)
  })

  x = convert_to_episodes(coding_df)

  expect_true(all(names(x) %in% c("episodes", "coding")))

  # checking that all there is 1 episode per status row
  expect_true(sum(x$coding$status, na.rm = TRUE) == nrow(x$episodes))

  expect_true({
    sum(x$coding$status == 1, na.rm = TRUE) ==
      sum(x$coding$status == 0, na.rm = TRUE)
  })

  expect_true(max(x$coding$run_id, na.rm = TRUE) == nrow(x$episodes))
  expect_true(
    max(x$coding$run_id, na.rm = TRUE) == max(x$episodes$run_id, na.rm = TRUE)
  )
  expect_error(
    convert_to_episodes(coding_df2, fps = 0),
    "`fps` must be a positive integer scalar."
  )

  expect_error(
    convert_to_episodes(coding_df2, delta_window = -1),
    "`delta_window` must be a numeric scalar >= 0."
  )

  expect_error(
    convert_to_episodes(coding_df2, T_up = 1.5),
    "`T_up` must be a numeric scalar in \\[0, 1\\]."
  )

  expect_error(
    convert_to_episodes(coding_df2, T_down = -0.1),
    "`T_down` must be a numeric scalar in \\[0, 1\\]."
  )

  expect_error(
    convert_to_episodes(coding_df2, T_up = 0.1, T_down = 0.2),
    "`T_up` must be >= `T_down`."
  )

  expect_error(
    convert_to_episodes(coding_df2, delta = 0),
    "`delta` must be a numeric scalar > 0."
  )

  expect_error(
    convert_to_episodes(coding_df2, min_dur_sec = 0),
    "`min_dur_sec` must be a numeric scalar > 0."
  )

  expect_error(
    convert_to_episodes(coding_df2, consecutive_missing = -1),
    "`consecutive_missing` must be a non-negative integer scalar."
  )

  expect_error(
    convert_to_episodes(coding_df2, consecutive_missing = 1.5),
    "`consecutive_missing` must be a non-negative integer scalar."
  )
  x = convert_to_episodes(coding_df2 |> dplyr::select(-id))
  expect_true("id" %in% names(x$coding))
  expect_true(
    max(x$coding$id, na.rm = TRUE) == 1
  )
  x = convert_to_episodes(coding_df2 |> dplyr::select(-subject))
  expect_true("subject" %in% names(x$coding))
  expect_true(
    identical(
      x$coding |> dplyr::count(subject) |> dplyr::pull(subject),
      factor("unknown", levels = levels(x$coding$subject))
    )
  )

  expect_no_error(convert_to_episodes(
    rbind.data.frame(coding_df2 |> dplyr::mutate(id = 1), coding_df2 |> dplyr::mutate(id = 2))
  ))

  expect_error(convert_to_episodes(
    rbind.data.frame(
      coding_df2 |> dplyr::mutate(id = 1),
      coding_df2 |> dplyr::mutate(id = 1)
    ),
    "Duplicate `video_time` values found within `id`/`subject`/`emotion` groups."
  ))
})
