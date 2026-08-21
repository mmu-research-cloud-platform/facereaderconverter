test_that("convert_to_episodes", {
  coding_df <- read.csv("testdata/testdata_detailed.csv") |>
    dplyr::mutate(id = 1, subject = "parent")

  coding_df2 <- coding_df |>
    tidyr::pivot_longer(
      cols = c(neutral, happy, sad, angry, surprised, scared, disgusted),
      names_to = "emotion",
      values_to = "value"
    )

  # long data
  expect_no_error({
    convert_to_episodes(coding_df2)
  })

  expect_equal(
    {
      class(convert_to_episodes(coding_df2))
    },
    c("fr_coding", "list")
  )

  expect_no_error({
    convert_to_episodes(coding_df2, fps = 30)
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

  duplicate_long_df <- dplyr::bind_rows(
    coding_df2,
    coding_df2 |> dplyr::slice(1)
  )
  expect_error(
    convert_to_episodes(duplicate_long_df),
    "Duplicate `video_time` values found within `id`/`subject` groups: id=1, subject=parent"
  )

  # wide data
  expect_no_error({
    convert_to_episodes(coding_df)
  })

  x <- convert_to_episodes(coding_df)

  expect_true(all(names(x) %in% c("episodes", "coding", "metadata")))

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
  expect_true(all(expected_cols %in% names(x$episodes)))

  # checking that all there is 1 episode per status row
  expect_true(sum(x$coding$status, na.rm = TRUE) == nrow(x$episodes))

  expect_true({
    sum(x$coding$status == 1, na.rm = TRUE) ==
      sum(x$coding$status == 0, na.rm = TRUE)
  })

  expect_all_true(
    sort(unique(x$coding$run_id)) == sort(unique(x$episodes$run_id))
  )
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
  x <- convert_to_episodes(coding_df2 |> dplyr::select(-id))

  expect_true("id" %in% names(x$coding))
  expect_true(
    max(x$coding$id, na.rm = TRUE) == 1
  )
  x <- convert_to_episodes(coding_df2 |> dplyr::select(-subject))
  expect_true("subject" %in% names(x$coding))
  expect_true(
    identical(
      x$coding |> dplyr::count(subject) |> dplyr::pull(subject),
      factor("unknown", levels = levels(x$coding$subject))
    )
  )

  expect_no_error(convert_to_episodes(
    rbind.data.frame(
      coding_df2 |> dplyr::mutate(id = 1),
      coding_df2 |> dplyr::mutate(id = 2)
    )
  ))

  expect_error(
    convert_to_episodes(
      rbind.data.frame(
        coding_df2 |> dplyr::mutate(id = 1),
        coding_df2 |> dplyr::mutate(id = 1)
      )
    ),
    "Duplicate `video_time` values found within `id`/`subject` groups: id=1, subject=parent"
  )

  expect_error(
    convert_to_episodes(
      rbind.data.frame(
        coding_df2 |> dplyr::mutate(id = 1),
        coding_df2 |> dplyr::mutate(id = 1),
        coding_df2 |> dplyr::mutate(id = 2),
        coding_df2 |> dplyr::mutate(id = 2)
      )
    ),
    "Duplicate `video_time` values found within `id`/`subject` groups: id=1, subject=parent; id=2, subject=parent"
  )
})

test_that("convert_to_episodes trims end_frame but not end_time after trailing NAs", {
  coding_df <- tibble::tibble(
    id = 1L,
    subject = "parent",
    emotion = "happy",
    frame = 1:6,
    video_time = c(
      "00:00:00.000",
      "00:00:00.033",
      "00:00:00.066",
      "00:00:00.100",
      "00:00:00.133",
      "00:00:00.166"
    ),
    value = c(0, 0.3, 0.4, NA, NA, 0)
  )

  x <- convert_to_episodes(
    coding_df,
    T_up = 0.2,
    T_down = 0.1,
    delta = 1,
    delta_window = 0.1,
    min_dur_sec = 1 / 30,
    consecutive_missing = 10L,
    fps = 30L
  )

  expect_equal(nrow(x$episodes), 1L)
  expect_equal(x$episodes$start_frame[[1]], 2L)
  expect_equal(x$episodes$end_frame[[1]], 3L)
  expect_equal(x$episodes$start_time[[1]], "00:00:00.033")
  expect_equal(x$episodes$end_time[[1]], "00:00:00.133")
  expect_equal(x$episodes$n_frames[[1]], 2L)
  expect_equal(x$episodes$duration_s[[1]], 2 / 30)

  expect_equal(x$coding$in_state, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(
    x$coding$status,
    c(NA_integer_, 1L, 0L, NA_integer_, NA_integer_, NA_integer_)
  )
  expect_equal(
    x$coding$run_id,
    c(NA_integer_, 1L, 1L, NA_integer_, NA_integer_, NA_integer_)
  )
})
