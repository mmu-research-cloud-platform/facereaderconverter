TEST_DATA <- Sys.getenv("TEST_DATA")

load(file.path(TEST_DATA, "test_data.RDa"))


test_that("convert_to_episodes; from csv conversion", {
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
  expect_error(
    convert_to_episodes(coding_df2 |> dplyr::select(-id)),
    "`id` column required\\."
  )
  expect_error(
    convert_to_episodes(coding_df2 |> dplyr::select(-subject)),
    "`subject` column required\\."
  )

  base_df <- coding_df2 |> dplyr::filter(id == 1)

  expect_no_error(convert_to_episodes(
    rbind.data.frame(
      base_df |> dplyr::mutate(id = 1),
      base_df |> dplyr::mutate(id = 2)
    )
  ))

  expect_error(
    convert_to_episodes(
      rbind.data.frame(
        base_df |> dplyr::mutate(id = 1),
        base_df |> dplyr::mutate(id = 1)
      )
    ),
    "Duplicate `video_time` values found within `id`/`subject` groups: id=1, subject=parent"
  )

  expect_error(
    convert_to_episodes(
      rbind.data.frame(
        base_df |> dplyr::mutate(id = 1),
        base_df |> dplyr::mutate(id = 1),
        base_df |> dplyr::mutate(id = 2),
        base_df |> dplyr::mutate(id = 2)
      )
    ),
    "Duplicate `video_time` values found within `id`/`subject` groups: id=1, subject=parent; id=2, subject=parent"
  )
})


test_that("convert_to_episodes; from test_data", {
  coding_df <- test_coding

  coding_df2 <- test_coding_wide

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

  expect_error(
    convert_to_episodes(coding_df2 |> dplyr::select(-id)),
    "`id` column required\\."
  )
  expect_error(
    convert_to_episodes(coding_df2 |> dplyr::select(-subject)),
    "`subject` column required\\."
  )

  base_df <- coding_df2 |> dplyr::filter(id == 1)

  expect_no_error(convert_to_episodes(
    rbind.data.frame(
      base_df |> dplyr::mutate(id = 1),
      base_df |> dplyr::mutate(id = 2)
    )
  ))

  expect_error(
    convert_to_episodes(
      rbind.data.frame(
        base_df |> dplyr::mutate(id = 1),
        base_df |> dplyr::mutate(id = 1)
      )
    ),
    "Duplicate `video_time` values found within `id`/`subject` groups: id=1, subject=parent"
  )

  expect_error(
    convert_to_episodes(
      rbind.data.frame(
        base_df |> dplyr::mutate(id = 1),
        base_df |> dplyr::mutate(id = 1),
        base_df |> dplyr::mutate(id = 2),
        base_df |> dplyr::mutate(id = 2)
      )
    ),
    "Duplicate `video_time` values found within `id`/`subject` groups: id=1, subject=parent; id=1, subject=teen; id=2, subject=parent; id=2, subject=teen"
  )
})

test_that("convert_to_episodes logic", {
  coding_df <- test_coding
  c2e1 <- convert_to_episodes(coding_df, T_up = 0.2)
  c2e2 <- convert_to_episodes(coding_df, T_up = 0.15)

  expect_lte(nrow(c2e1$episodes), nrow(c2e2$episodes))
  expect_lt(nrow(c2e1$episodes), nrow(c2e2$episodes))

  c2e3 <- convert_to_episodes(coding_df, T_up = 0.2, T_down = 0.1)
  c2e4 <- convert_to_episodes(coding_df, T_up = 0.2, T_down = 0.15)

  expect_lte(nrow(c2e3$episodes), nrow(c2e4$episodes))
})
