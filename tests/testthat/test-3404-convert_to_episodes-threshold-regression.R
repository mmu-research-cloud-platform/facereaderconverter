test_that("convert_to_episodes returns episode max_value above T_up", {
  coding_df <- tibble::tibble(
    id = 1L,
    subject = "parent",
    emotion = "happy",
    frame = 1:6,
    value = c(0.05, 0.25, 0.35, 0.15, 0.05, 0.05)
  )

  converted <- convert_to_episodes(
    coding_df,
    T_up = 0.2,
    T_down = 0.1,
    delta = 0.1,
    delta_window = 1 / 30,
    min_dur_sec = 1 / 30,
    fps = 30L
  )

  expect_gt(nrow(converted$episodes), 0L)
  expect_true(all(converted$episodes$max_value > 0.2))
})
