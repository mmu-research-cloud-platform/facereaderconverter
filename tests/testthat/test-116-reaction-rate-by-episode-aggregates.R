TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)
test_data_delta <- test_coding |>
  convert_to_episodes() |>
  add_delta_column(delta_window = 0.2, delta = 0.1, fps = 30)
test_that("reaction_rate_by_episode aggregates back to reaction_rate", {
  episode_result <- reaction_rate_by_episode(
    test_data_delta,
  )

  summary_from_episode <- episode_result[,
    .(
      n_episodes = .N,
      n_reactions = sum(reaction)
    ),
    by = .(id, denominator, numerator, emotion)
  ]
  summary_from_episode[,
    reaction_rate := ifelse(
      n_episodes > 0L,
      n_reactions / n_episodes,
      NA_real_
    )
  ]
  summary_from_episode[, `:=`(
    n_episodes = as.integer(n_episodes),
    n_reactions = as.integer(n_reactions),
    reaction_rate = as.numeric(reaction_rate)
  )]
  data.table::setorder(
    summary_from_episode,
    id,
    denominator,
    numerator,
    emotion
  )

  expect_equal(
    reaction_rate(test_data_delta)[, .(
      id,
      denominator,
      numerator,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )],
    summary_from_episode[, .(
      id,
      denominator,
      numerator,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )],
    ignore_attr = TRUE
  )
})
