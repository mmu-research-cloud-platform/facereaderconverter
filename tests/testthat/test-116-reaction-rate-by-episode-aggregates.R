TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction_rate_by_episode aggregates back to reaction_rate", {
  episode_result <- reaction_rate_by_episode(test_deltas)

  summary_from_episode <- episode_result[,
    .(
      n_episodes = .N,
      n_reactions = sum(reaction)
    ),
    by = .(id, subject, emotion)
  ]
  summary_from_episode[, `:=`(
    n_episodes = as.integer(n_episodes),
    n_reactions = as.integer(n_reactions),
    reaction_rate = as.numeric(n_reactions / n_episodes)
  )]
  data.table::setorder(summary_from_episode, id, subject, emotion)

  expect_equal(
    reaction_rate(test_deltas),
    summary_from_episode[, .(
      id,
      subject,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )],
    ignore_attr = TRUE
  )
})
