TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

make_with_short_delta <- function(x) {
  out <- copy(x)
  out[, delta := 0L]
  out[1:2, delta := 1L]
  out
}

test_that("reaction_rate supports frame-based limits", {
  seconds_limit <- reaction_rate(
    test_deltas,
    episode_limit = 3,
    exclude_start = 0.1
  )
  frames_limit <- reaction_rate(
    test_deltas,
    episode_limit_frames = 90,
    exclude_start_frames = 3
  )

  expect_equal(
    seconds_limit[, .(
      id,
      subject,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )],
    frames_limit[, .(
      id,
      subject,
      emotion,
      n_episodes,
      n_reactions,
      reaction_rate
    )]
  )
})
