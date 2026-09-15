TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("synchrony aggregation respects constraint_method", {
  coded_data <- test_coding |>
    convert_to_episodes(
      T_up = 0.15,
      T_down = 0.05,
      delta = 1,
      fps = 30,
      delta_window = 0.2
    )

  methods <- c("strict", "episode", "frames", "loose")
  aggregated <- lapply(methods, function(method) {
    synchrony(
      coded_data,
      time_limit = 3,
      constraint_method = method,
      exclude_emotions = NULL
    )
  })
  by_episode <- lapply(methods, function(method) {
    synchrony_by_episode(
      coded_data,
      time_limit = 3,
      constraint_method = method,
      exclude_emotions = NULL
    )
  })

  for (i in seq_along(methods)) {
    expected <- by_episode[[i]][,
      .(
        n_episodes = .N,
        synchrony = mean(synchrony)
      ),
      by = .(id, denominator, numerator, emotion)
    ]
    data.table::setorder(expected, id, denominator, numerator, emotion)

    expect_equal(
      aggregated[[i]],
      expected,
      info = methods[[i]]
    )
  }

  expect_false(identical(aggregated[[1L]], aggregated[[2L]]))
})
