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

test_that("reaction_rate returns the expected columns", {
  result <- reaction_rate(test_deltas)

  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "id",
      "subject",
      "emotion",
      "n_episodes",
      "n_reactions",
      "reaction_rate"
    ) %in%
      names(result)
  ))
  expect_true(all(result$n_episodes >= 0L))
  expect_true(all(result$n_reactions >= 0L))
  expect_true(all(result$reaction_rate >= 0 | is.na(result$reaction_rate)))
})
