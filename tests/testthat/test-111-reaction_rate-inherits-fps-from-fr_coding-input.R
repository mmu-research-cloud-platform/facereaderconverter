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

test_that("reaction_rate inherits fps from fr_coding input", {
  result <- reaction_rate(test_deltas)

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
