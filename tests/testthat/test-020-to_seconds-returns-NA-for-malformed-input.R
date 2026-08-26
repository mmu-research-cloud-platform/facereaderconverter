library(testthat)


test_that("to_seconds returns NA for malformed input", {
  expect_equal(to_seconds("00:00:10."), NA_real_)
})
