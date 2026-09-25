library(testthat)


test_that("to_seconds pads and truncates fractional digits", {
  expect_equal(to_seconds("00:00:10"), 10)
  expect_equal(to_seconds("00:00:10.5"), 10.500)
  expect_equal(to_seconds("00:00:10.0005"), 10.000)
})

test_that("to_seconds adjusts fractional digits explicitly", {
  expect_equal(to_seconds("00:00:10.56", digits = 1L), 10.5)
  expect_equal(to_seconds("00:00:10.5", digits = 2L), 10.50)
  expect_equal(to_seconds("00:00:10.5", digits = 0L), 10)
})
