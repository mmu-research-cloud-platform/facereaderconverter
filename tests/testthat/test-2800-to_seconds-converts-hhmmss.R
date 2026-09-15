library(testthat)


test_that("to_seconds converts hh:mm:ss and hh:mm:ss.mmm to seconds", {
  expect_equal(to_seconds("00:00:00", digits = 0L), 0)
  expect_equal(
    to_seconds(c("00:00:00.000", "00:01:02.500", "01:02:03.250")),
    c(0, 62.5, 3723.25)
  )
})

test_that("to_seconds returns NA for malformed input", {
  expect_equal(to_seconds("00:00:10."), NA_real_)
})
