library(testthat)


test_that("to_seconds converts hh:mm:ss and hh:mm:ss.mmm to seconds", {
  expect_equal(
    to_seconds(c("00:00:00", "00:00:00.000", "00:01:02.500", "01:02:03.250")),
    c(0, 0, 62.5, 3723.25)
  )
})
