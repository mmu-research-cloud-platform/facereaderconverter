library(testthat)

test_that("native max_delta_ranges calculates clipped ordered ranges", {
  expect_equal(
    facereaderconverter:::max_delta_ranges(
      c(NA, 0.1, 0.5, 0.2, 0.8, NA),
      start_row = c(3L, 5L, 6L),
      end_row = c(4L, 5L, 6L),
      group_start_row = c(1L, 1L, 1L),
      k = 2L
    ),
    c(0.4, 0.6, 0.6)
  )
})
