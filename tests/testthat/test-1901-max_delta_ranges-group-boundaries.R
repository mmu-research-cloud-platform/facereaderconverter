library(testthat)

test_that("native max_delta_ranges restarts at group boundaries", {
  expect_equal(
    facereaderconverter:::max_delta_ranges(
      c(0.1, 0.5, 0.2, 0.8),
      start_row = c(2L, 4L),
      end_row = c(2L, 4L),
      group_start_row = c(1L, 3L),
      k = 1L
    ),
    c(0.4, 0.6)
  )
})
