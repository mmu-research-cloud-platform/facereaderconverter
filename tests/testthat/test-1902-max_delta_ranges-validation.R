library(testthat)

test_that("native max_delta_ranges validates range vectors", {
  expect_error(
    facereaderconverter:::max_delta_ranges(
      c(0.1, 0.2),
      start_row = 1L,
      end_row = c(1L, 2L),
      group_start_row = 1L,
      k = 0L
    ),
    "Range position vectors must have the same length."
  )
})
