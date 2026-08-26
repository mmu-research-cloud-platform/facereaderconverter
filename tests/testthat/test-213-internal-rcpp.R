library(testthat)

test_that("internal Rcpp helpers behave as expected", {
  expect_identical(
    facereaderconverter:::all_deltas(c(1, 3, 2, 5), 1L, 1),
    c(NA_integer_, 1L, 0L, 1L)
  )

  expect_identical(
    facereaderconverter:::hysteresis_state(
      c(0, 0.3, 0.25, 0.05),
      1L,
      0.2,
      0.1,
      0.5,
      1L,
      0L
    ),
    c(FALSE, TRUE, TRUE, FALSE)
  )
})
