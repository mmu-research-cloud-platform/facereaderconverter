TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)


test_that("delta is an alias for add_delta_column", {
  result_alias <- delta(
    test_coding,
    delta_window = 0.2,
    delta = 0.1,
    fps = 30
  )
  result_direct <- add_delta_column(
    test_coding,
    delta_window = 0.2,
    delta = 0.1,
    fps = 30
  )

  expect_identical(result_alias, result_direct)
  expect_true("delta" %in% names(result_alias))
})
