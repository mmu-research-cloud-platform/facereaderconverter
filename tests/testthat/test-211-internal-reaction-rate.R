TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

test_that("reaction-rate internals classify scalars and whole numbers", {
  expect_true(facereaderconverter:::is_reaction_rate_scalar(1))
  expect_false(facereaderconverter:::is_reaction_rate_scalar(c(1, 2)))
  expect_false(facereaderconverter:::is_reaction_rate_scalar(NA_real_))

  expect_true(facereaderconverter:::is_reaction_rate_whole(30L))
  expect_false(facereaderconverter:::is_reaction_rate_whole(30.5))
  expect_false(facereaderconverter:::is_reaction_rate_whole(NA_real_))
})
