library(testthat)

test_that("resolve_conversion_metadata rejects invalid resolved values", {
  path <- file.path("input", "sample.txt")

  expect_error(
    facereaderconverter:::resolve_conversion_metadata(
      id = c("one", "two"),
      subject = NULL,
      inpath = path
    ),
    "`id` must resolve to one non-missing string or numeric value."
  )
  expect_error(
    facereaderconverter:::resolve_conversion_metadata(
      id = NA_character_,
      subject = NULL,
      inpath = path
    ),
    "`id` must resolve to one non-missing string or numeric value."
  )
  expect_error(
    facereaderconverter:::resolve_conversion_metadata(
      id = TRUE,
      subject = NULL,
      inpath = path
    ),
    "`id` must be a string, numeric value, or function returning one."
  )
  expect_error(
    facereaderconverter:::resolve_conversion_metadata(
      id = NULL,
      subject = function(inpath) c("parent", "child"),
      inpath = path
    ),
    "`subject` must resolve to one non-missing string or numeric value."
  )
})
