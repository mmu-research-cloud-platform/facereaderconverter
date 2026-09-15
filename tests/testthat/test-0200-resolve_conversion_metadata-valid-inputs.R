library(testthat)

test_that("resolve_conversion_metadata returns optional scalar metadata", {
  path <- file.path("input", "sample.txt")

  expect_identical(
    facereaderconverter:::resolve_conversion_metadata(
      id = 42,
      subject = "parent",
      inpath = path
    ),
    list(id = 42, subject = "parent")
  )
  expect_identical(
    facereaderconverter:::resolve_conversion_metadata(
      id = NULL,
      subject = NULL,
      inpath = path
    ),
    list(id = NULL, subject = NULL)
  )
})

test_that("resolve_conversion_metadata evaluates path callbacks", {
  path <- file.path("input", "sample.txt")

  expect_identical(
    facereaderconverter:::resolve_conversion_metadata(
      id = function(inpath) tools::file_path_sans_ext(basename(inpath)),
      subject = function(inpath) dirname(inpath),
      inpath = path
    ),
    list(id = "sample", subject = "input")
  )
})
