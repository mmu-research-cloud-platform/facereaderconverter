library(testthat)

test_that("map_paths remaps nested files and creates output directories", {
  base <- tempfile("map_paths_")
  input_dir <- file.path(base, "input.root")
  output_dir <- file.path(base, "output.root")
  nested_dir <- file.path(input_dir, "nested")
  dir.create(nested_dir, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE, force = TRUE), add = TRUE)

  files <- c(
    file.path(input_dir, "a.txt"),
    file.path(nested_dir, "b.txt")
  )
  file.create(files)

  result <- map_paths(input_dir, paste0(output_dir, "/"), files)

  expect_identical(
    result,
    c(
      file.path(output_dir, "a.txt"),
      file.path(output_dir, "nested", "b.txt")
    )
  )
  expect_true(dir.exists(output_dir))
  expect_true(dir.exists(file.path(output_dir, "nested")))
})
