TEST_DATA <- Sys.getenv("TEST_DATA")

test_that("convertFRDirectory reports successful conversion count", {
  input_dir <- file.path(TEST_DATA, "FR9")
  skip_if(!dir.exists(input_dir), "The FR9 directory fixture is not available")

  output_dir <- tempfile("frdir_output_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expected <- length(list.files(
    input_dir,
    pattern = "\\.(txt|xlsx|csv)$",
    ignore.case = TRUE,
    full.names = TRUE
  ))

  test_started <- Sys.time()
  expect_message(
    result <- convertFRDirectory(
      input_dir,
      output_dir,
      cores = 1L
    ),
    paste0("Successfully converted ", expected, " files\\.")
  )
  expect_equal(sum(result$status == "Success"), expected)
  expect_files_modified_since(
    result$outpath[result$status == "Success"],
    test_started
  )
  expect_files_modified_since(
    file.path(output_dir, "metadata.csv"),
    test_started
  )
})
