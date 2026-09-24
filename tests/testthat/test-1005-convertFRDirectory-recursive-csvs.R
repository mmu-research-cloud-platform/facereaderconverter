test_that("convertFRDirectory keeps CSVs distinct from sibling source stems", {
  input_dir <- tempfile("fr_recursive_input_")
  output_dir <- tempfile("fr_recursive_output_")
  dir.create(file.path(input_dir, "first"), recursive = TRUE)
  dir.create(file.path(input_dir, "second"), recursive = TRUE)
  on.exit(
    unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  file.copy(
    testthat::test_path("testdata", "testdata_detailed.txt"),
    file.path(input_dir, "first", "session.txt")
  )
  readr::write_csv(
    data.frame(video_time = "00:00:00.000", neutral = 0.5),
    file.path(input_dir, "second", "session.csv")
  )

  test_started <- Sys.time()
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    recursive = TRUE,
    cores = 1L,
    save_metadata = NULL
  )

  expect_true(all(result$status == "Success"))
  relative_paths <- sub(
    paste0("^", normalizePath(input_dir, winslash = "/"), "/"),
    "",
    normalizePath(result$inpath, winslash = "/")
  )
  expect_setequal(relative_paths, c("first/session.txt", "second/session.csv"))
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
})
