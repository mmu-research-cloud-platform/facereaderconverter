TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

load_synchrony_cli <- function() {
  script_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "synchrony-moments"
  )
  script <- readLines(script_path, warn = FALSE)
  entry_point <- which(script == "if (sys.nframe() == 0L) {")
  script <- script[seq_len(entry_point[[1L]] - 1L)]
  environment <- new.env(parent = globalenv())
  eval(parse(text = script), envir = environment)
  environment
}

test_that("synchrony-moments manifest reports zero exported videos", {
  input_dir <- file.path(TEST_DATA, "8895")
  skip_if_not(dir.exists(input_dir))
  output_dir <- tempfile("synchrony-8895-output-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  cli <- load_synchrony_cli()

  result <- cli$main(c(
    "--input",
    input_dir,
    "--output-dir",
    output_dir,
    "--output",
    "folder"
  ))
  manifest <- data.table::fread(file.path(
    output_dir,
    "synchrony-moments-manifest.csv"
  ))

  expect_equal(nrow(result$videos), 1L)
  expect_equal(result$videos$status, "completed")
  expect_equal(
    sum(vapply(
      result$results,
      function(video) nrow(video$clip_manifest),
      integer(1)
    )),
    0L
  )
  expect_true("n_videos_exported" %in% names(manifest))
  expect_true(all(manifest$n_videos_exported == 0L))
})
