library(testthat)

load_synchrony_cli <- function() {
  script_path <- system.file(
    "scripts",
    "synchrony-moments",
    package = "facereaderconverter"
  )
  if (!nzchar(script_path)) {
    script_path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      "synchrony-moments"
    )
  }
  script <- readLines(script_path, warn = FALSE)
  entry_point <- which(script == "if (sys.nframe() == 0L) {")
  script <- script[seq_len(entry_point[[1L]] - 1L)]
  environment <- new.env(parent = globalenv())
  eval(parse(text = script), envir = environment)
  environment
}

test_that("synchrony-moments manifest reports zero exported videos", {
  input_dir <- tempfile("synchrony-zero-export-input-")
  dir.create(input_dir)
  file.create(file.path(input_dir, "zero-export.mp4"))
  for (participant in c("parent", "teen")) {
    writeLines(
      c(
        "Video analysis detailed log",
        "",
        "Face Model\tGeneral",
        "Filename\tzero-export.mp4",
        "Frame rate\t30",
        "",
        "Video Time\tNeutral\tHappy\tParticipant Name",
        paste("00:00:00.000\t0\t0.01", participant, sep = "\t"),
        paste("00:00:00.033\t0\t0.01", participant, sep = "\t")
      ),
      file.path(input_dir, paste0(participant, "_detailed.txt"))
    )
  }
  on.exit(unlink(input_dir, recursive = TRUE, force = TRUE), add = TRUE)
  output_dir <- tempfile("synchrony-zero-export-output-")
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
