TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

load_cli_parser <- function() {
  script <- readLines(
    testthat::test_path("..", "..", "inst", "scripts", "synchrony-moments"),
    warn = FALSE
  )
  entry_point <- which(script == "if (sys.nframe() == 0L) {")
  script <- script[seq_len(entry_point[[1L]] - 1L)]
  environment <- new.env(parent = globalenv())
  eval(parse(text = script), envir = environment)
  environment
}

test_that("synchrony-moments CLI maps every pipeline argument", {
  cli <- load_cli_parser()
  values <- cli$parse_args(c(
    "--input",
    "data/study",
    "--output-dir",
    "clips",
    "--video-pattern",
    "\\.(mp4|mov)$",
    "--video",
    "recording.mp4",
    "--subject-from-filename",
    "--verbose",
    "--t-up",
    "0.25",
    "--t-down",
    "0.15",
    "--delta",
    "0.12",
    "--delta-window",
    "0.3",
    "--min-dur-sec",
    "0.2",
    "--consecutive-missing",
    "10",
    "--cores",
    "1",
    "--time-limit",
    "2",
    "--time-limit-frames",
    "50",
    "--constraint-method",
    "frames",
    "--missing-threshold",
    "0.1",
    "--exclude-emotions",
    "neutral,sad",
    "--n",
    "2",
    "--emotion",
    "happy,surprised",
    "--optimised-subject",
    "parent",
    "--no-only-synchronies",
    "--buffer",
    "2,1",
    "--buffer-units",
    "frames",
    "--output",
    "folder",
    "--ffmpeg",
    "ffmpeg",
    "--overwrite"
  ))
  values$inpath <- values$input
  values$input <- NULL

  expect_setequal(
    names(values),
    names(formals(facereaderconverter::synchrony_moments_pipeline))
  )
  expect_equal(values$buffer, c(before = 2, after = 1))
  expect_equal(values$emotion, c("happy", "surprised"))
  expect_equal(values$exclude_emotions, c("neutral", "sad"))
  expect_true(values$verbose)
  expect_false(values$only_synchronies)
})

test_that("synchrony-moments CLI loads subject maps from CSV", {
  cli <- load_cli_parser()
  map_path <- tempfile(fileext = ".csv")
  on.exit(unlink(map_path), add = TRUE)
  writeLines(
    c("export_filename,subject", "first.txt,parent"),
    map_path
  )

  values <- cli$parse_args(c(
    "--input",
    "data/study",
    "--subject-map",
    map_path
  ))

  expect_equal(values$subject_map, c("first.txt" = "parent"))
})

test_that("synchrony-moments CLI uses input as the default output directory", {
  cli <- load_cli_parser()
  values <- cli$parse_args(c("--input", "data/study"))

  expect_null(values$output_dir)
})

test_that("synchrony-moments CLI processes two matched Brazil XLSX video pairs", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  skip_if(Sys.which("ffmpeg") == "", "FFmpeg is not available.")
  skip_if(Sys.which("ffprobe") == "", "FFprobe is not available.")
  output_dir <- tempfile("brazil-cli-output-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  cli <- load_cli_parser()
  test_started <- Sys.time()

  result <- NULL
  expect_message(
    result <- cli$main(c(
      "--input",
      brazil_dir,
      "--output-dir",
      output_dir,
      "--subject-from-filename",
      "--n",
      "10",
      "--emotion",
      "happy",
      "--output",
      "folder",
      "--overwrite"
    )),
    "ID100024_side_by_side - Copy.mp4"
  )

  expect_equal(nrow(result$videos), 2L)
  expect_length(result$results, 2L)
  expect_true(file.exists(file.path(
    output_dir,
    "synchrony-moments-manifest.csv"
  )))
  expect_true(all(vapply(
    result$results,
    function(video) file.exists(file.path(video$clip_output, "manifest.csv")),
    logical(1)
  )))
  expect_files_modified_since(
    c(
      file.path(output_dir, "synchrony-moments-manifest.csv"),
      unlist(
        lapply(result$results, function(video) {
          c(
            file.path(video$clip_output, "manifest.csv"),
            file.path(video$clip_output, video$clip_manifest$clip_filename)
          )
        }),
        use.names = FALSE
      )
    ),
    test_started
  )
  expect_true(all(vapply(
    result$results,
    function(video) {
      length(list.files(
        video$clip_output,
        pattern = "\\.(mp4|mov|avi|mkv|webm)$",
        full.names = TRUE,
        ignore.case = TRUE
      )) >
        0L
    },
    logical(1)
  )))
})

test_that("synchrony-moments CLI exports no Brazil episodes when t-up is 1", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  output_dir <- tempfile("brazil-no-episodes-")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  cli <- load_cli_parser()
  test_started <- Sys.time()

  result <- NULL
  expect_message(
    result <- cli$main(c(
      "--input",
      brazil_dir,
      "--output-dir",
      output_dir,
      "--subject-from-filename",
      "--t-up",
      "1",
      "--n",
      "10",
      "--emotion",
      "happy",
      "--output",
      "folder",
      "--overwrite"
    )),
    "Completed synchrony moments pipeline"
  )

  expect_equal(nrow(result$videos), 2L)
  expect_true(file.exists(file.path(
    output_dir,
    "synchrony-moments-manifest.csv"
  )))
  expect_files_modified_since(
    file.path(output_dir, "synchrony-moments-manifest.csv"),
    test_started
  )
  expect_length(
    list.files(
      output_dir,
      pattern = "\\.(mp4|mov|avi|mkv|webm)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ),
    0L
  )
})

test_that("synchrony-moments CLI help returns without exiting R", {
  cli <- load_cli_parser()

  expect_message(expect_null(cli$main("--help")), "Usage: synchrony-moments")
})

test_that("synchrony-moments CLI main returns errors without exiting R", {
  cli <- load_cli_parser()

  expect_error(
    cli$main(c("--input", "missing-directory")),
    "one existing directory"
  )
})

test_that("synchrony-moments CLI rejects the removed FPS option", {
  cli <- load_cli_parser()

  expect_error(
    cli$parse_args(c("--input", "data/study", "--fps", "29.5")),
    "Unknown option: --fps"
  )
})
