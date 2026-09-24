TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

test_that("video_pattern filters candidates while retaining all regex matches", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  source_videos <- list.files(
    brazil_dir,
    pattern = "^ID100024.*\\.mp4$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  export_files <- list.files(
    brazil_dir,
    pattern = "^100024_(child|mum)_.*_detailed( - Copy)?\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(source_videos) != 3L,
    "Brazil fixture must contain three ID100024 videos."
  )
  skip_if(
    length(export_files) != 3L,
    "Brazil fixture must contain three matching ID100024 detailed exports."
  )

  root <- tempfile("synchrony-video-pattern-")
  output_dir <- tempfile("synchrony-video-pattern-output-")
  dir.create(root)
  on.exit(
    unlink(
      c(root, output_dir),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  file.copy(source_videos, root)
  file.copy(export_files, root)

  expect_warning(
    result <- synchrony_moments_pipeline(
      root,
      video_pattern = "^ID100024",
      subject_from_filename = TRUE,
      output_dir = output_dir,
      emotion = "not_an_emotion"
    ),
    "sufficiently matched outputs"
  )

  expect_equal(nrow(result$videos), 3L)
  expect_equal(
    sum(result$videos$status == "completed"),
    1L
  )
  expect_equal(length(result$results), 1L)
  expect_true(all(grepl("^ID100024", result$videos$video_filename)))
})
