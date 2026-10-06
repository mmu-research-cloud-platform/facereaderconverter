test_that("loadFRfile does not infer metadata for CSVs that already carry it", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  readr::write_csv(
    data.frame(
      video_time = c("00:00:00.000", "00:00:00.033"),
      neutral = c(0.1, 0.2),
      id = "8895",
      subject = "mum"
    ),
    path
  )

  data <- loadFRfile(path)
  expect_identical(unique(as.character(data$id)), "8895")
  expect_identical(unique(data$subject), "mum")

  plain <- tempfile(fileext = ".csv")
  on.exit(unlink(plain), add = TRUE)
  readr::write_csv(data.frame(video_time = "00:00:00.000", neutral = 0.5), plain)
  expect_false(any(c("id", "subject") %in% names(loadFRfile(plain))))
})

test_that("default inferred metadata does not rename converted CSVs", {
  input_dir <- tempfile("fr_default_names_")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE, force = TRUE), add = TRUE)
  inpath <- file.path(input_dir, "session_detailed.txt")
  file.copy(testthat::test_path("testdata", "testdata_detailed.txt"), inpath)

  result <- convertFRFiles(inpath)
  expect_identical(
    basename(result$outpath),
    "session_detailed.csv"
  )
  converted <- readr::read_csv(result$outpath, show_col_types = FALSE)
  expect_identical(unique(converted$subject), "session_detailed")
})

test_that("in-place directory reruns skip renamed derived CSVs", {
  input_dir <- tempfile("fr_rerun_")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(
    testthat::test_path("testdata", "testdata_detailed.txt"),
    file.path(input_dir, "session_detailed.txt")
  )

  run <- function() {
    convertFRDirectory(
      input_dir,
      input_dir,
      id = "8895",
      subject = "mum",
      cores = 1L,
      save_metadata = NULL
    )
  }
  first <- run()
  expect_identical(
    basename(first$outpath),
    "8895_mum_session_detailed_detailed.csv"
  )
  second <- run()
  expect_true(all(second$status == "Success"))
  expect_identical(basename(second$inpath), "session_detailed.txt")
})

test_that("directory episodes reject duplicate NA-ID subject pairs", {
  root <- tempfile("na-id-duplicates-")
  dir.create(file.path(root, "a"), recursive = TRUE)
  dir.create(file.path(root, "b"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  fixture <- file.path(
    Sys.getenv("TEST_DATA"),
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
  )
  skip_if(!file.exists(fixture), "c2e-directory fixture is not available")
  file.copy(fixture, file.path(root, "a", "mum.txt"))
  file.copy(fixture, file.path(root, "b", "mum.txt"))

  expect_error(
    convert_directory_to_episodes(
      root,
      id_pattern = "no-such-id",
      cores = 1L
    ),
    "Multiple exports resolve to the same ID and subject"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})
