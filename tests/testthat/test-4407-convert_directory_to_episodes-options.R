library(testthat)

copy_4407_fixture <- function(destination) {
  file.copy(
    file.path(
      Sys.getenv("TEST_DATA"),
      "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
    ),
    destination
  )
}

test_that("4407 filter_name matches export basenames before parsing", {
  root <- tempfile("directory-selection-")
  dir.create(file.path(root, "keep-parent"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "keep-parent", "keep.txt"))
  writeLines(
    "not a FaceReader export",
    file.path(root, "keep-parent", "other.txt")
  )

  result <- convert_directory_to_episodes(
    root,
    filter_name = "^keep\\.txt$",
    cores = 1L
  )
  expect_equal(unique(as.character(result$coding$subject)), "keep")
  expect_identical(result$metadata$fps, 30L)
  expect_true(file.exists(file.path(root, "episodes.RDa")))
  expect_error(
    convert_directory_to_episodes(
      root,
      filter_name = "^keep-parent$",
      outpath = file.path(root, "none.RDa")
    ),
    "No detailed FaceReader exports were found"
  )
})

test_that("4408 skip_fails warns for invalid exports and keeps good data", {
  root <- tempfile("directory-skip-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("Video analysis detailed log", "Filename\tbroken.mp4"),
    file.path(root, "a-bad.txt")
  )
  copy_4407_fixture(file.path(root, "b-good.txt"))
  writeLines(
    c("Video analysis detailed log", "Filename\tbroken.mp4", "Frame rate\t30"),
    file.path(root, "c-broken.txt")
  )
  writeLines(
    c("Video analysis detailed log", "Filename\tother.mp4", "Frame rate\t60"),
    file.path(root, "d-conflict.txt")
  )
  warnings <- character()
  result <- withCallingHandlers(
    convert_directory_to_episodes(root, skip_fails = TRUE, cores = 1L),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(unique(as.character(result$coding$subject)), "b-good")
  expect_length(warnings, 3L)
  expect_match(warnings[[1L]], "a-bad\\.txt")
  expect_match(warnings[[2L]], "c-broken\\.txt")
  expect_match(warnings[[3L]], "d-conflict\\.txt")
  expect_true(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4409 skip_fails FALSE stops at a failed export", {
  root <- tempfile("directory-fail-fast-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("Video analysis detailed log", "Filename\tbroken.mp4"),
    file.path(root, "a-bad.txt")
  )
  copy_4407_fixture(file.path(root, "b-good.txt"))

  expect_error(
    convert_directory_to_episodes(root, skip_fails = FALSE, cores = 1L),
    "Invalid or missing frame rate"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4410 skip_fails still errors when no detailed exports survive", {
  root <- tempfile("directory-all-fail-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("Video analysis detailed log", "Filename\tbroken.mp4", "Frame rate\t30"),
    file.path(root, "broken.txt")
  )

  expect_warning(
    expect_error(
      convert_directory_to_episodes(root, skip_fails = TRUE, cores = 1L),
      "No detailed FaceReader exports could be processed"
    ),
    "Skipping FaceReader export"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4411 new directory options reject invalid values", {
  root <- tempfile("directory-options-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(
    convert_directory_to_episodes(root, skip_fails = NA),
    "skip_fails"
  )
  expect_error(
    convert_directory_to_episodes(root, filter_name = ""),
    "filter_name"
  )
})

test_that("4412 skip_fails skips a subject pattern mismatch", {
  root <- tempfile("directory-subject-skip-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "a-bad.txt"))
  copy_4407_fixture(file.path(root, "b-good.txt"))

  expect_warning(
    result <- convert_directory_to_episodes(
      root,
      subject_pattern = "good",
      skip_fails = TRUE,
      cores = 1L
    ),
    "Subject pattern did not match:.*a-bad\\.txt"
  )
  expect_equal(unique(as.character(result$coding$subject)), "good")
})

test_that("4413 README directory episode example runs on fixture", {
  root <- file.path(Sys.getenv("TEST_DATA"), "c2e-directory")
  output <- tempfile(fileext = ".RDa")
  on.exit(unlink(output), add = TRUE)

  coded_data <- convert_directory_to_episodes(
    inpath = root,
    outpath = output,
    filter_name = "^Participant 10_8892_.*_detailed\\.txt$",
    skip_fails = TRUE,
    cores = 1L
  )

  expect_s3_class(coded_data, "fr_coding")
  expect_gt(nrow(coded_data$episodes), 0L)
  expect_true(file.exists(output))
  expect_equal(
    unique(as.character(coded_data$coding$subject)),
    "Participant 10_8892_Analysis 1_video_20260721_125850_detailed"
  )
})

test_that("4414 directory episodes report discovery and processing counts", {
  root <- tempfile("directory-message-counts-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "a-good.txt"))
  writeLines(
    c("Video analysis detailed log", "Filename\tbroken.mp4"),
    file.path(root, "b-bad.txt")
  )
  writeLines("Video analysis state log", file.path(root, "c-state.txt"))
  writeLines("not selected", file.path(root, "d-ignored.txt"))

  messages <- character()
  warnings <- character()
  result <- withCallingHandlers(
    convert_directory_to_episodes(
      root,
      filter_name = "^[abc]-.*\\.txt$",
      skip_fails = TRUE,
      cores = 1L
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(result, "fr_coding")
  expect_match(
    paste(messages, collapse = "\n"),
    "Found 4 FaceReader TXT/XLSX file"
  )
  expect_match(
    paste(messages, collapse = "\n"),
    "Selected 3 file.*filtered out 1 file"
  )
  expect_match(
    paste(messages, collapse = "\n"),
    "Processed 1 detailed file.*ignored 1 non-detailed file.*skipped 1 failed file"
  )
  expect_length(warnings, 1L)
  expect_match(warnings[[1L]], "b-bad\\.txt")
})

test_that("4415 directory episodes report zero skips without a filter", {
  root <- tempfile("directory-message-success-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "good.txt"))
  messages <- character()
  withCallingHandlers(
    convert_directory_to_episodes(root, cores = 1L),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(
    paste(messages, collapse = "\n"),
    "Found 1 FaceReader TXT/XLSX file"
  )
  expect_match(
    paste(messages, collapse = "\n"),
    "Processed 1 detailed file.*ignored 0 non-detailed file.*skipped 0 failed file"
  )
})

test_that("4416 skip_fails defaults to warning and keeping good exports", {
  root <- tempfile("directory-default-skip-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("Video analysis detailed log", "Filename\tbroken.mp4"),
    file.path(root, "a-bad.txt")
  )
  copy_4407_fixture(file.path(root, "b-good.txt"))

  expect_warning(
    result <- convert_directory_to_episodes(root, cores = 1L),
    "Skipping FaceReader export.*a-bad\\.txt"
  )
  expect_equal(unique(as.character(result$coding$subject)), "b-good")
  expect_true(file.exists(file.path(root, "episodes.RDa")))
})

copy_4407_fps_fixture <- function(destination) {
  file.copy(
    file.path(
      Sys.getenv("TEST_DATA"),
      "FR9/FR9 1218 participant_Analysis 1_video_20260910_110344_detailed.xlsx"
    ),
    destination
  )
}

test_that("4417 subject mismatch errors when skip_fails is FALSE", {
  root <- tempfile("directory-subject-fail-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "a-bad.txt"))
  copy_4407_fixture(file.path(root, "b-good.txt"))

  expect_error(
    convert_directory_to_episodes(
      root,
      subject_pattern = "good",
      skip_fails = FALSE,
      cores = 1L
    ),
    "Subject pattern did not match:.*a-bad\\.txt"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4418 multiple IDs error when skip_fails is FALSE", {
  root <- tempfile("directory-multi-id-fail-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "only.txt"))

  expect_error(
    convert_directory_to_episodes(
      root,
      id_pattern = "#[0-9]{4}|card|synced",
      skip_fails = FALSE,
      cores = 1L
    ),
    "Multiple different IDs"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4419 multiple IDs are skipped with a warning when skip_fails is TRUE", {
  root <- tempfile("directory-multi-id-skip-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "only.txt"))

  expect_warning(
    expect_error(
      convert_directory_to_episodes(
        root,
        id_pattern = "#[0-9]{4}|card|synced",
        skip_fails = TRUE,
        cores = 1L
      ),
      "No detailed FaceReader exports could be processed"
    ),
    "Multiple different IDs"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4420 duplicate ID and subject is skipped when skip_fails is TRUE", {
  root <- tempfile("directory-duplicate-skip-")
  dir.create(file.path(root, "nested-a"), recursive = TRUE)
  dir.create(file.path(root, "nested-b"))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "nested-a", "same-a.txt"))
  copy_4407_fixture(file.path(root, "nested-b", "same-b.txt"))

  expect_warning(
    result <- convert_directory_to_episodes(
      root,
      id_pattern = "#[0-9]{4}",
      subject_pattern = "same",
      skip_fails = TRUE,
      cores = 1L
    ),
    "Multiple exports resolve to the same ID and subject.*same-a\\.txt"
  )
  expect_equal(unique(as.character(result$coding$subject)), "same")
  expect_equal(unique(as.character(result$coding$id)), "#8892")
  expect_true(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4421 duplicate ID and subject errors when skip_fails is FALSE", {
  root <- tempfile("directory-duplicate-fail-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fixture(file.path(root, "same-a.txt"))
  copy_4407_fixture(file.path(root, "same-b.txt"))

  expect_error(
    convert_directory_to_episodes(
      root,
      id_pattern = "#[0-9]{4}",
      subject_pattern = "same",
      skip_fails = FALSE,
      cores = 1L
    ),
    "Multiple exports resolve to the same ID and subject"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4422 conflicting FPS is skipped when skip_fails is TRUE", {
  root <- tempfile("directory-fps-skip-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fps_fixture(file.path(root, "first.xlsx"))
  copy_4407_fixture(file.path(root, "second.txt"))

  expect_warning(
    result <- convert_directory_to_episodes(
      root,
      skip_fails = TRUE,
      cores = 1L
    ),
    "Conflicting frame rates.*second\\.txt"
  )
  expect_equal(unique(as.character(result$coding$subject)), "first")
  expect_true(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4423 conflicting FPS errors when skip_fails is FALSE", {
  root <- tempfile("directory-fps-fail-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4407_fps_fixture(file.path(root, "first.xlsx"))
  copy_4407_fixture(file.path(root, "second.txt"))

  expect_error(
    convert_directory_to_episodes(root, skip_fails = FALSE, cores = 1L),
    "Conflicting frame rates"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("4424 existing output warns and skips conversion by default", {
  root <- tempfile("directory-existing-output-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  output <- file.path(root, "episodes.RDa")
  coded_data <- list(test_value = "preserve existing output")
  save(coded_data, file = output)

  expect_warning(
    result <- withVisible(convert_directory_to_episodes(
      root,
      outpath = output
    )),
    "Output already exists.*skipping conversion"
  )
  expect_false(result$visible)
  expect_identical(result$value, coded_data)
  saved <- new.env(parent = emptyenv())
  load(output, envir = saved)
  expect_identical(saved$coded_data, coded_data)
})
