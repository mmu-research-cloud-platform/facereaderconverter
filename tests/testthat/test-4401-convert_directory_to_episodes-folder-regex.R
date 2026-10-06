library(testthat)

copy_folder_regex_fixture <- function(relative_path, destination) {
  file.copy(file.path(Sys.getenv("TEST_DATA"), relative_path), destination)
}

test_that("directory episodes infer IDs and subjects from their own sources", {
  parent <- tempfile("folder-regex-")
  root <- file.path(parent, "study")
  dir.create(file.path(root, "8892", "mum"), recursive = TRUE)
  dir.create(file.path(root, "8895", "teen"), recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  copy_folder_regex_fixture(
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt",
    file.path(root, "8892", "mum", "mum.txt")
  )
  copy_folder_regex_fixture(
    "c2e-directory/mum/8894/Participant 11_8894_Analysis 1_video_20260721_135456_detailed.txt",
    file.path(root, "8895", "teen", "teen.txt")
  )

  result <- convert_directory_to_episodes(root, cores = 1L)
  expect_setequal(
    as.character(unique(result$coding$id)),
    c(
      "#8892 Sussed card game teen view point synced 10 minutes_1",
      "#8894 Sussed card game teen view point synced 10 minutes"
    )
  )
  expect_setequal(as.character(unique(result$coding$subject)), c("mum", "teen"))
  expect_gt(nrow(result$episodes), 0L)
  saved <- new.env(parent = emptyenv())
  expect_identical(
    load(file.path(root, "episodes.RDa"), envir = saved),
    "coded_data"
  )
  expect_equal(saved$coded_data, result)
})

test_that("directory names do not override media and FaceReader filenames", {
  parent <- tempfile("folder-precedence-")
  root <- file.path(parent, "study-9999-teen")
  dir.create(file.path(root, "8888", "mum"), recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  copy_folder_regex_fixture(
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt",
    file.path(root, "8888", "mum", "Participant_teen_detailed.txt")
  )

  result <- convert_directory_to_episodes(
    root,
    subject_pattern = "Participant_teen_detailed",
    cores = 1L
  )
  expect_equal(
    unique(as.character(result$coding$id)),
    "#8892 Sussed card game teen view point synced 10 minutes_1"
  )
  expect_equal(
    unique(as.character(result$coding$subject)),
    "Participant_teen_detailed"
  )
})

test_that("directory regexes do not use parent folders as metadata sources", {
  parent <- tempfile("outer-8822-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  copy_folder_regex_fixture(
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt",
    file.path(root, "analysis.txt")
  )

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "8822",
    cores = 1L
  )
  expect_true(all(is.na(as.character(result$coding$id))))
  expect_equal(unique(as.character(result$coding$subject)), "analysis")
})

test_that("directory episodes default IDs and subjects to fixture filenames", {
  root <- file.path(Sys.getenv("TEST_DATA"), "c2e-directory")
  detailed_files <- list.files(
    root,
    recursive = TRUE,
    pattern = "_detailed\\.txt$",
    full.names = TRUE
  )
  expected_subjects <- tools::file_path_sans_ext(basename(detailed_files))
  expected_ids <- vapply(
    detailed_files,
    function(path) {
      extract_subject_id_metadata(path)$id
    },
    character(1)
  )
  output <- tempfile(fileext = ".RDa")
  on.exit(unlink(output), add = TRUE)

  result <- convert_directory_to_episodes(
    root,
    outpath = output,
    cores = 1L
  )
  observed_groups <- unique(data.frame(
    id = as.character(result$coding$id),
    subject = as.character(result$coding$subject)
  ))

  expect_setequal(observed_groups$id, expected_ids)
  expect_setequal(observed_groups$subject, expected_subjects)
})

test_that("directory episodes default IDs and subjects to fixture filenames - brazil", {
  root <- file.path(Sys.getenv("TEST_DATA"), "brazil")
  candidate_files <- list.files(
    root,
    recursive = TRUE,
    pattern = "\\.(txt|xlsx)$",
    full.names = TRUE
  )
  detailed_files <- candidate_files[vapply(
    candidate_files,
    function(path) {
      identical(synchrony_fr_header_metadata(path)$type, "detailed")
    },
    logical(1)
  )]
  expected_subjects <- tools::file_path_sans_ext(basename(detailed_files))
  expected_ids <- vapply(
    detailed_files,
    function(path) {
      value <- extract_subject_id_metadata(path)$id
      if (is.null(value)) NA_character_ else value
    },
    character(1)
  )
  output <- tempfile(fileext = ".RDa")
  on.exit(unlink(output), add = TRUE)

  result <- convert_directory_to_episodes(
    root,
    outpath = output,
    cores = 1L
  )
  observed_groups <- unique(data.frame(
    id = as.character(result$coding$id),
    subject = as.character(result$coding$subject)
  ))

  expect_setequal(observed_groups$id, unname(expected_ids))
  expect_setequal(observed_groups$subject, expected_subjects)

  expect_true(anyDuplicated(observed_groups$id) > 0L)

  expect_false(anyDuplicated(observed_groups$subject) > 0L)
})
