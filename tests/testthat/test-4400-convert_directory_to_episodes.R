library(testthat)

copy_4400_fixture <- function(relative_path, destination) {
  file.copy(file.path(Sys.getenv("TEST_DATA"), relative_path), destination)
}

test_that("directory episodes combine fixture exports and save RDa", {
  root <- file.path(Sys.getenv("TEST_DATA"), "c2e-directory")
  output <- tempfile(fileext = ".RDa")
  on.exit(unlink(output), add = TRUE)

  result <- convert_directory_to_episodes(
    root,
    outpath = output,
    cores = 1L
  )
  expect_s3_class(result, "fr_coding")
  expect_identical(result$metadata$fps, 30L)
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
  expect_setequal(
    as.character(unique(result$coding$id)),
    unname(vapply(
      detailed_files,
      function(path) {
        value <- extract_subject_id_metadata(path)$id
        if (is.null(value)) NA_character_ else value
      },
      character(1)
    ))
  )
  expect_setequal(
    as.character(unique(result$coding$subject)),
    tools::file_path_sans_ext(basename(list.files(
      root,
      recursive = TRUE,
      pattern = "_detailed\\.(txt|xlsx)$",
      full.names = TRUE
    )))
  )
  expect_gt(nrow(result$episodes), 0L)
  saved <- new.env(parent = emptyenv())
  expect_identical(load(output, envir = saved), "coded_data")
  expect_equal(saved$coded_data, result)
})

test_that("directory episodes use fixture filename regexes independently", {
  parent <- tempfile("directory-episodes-regex-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4400_fixture(
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt",
    file.path(root, "8892-mum.txt")
  )
  copy_4400_fixture(
    "c2e-directory/mum/8894/Participant 11_8894_Analysis 1_video_20260721_135456_detailed.txt",
    file.path(root, "8895-teen.txt")
  )
  output <- file.path(root, "result.RDa")

  result <- convert_directory_to_episodes(
    root,
    outpath = output,
    id_pattern = "(?<![0-9])[0-9]{4}(?![0-9])",
    subject_pattern = "mum|teen",
    cores = 1L
  )
  expect_setequal(
    as.character(unique(result$coding$id)),
    c(
      "8892",
      "8894"
    )
  )
  expect_setequal(
    as.character(unique(result$coding$subject)),
    c("mum", "teen")
  )
  expect_true(file.exists(output))
  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root, outpath = output, cores = 1L),
      error = identity
    )),
    "Output already exists"
  )

  only_id <- convert_directory_to_episodes(
    root,
    id_pattern = "(?<![0-9])[0-9]{4}(?![0-9])",
    cores = 1L
  )
  expect_setequal(
    as.character(unique(only_id$coding$subject)),
    c("8892-mum", "8895-teen")
  )
})

test_that("directory episodes reject fixture exports with conflicting FPS", {
  parent <- tempfile("directory-episodes-fps-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4400_fixture(
    "FR9/FR9 1218 participant_Analysis 1_video_20260910_110344_detailed.xlsx",
    file.path(root, "first.xlsx")
  )
  copy_4400_fixture(
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt",
    file.path(root, "second.txt")
  )

  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root),
      error = identity
    )),
    "Conflicting frame rates"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("directory episodes reject duplicate media IDs and export subjects", {
  parent <- tempfile("directory-episodes-duplicate-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  source <- file.path(
    Sys.getenv("TEST_DATA"),
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
  )
  dir.create(file.path(root, "nested-a"))
  dir.create(file.path(root, "nested-b"))
  file.copy(source, file.path(root, "nested-a", "same-a.txt"))
  file.copy(source, file.path(root, "nested-b", "same-b.txt"))

  expect_error(
    convert_directory_to_episodes(
      root,
      id_pattern = "#[0-9]{4}",
      subject_pattern = "same",
      cores = 1L
    ),
    "Multiple exports resolve to the same ID and subject"
  )
})

test_that("directory episodes reject unmatched fixture regexes", {
  parent <- tempfile("directory-episodes-regex-error-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4400_fixture(
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt",
    file.path(root, "sample.txt")
  )

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "NO-MATCH",
    cores = 1L
  )
  expect_true(all(is.na(as.character(result$coding$id))))
  expect_equal(unique(as.character(result$coding$subject)), "sample")
  expect_true(file.exists(file.path(root, "episodes.RDa")))
})
