library(testthat)

copy_4400_fixture <- function(destination, index = 1L) {
  file.copy(
    test_fixture_path(if (index == 1L) "c2e_primary_txt" else "c2e_secondary_txt"),
    destination
  )
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
  detailed_files <- test_fixture_paths("c2e_detailed_txt")
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
    tools::file_path_sans_ext(basename(detailed_files))
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
  first_name <- paste0(test_fixture_id("8892"), "-mum.txt")
  second_name <- paste0(test_fixture_id("8895"), "-teen.txt")
  copy_4400_fixture(file.path(root, first_name), 1L)
  copy_4400_fixture(file.path(root, second_name), 2L)
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
    test_fixture_id(c("8892", "8894"))
  )
  expect_setequal(
    as.character(unique(result$coding$subject)),
    c("mum", "teen")
  )
  expect_true(file.exists(output))
  expect_warning(
    existing_result <- convert_directory_to_episodes(
      root,
      outpath = output,
      cores = 1L
    ),
    "Output already exists.*skipping conversion"
  )
  expect_equal(existing_result, result)

  only_id <- convert_directory_to_episodes(
    root,
    id_pattern = "(?<![0-9])[0-9]{4}(?![0-9])",
    cores = 1L
  )
  expect_setequal(
    as.character(unique(only_id$coding$subject)),
    c(
      tools::file_path_sans_ext(c(first_name, second_name))
    )
  )
})

test_that("directory episodes reject conflicting FPS for the same id when skip_fails is FALSE", {
  parent <- tempfile("directory-episodes-fps-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  fixtures <- test_fixture_fps_conflict_paths()
  # Same media file (so the same id) but a different recorded frame rate
  file.copy(fixtures[[1L]], file.path(root, "first.txt"))
  file.copy(fixtures[[2L]], file.path(root, "second.txt"))

  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root, skip_fails = FALSE, cores = 1L),
      error = identity
    )),
    "Conflicting frame rates"
  )
  expect_false(file.exists(file.path(root, "episodes.RDa")))
})

test_that("directory episodes reject duplicate IDs and subjects when skip_fails is FALSE", {
  parent <- tempfile("directory-episodes-duplicate-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  source <- test_fixture_path("c2e_primary_txt")
  dir.create(file.path(root, "nested-a"))
  dir.create(file.path(root, "nested-b"))
  file.copy(source, file.path(root, "nested-a", "same-a.txt"))
  file.copy(source, file.path(root, "nested-b", "same-b.txt"))

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
})

test_that("directory episodes reject unmatched fixture regexes", {
  parent <- tempfile("directory-episodes-regex-error-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  copy_4400_fixture(file.path(root, "sample.txt"))

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "NO-MATCH",
    cores = 1L
  )
  expect_true(all(is.na(as.character(result$coding$id))))
  expect_equal(unique(as.character(result$coding$subject)), "sample")
  expect_true(file.exists(file.path(root, "episodes.RDa")))
})
