library(testthat)

write_directory_detailed <- function(path, fps = "30", type = "detailed") {
  writeLines(
    c(
      paste("Video analysis", type, "log"),
      "",
      "Face Model\tGeneral",
      "Calibration\t-",
      "Start time\t6/4/2026 13:31:06.331",
      "Filename\tsession.mp4",
      paste("Frame rate", fps, sep = "\t"),
      "",
      "Video Time\tNeutral\tHappy",
      sprintf("00:00:00.%03d\t0\t0.6", seq(0L, 363L, by = 33L))
    ),
    path
  )
}

test_that("directory episodes combine detailed files, default metadata, and save RDa", {
  root <- tempfile("directory-episodes-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(root, "nested"))
  write_directory_detailed(file.path(root, "first.txt"), fps = "29.97")
  write_directory_detailed(file.path(root, "nested", "second.txt"), fps = "30")
  write_directory_detailed(file.path(root, "state.txt"), type = "state")

  result <- convert_directory_to_episodes(root, cores = 1L)
  expect_s3_class(result, "fr_coding")
  expect_identical(result$metadata$fps, 30L)
  expect_setequal(as.character(unique(result$coding$id)), c("first", "second"))
  expect_setequal(
    as.character(unique(result$coding$subject)),
    c("first", "second")
  )
  expect_true(nrow(result$episodes) > 0L)
  saved <- new.env(parent = emptyenv())
  expect_identical(
    load(file.path(root, "episodes.RDa"), envir = saved),
    "coded_data"
  )
  expect_equal(saved$coded_data, result)
})

test_that("directory episodes use optional ID and subject regex independently", {
  parent <- tempfile("directory-episodes-regex-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  write_directory_detailed(file.path(root, "1234 mum.txt"))
  write_directory_detailed(file.path(root, "1234 teen.txt"))
  output <- file.path(root, "result.RDa")

  result <- convert_directory_to_episodes(
    root,
    outpath = output,
    id_pattern = "[0-9]{4}",
    subject_pattern = "mum|teen",
    cores = 1L
  )
  expect_equal(unique(as.character(result$coding$id)), "1234")
  expect_setequal(as.character(unique(result$coding$subject)), c("mum", "teen"))
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
    id_pattern = "[0-9]{4}",
    cores = 1L
  )
  expect_setequal(
    as.character(unique(only_id$coding$subject)),
    c("1234 mum", "1234 teen")
  )
})

test_that("directory episodes reject conflicting or missing FPS before saving", {
  root <- tempfile("directory-episodes-fps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  first <- file.path(root, "first.txt")
  second <- file.path(root, "second.txt")
  write_directory_detailed(first, fps = "24")
  write_directory_detailed(second, fps = "30")

  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root),
      error = identity
    )),
    "Conflicting frame rates"
  )
  expect_equal(file.exists(file.path(root, "episodes.RDa")), FALSE)
  write_directory_detailed(second, fps = "missing")
  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root),
      error = identity
    )),
    "Invalid or missing frame rate"
  )
})

test_that("directory episodes reject duplicate resolved groups", {
  parent <- tempfile("directory-episodes-duplicate-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  write_directory_detailed(file.path(root, "1234 mum A.txt"))
  write_directory_detailed(file.path(root, "1234 mum B.txt"))

  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(
        root,
        id_pattern = "[0-9]{4}",
        subject_pattern = "mum"
      ),
      error = identity
    )),
    "Multiple exports resolve to the same ID and subject"
  )
})

test_that("directory episodes reject multi-participant exports", {
  root <- tempfile("directory-episodes-participants-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(root, "mixed.txt")
  writeLines(
    c(
      "Video analysis detailed log",
      "",
      "Face Model\tGeneral",
      "Calibration\t-",
      "Start time\t6/4/2026 13:31:06.331",
      "Filename\tsession.mp4",
      "Frame rate\t30",
      "",
      "Video Time\tNeutral\tHappy\tParticipant Name",
      "00:00:00.000\t0\t0.6\tA",
      "00:00:00.033\t0\t0.6\tB"
    ),
    path
  )
  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root),
      error = identity
    )),
    "Multiple participants in one export"
  )
  expect_equal(file.exists(file.path(root, "episodes.RDa")), FALSE)
})

test_that("directory episodes reject an unmatched supplied regex", {
  root <- file.path(tempfile("directory-episodes-regex-error-"), "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(dirname(root), recursive = TRUE, force = TRUE), add = TRUE)
  write_directory_detailed(file.path(root, "sample.txt"))

  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root, id_pattern = "[0-9]{4}"),
      error = identity
    )),
    "ID or subject pattern did not match"
  )
  expect_equal(file.exists(file.path(root, "episodes.RDa")), FALSE)
})
