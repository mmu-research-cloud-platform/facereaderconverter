TEST_DATA <- Sys.getenv("TEST_DATA")

txt_fixture <- file.path(
  TEST_DATA,
  "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
)
xlsx_fixture <- file.path(
  TEST_DATA,
  "FR9/FR9 1218 participant_Analysis 1_video_20260910_110344_detailed.xlsx"
)

write_txt_fixture <- function(destination, fps = NULL, keep_fps_line = TRUE) {
  lines <- readLines(txt_fixture, warn = FALSE)
  if (!keep_fps_line) {
    lines <- lines[!grepl("^Frame rate", lines)]
  } else if (!is.null(fps)) {
    lines <- sub("^Frame rate.*", paste0("Frame rate\t", fps), lines)
  }
  writeLines(lines, destination)
}

fps_for <- function(metadata_fps, id) {
  if (is.data.frame(metadata_fps)) {
    metadata_fps$fps[match(as.character(id), as.character(metadata_fps$id))]
  } else {
    metadata_fps
  }
}

test_that("directory conversion accepts different frame rates for different ids", {
  root <- tempfile("directory-mixed-fps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(xlsx_fixture, file.path(root, "sixteen.xlsx"))
  file.copy(txt_fixture, file.path(root, "thirty.txt"))

  result <- convert_directory_to_episodes(root, cores = 1L)
  fps <- result$metadata$fps

  expect_s3_class(fps, "data.table")
  expect_equal(nrow(fps), 2L)
  # The FR9 XLSX header stores 16.00047 rather than exactly 16
  expect_equal(sort(fps$fps), c(16, 30), tolerance = 1e-4)
  ids <- unique(result$coding[subject == "sixteen"]$id)
  expect_equal(fps_for(fps, ids), 16, tolerance = 1e-4)
  ids <- unique(result$coding[subject == "thirty"]$id)
  expect_equal(fps_for(fps, ids), 30)
})

test_that("durations follow each id's inherited frame rate", {
  root <- tempfile("directory-mixed-fps-durations-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(xlsx_fixture, file.path(root, "sixteen.xlsx"))
  file.copy(txt_fixture, file.path(root, "thirty.txt"))

  result <- convert_directory_to_episodes(root, cores = 1L)
  fps <- result$metadata$fps
  episodes <- result$episodes
  expect_gt(nrow(episodes), 0L)
  expect_equal(
    episodes$duration_s,
    episodes$n_frames / fps_for(fps, episodes$id)
  )
})

test_that("a non-integer header rate is kept in the directory result", {
  root <- tempfile("directory-noninteger-fps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_txt_fixture(file.path(root, "ntsc.txt"), fps = "29.970000000")

  result <- convert_directory_to_episodes(root, cores = 1L)
  expect_equal(result$metadata$fps, 29.97)
})

test_that("files for the same id with the same rate are accepted", {
  root <- tempfile("directory-same-id-fps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_txt_fixture(file.path(root, "first.txt"))
  write_txt_fixture(file.path(root, "second.txt"))

  result <- convert_directory_to_episodes(root, cores = 1L)
  expect_identical(result$metadata$fps, 30L)
  expect_setequal(
    unique(as.character(result$coding$subject)),
    c("first", "second")
  )
})

test_that("a file with no header rate gets an inferred rate", {
  root <- tempfile("directory-inferred-fps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_txt_fixture(file.path(root, "nofps.txt"), keep_fps_line = FALSE)

  result <- suppressMessages(convert_directory_to_episodes(root, cores = 1L))
  expect_identical(result$metadata$fps, 30L)
})

test_that("inferred and header rates for the same id must agree", {
  root <- tempfile("directory-inferred-conflict-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_txt_fixture(file.path(root, "first.txt"), fps = "25.000000000")
  write_txt_fixture(file.path(root, "second.txt"), keep_fps_line = FALSE)

  expect_error(
    suppressMessages(convert_directory_to_episodes(
      root,
      skip_fails = FALSE,
      cores = 1L
    )),
    "Conflicting frame rates"
  )
})

test_that("an inferred rate within tolerance of a header rate for the same id is accepted", {
  root <- tempfile("directory-inferred-agree-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_txt_fixture(file.path(root, "first.txt"), fps = "30.000000000")
  write_txt_fixture(file.path(root, "second.txt"), keep_fps_line = FALSE)

  result <- suppressMessages(convert_directory_to_episodes(root, cores = 1L))
  expect_identical(result$metadata$fps, 30L)
})

test_that("an unusable header rate with too little data is skipped", {
  root <- tempfile("directory-no-fps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_txt_fixture(file.path(root, "good.txt"))
  writeLines(
    c("Video analysis detailed log", "Filename\tbroken.mp4", "Frame rate\tabc"),
    file.path(root, "bad.txt")
  )

  expect_warning(
    result <- convert_directory_to_episodes(
      root,
      skip_fails = TRUE,
      cores = 1L
    ),
    "bad\\.txt"
  )
  expect_equal(unique(as.character(result$coding$subject)), "good")
})
