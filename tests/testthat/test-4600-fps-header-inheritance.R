TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

txt_fixture <- file.path(
  TEST_DATA,
  "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
)
xlsx_fixture <- file.path(
  TEST_DATA,
  "FR9/FR9 1218 participant_Analysis 1_video_20260910_110344_detailed.xlsx"
)

# Fixture text with the "Frame rate" line replaced or removed
txt_with_fps <- function(destination, fps = NULL) {
  lines <- readLines(txt_fixture, warn = FALSE)
  lines <- if (is.null(fps)) {
    lines[!grepl("^Frame rate", lines)]
  } else {
    sub("^Frame rate.*", paste0("Frame rate\t", fps), lines)
  }
  writeLines(lines, destination)
  destination
}

test_that("convertFRFiles carries the header fps on returned data", {
  data <- convertFRFiles(txt_fixture, return_data = TRUE)
  expect_equal(fr_inherited_fps(data), 30)
})

test_that("a non-integer header rate is kept unrounded", {
  path <- txt_with_fps(tempfile(fileext = ".txt"), "29.970000000")
  on.exit(unlink(path), add = TRUE)
  data <- convertFRFiles(path, return_data = TRUE)
  expect_equal(fr_inherited_fps(data), 29.97)
})

test_that("data from a file without a header rate carries no fps", {
  path <- txt_with_fps(tempfile(fileext = ".txt"), NULL)
  on.exit(unlink(path), add = TRUE)
  data <- convertFRFiles(path, return_data = TRUE)
  expect_null(fr_inherited_fps(data))
})

test_that("convertFRExcelFiles carries the header fps on returned data", {
  data <- convertFRExcelFiles(xlsx_fixture, return_data = TRUE)
  # The FR9 XLSX header stores 16.00047 rather than exactly 16
  expect_equal(fr_inherited_fps(data), 16, tolerance = 1e-4)
})

test_that("loadFRfile passes the header fps through", {
  expect_equal(fr_inherited_fps(loadFRfile(txt_fixture)), 30)
  expect_equal(fr_inherited_fps(loadFRfile(xlsx_fixture)), 16, tolerance = 1e-4)
})

test_that("added id and subject columns are unchanged by fps inheritance", {
  data <- loadFRfile(txt_fixture)
  expect_true(all(c("id", "subject", "video_time") %in% names(data)))
  expect_false("fps" %in% names(data))
})

test_that("convert_to_episodes inherits the header fps without an fps argument", {
  data <- loadFRfile(xlsx_fixture)
  keep <- c("id", "subject", "video_time", "neutral", "happy", "sad", "angry")
  coding <- data[seq_len(2000), intersect(keep, names(data))]
  attr(coding, "fr_metadata") <- attr(data, "fr_metadata")
  res <- expect_no_message(
    convert_to_episodes(coding, cores = 1L)
  )
  expect_equal(res$metadata$fps, 16, tolerance = 1e-4)
})

test_that("a supplied fps that conflicts with the header warns and loses", {
  data <- loadFRfile(xlsx_fixture)
  keep <- c("id", "subject", "video_time", "neutral", "happy")
  coding <- data[seq_len(2000), intersect(keep, names(data))]
  attr(coding, "fr_metadata") <- attr(data, "fr_metadata")
  expect_warning(
    res <- convert_to_episodes(coding, fps = 30, cores = 1L),
    "conflicts with stored metadata"
  )
  expect_equal(res$metadata$fps, 16, tolerance = 1e-4)
})

test_that("fps is used only when the file has no frame rate", {
  path <- txt_with_fps(tempfile(fileext = ".txt"), NULL)
  on.exit(unlink(path), add = TRUE)
  data <- loadFRfile(path)
  keep <- c("id", "subject", "video_time", "neutral", "happy")
  coding <- data[seq_len(2000), intersect(keep, names(data))]
  res <- convert_to_episodes(coding, fps = 24, cores = 1L)
  expect_identical(res$metadata$fps, 24L)
})

test_that("fps is inferred when neither file nor argument supplies one", {
  path <- txt_with_fps(tempfile(fileext = ".txt"), NULL)
  on.exit(unlink(path), add = TRUE)
  data <- loadFRfile(path)
  keep <- c("id", "subject", "video_time", "neutral", "happy")
  coding <- data[seq_len(2000), intersect(keep, names(data))]
  expect_message(
    res <- convert_to_episodes(coding, cores = 1L),
    "inferred"
  )
  expect_identical(res$metadata$fps, 30L)
})
