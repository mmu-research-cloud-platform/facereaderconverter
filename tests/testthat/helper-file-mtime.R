expect_files_modified_since <- function(paths, started_at, tolerance = 2) {
  expect_gt(length(paths), 0L)
  info <- file.info(paths)
  expect_true(all(!is.na(info$mtime)))
  expect_true(all(info$mtime >= started_at - tolerance))
}

age_files <- function(paths, seconds = 60) {
  paths <- paths[file.exists(paths)]
  if (length(paths) > 0L) {
    Sys.setFileTime(paths, Sys.time() - seconds)
  }
}
