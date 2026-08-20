test_that("103 internal duplicate timecode errors by default", {
  df <- data.frame(
    `Video Time` = c("00:00:00.000", "00:00:00.000", "00:00:00.033"),
    Neutral = c(0, 0, 0),
    check.names = FALSE
  )

  expect_error(
    check_duplicate_timecodes(df),
    "Duplicate timecodes"
  )
})

test_that("103 internal duplicate timecode warns when allowed", {
  df <- data.frame(
    `Video Time` = c("00:00:00.000", "00:00:00.000", "00:00:00.033"),
    Neutral = c(0, 0, 0),
    check.names = FALSE
  )

  expect_warning(
    check_duplicate_timecodes(df, duplicate_timecodes_as_error = FALSE),
    "Duplicate timecodes"
  )
})

test_that("103 internal duplicate timecode respects participant grouping", {
  ok <- data.frame(
    `Participant Name` = c("Rebecca", "Rebecca", "Alex"),
    `Analysis Index` = c("Analysis 1", "Analysis 1", "Analysis 1"),
    `Video Time` = c("00:00:00.000", "00:00:00.033", "00:00:00.000"),
    Neutral = c(0, 0, 0),
    check.names = FALSE
  )
  bad <- ok
  bad$`Video Time`[2] <- "00:00:00.000"

  expect_no_error(check_duplicate_timecodes(ok))
  expect_error(check_duplicate_timecodes(bad), "Duplicate timecodes")
})
