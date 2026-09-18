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
