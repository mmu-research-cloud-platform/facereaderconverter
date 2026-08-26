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
