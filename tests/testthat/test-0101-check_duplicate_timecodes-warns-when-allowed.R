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
