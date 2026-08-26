test_that("102 internal fr header detects the header row", {
  lines <- c(
    "Video analysis detailed log",
    "some metadata",
    "  Video Time  Neutral Happy"
  )

  expect_equal(detect_fr_header_row(lines), 3L)
})
