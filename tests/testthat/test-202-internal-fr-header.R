test_that("102 internal fr header detects the header row", {
  lines <- c(
    "Video analysis detailed log",
    "some metadata",
    "  Video Time  Neutral Happy"
  )

  expect_equal(detect_fr_header_row(lines), 3L)
})

test_that("102 internal fr header fails when no header is present", {
  lines <- c(
    "Video analysis detailed log",
    "some metadata",
    "no matching header here"
  )

  expect_error(
    detect_fr_header_row(lines),
    "FaceReader header row not found"
  )
})
