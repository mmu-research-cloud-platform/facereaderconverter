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
