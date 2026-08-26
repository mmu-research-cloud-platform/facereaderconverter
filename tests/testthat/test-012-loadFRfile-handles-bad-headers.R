TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("readxl")

test_that("loadFRfile handles bad headers", {
  expect_message(
    bad_header <- loadFRfile(file.path(
      "testdata",
      "testdata_detailed_fail.txt"
    )),
    "FaceReader header row not found"
  )
  expect_null(bad_header)
})
