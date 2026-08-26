TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("readxl")

test_that("loadFRfile rejects unsupported extensions", {
  tmp <- tempfile(fileext = ".rds")
  file.copy(file.path("testdata", "testdata_detailed.txt"), tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_snapshot(error = TRUE, {
    loadFRfile(tmp)
  })
})
