TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

test_that("loadFRfile passes read_csv args without leaking them into clean_names", {
  expect_no_error(
    loadFRfile(
      file.path("testdata", "testdata_detailed.csv"),
      locale = readr::locale()
    )
  )
})
