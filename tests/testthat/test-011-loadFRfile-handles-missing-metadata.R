TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("readxl")

test_that("loadFRfile handles missing metadata", {
  expect_no_error(suppressWarnings(loadFRfile(
    file.path("testdata", "testdata_extracols_nometadata_detailed.xlsx"),
    clean_names = TRUE,
    values_as_numeric = TRUE
  )))
})
