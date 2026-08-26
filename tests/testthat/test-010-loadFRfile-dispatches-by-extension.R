TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("readxl")

test_that("loadFRfile dispatches by extension", {
  txt <- loadFRfile(
    file.path("testdata", "testdata_detailed.txt"),
    clean_names = TRUE,
    values_as_numeric = TRUE
  )
  xlsx <- loadFRfile(
    file.path("testdata", "testdata_excel_detailed.xlsx"),
    clean_names = TRUE,
    values_as_numeric = TRUE
  )
  csv <- loadFRfile(
    file.path(TEST_DATA, "testdata_detailed.csv"),
    clean_names = TRUE
  )
  extra <- loadFRfile(
    file.path("testdata", "testdata_extracols_detailed.xlsx"),
    clean_names = TRUE,
    values_as_numeric = TRUE
  )

  expect_equal(
    txt,
    convertFRFiles(
      file.path("testdata", "testdata_detailed.txt"),
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE
    ),
    tolerance = 1e-4
  )
  expect_equal(
    xlsx,
    convertFRExcelFiles(
      file.path("testdata", "testdata_excel_detailed.xlsx"),
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE
    ),
    tolerance = 1e-4
  )
  expect_identical(
    csv,
    readr::read_csv(
      file.path("testdata", "testdata_detailed.csv"),
      show_col_types = FALSE
    ) |>
      janitor::clean_names()
  )
  expect_true(ncol(extra) == 13)
})
