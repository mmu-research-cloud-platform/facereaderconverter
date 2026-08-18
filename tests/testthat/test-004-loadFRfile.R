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
    file.path("testdata", "testdata_detailed.csv"),
    clean_names = TRUE
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
})

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

test_that("loadFRfile rejects unsupported extensions", {
  tmp <- tempfile(fileext = ".rds")
  file.copy(file.path("testdata", "testdata_detailed.txt"), tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_snapshot(error = TRUE, {
    loadFRfile(tmp)
  })
})
