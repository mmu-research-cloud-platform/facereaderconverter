TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

test_that("convertFRExcelFiles adds id and subject metadata", {
  skip_if_not_installed("readxl")

  data <- convertFRExcelFiles(
    testthat::test_path("testdata", "testdata_excel_detailed.xlsx"),
    id = 1218,
    subject = "mum"
  )

  expect_true(all(data$id == 1218))
  expect_true(all(data$subject == "mum"))
})

test_that("convertFRDirectory adds metadata across all supported file types", {
  skip_if_not_installed("readxl")

  input_dir <- tempfile("fr_metadata_input_")
  output_dir <- tempfile("fr_metadata_output_")
  dir.create(input_dir)
  on.exit(
    unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  input_files <- c(
    testthat::test_path("testdata", "testdata_detailed.txt"),
    testthat::test_path("testdata", "testdata_excel_detailed.xlsx"),
    file.path(TEST_DATA, "testdata_detailed.csv")
  )
  input_names <- c("source_txt.txt", "source_xlsx.xlsx", "source_csv.csv")
  file.copy(input_files, file.path(input_dir, input_names))

  id_for_path <- function(path) {
    switch(
      tolower(tools::file_ext(path)),
      txt = "1001",
      xlsx = "1002",
      csv = "1003"
    )
  }
  subject_for_path <- function(path) {
    switch(
      tolower(tools::file_ext(path)),
      txt = "mum",
      xlsx = "teen",
      csv = "child"
    )
  }

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    id = id_for_path,
    subject = subject_for_path,
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_setequal(
    basename(result$outpath),
    c(
      "1001_mum_detailed.csv",
      "1002_teen_detailed.csv",
      "1003_child_detailed.csv"
    )
  )

  converted <- lapply(result$outpath, readr::read_csv, show_col_types = FALSE)
  expect_setequal(
    vapply(
      converted,
      function(data) as.character(unique(data$id)),
      character(1)
    ),
    c("1001", "1002", "1003")
  )
  expect_setequal(
    vapply(converted, function(data) unique(data$subject), character(1)),
    c("mum", "teen", "child")
  )
})
