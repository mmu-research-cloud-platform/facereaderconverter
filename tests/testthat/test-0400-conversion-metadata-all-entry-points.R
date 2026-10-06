TEST_DATA <- Sys.getenv("TEST_DATA")

test_data_path <- file.path(TEST_DATA, "test_data.RDa")
test_data_error <- tryCatch(
  {
    load(test_data_path)
    NULL
  },
  error = identity
)
if (inherits(test_data_error, "error")) {
  testthat::skip(sprintf(
    "Could not load test data fixtures: %s",
    test_data_error$message
  ))
}

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
  output_dir <- tempfile("metadata_all_entry_points_")
  dir.create(input_dir)
  dir.create(output_dir)
  on.exit(
    unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  test_started <- Sys.time()
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

  writeLines("stale output", result$outpath[[1]])
  age_files(c(result$outpath, file.path(output_dir, "metadata.csv")))
  test_started <- Sys.time()
  result <- convertFRDirectory(
    input_dir,
    output_dir,
    id = id_for_path,
    subject = subject_for_path,
    cores = 1L
  )

  expect_true(all(result$status == "Success"))
  expect_true(all(file.exists(result$outpath)))
  expect_files_modified_since(result$outpath, test_started)
  expect_files_modified_since(
    file.path(output_dir, "metadata.csv"),
    test_started
  )
  expect_false("stale output" %in% readLines(result$outpath[[1]]))
  expect_setequal(
    basename(result$outpath),
    c(
      "1001_mum_source_txt_detailed.csv",
      "1002_teen_source_xlsx_detailed.csv",
      "1003_child_source_csv_detailed.csv"
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

test_that("convertFRDirectory infers metadata from full paths", {
  source <- file.path(
    TEST_DATA,
    "brazil/Participant 10_Participant 10_Analysis 1_video_20260918_143140_detailed.xlsx"
  )
  input_dir <- tempfile("full_path_input_")
  dir.create(file.path(input_dir, "brazil"), recursive = TRUE)
  output_dir <- tempfile("full_path_output_")
  dir.create(output_dir)
  on.exit(
    unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE),
    add = TRUE
  )
  file.copy(source, file.path(input_dir, "brazil", basename(source)))

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    id_pattern = "8883",
    subject_pattern = "brazil",
    use_full_path = TRUE,
    cores = 1L,
    save_metadata = NULL
  )

  expect_identical(result$status, "Success")
  data <- readr::read_csv(result$outpath, show_col_types = FALSE)
  expect_true(all(data$id == "8883"))
  expect_true(all(data$subject == "brazil"))
})
