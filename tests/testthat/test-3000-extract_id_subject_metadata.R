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

test_that("extract_subject_id_metadata separates media and FaceReader names", {
  path <- file.path(
    TEST_DATA,
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
  )
  metadata <- extract_subject_id_metadata(path)

  expect_identical(
    metadata$id,
    "#8892 Sussed card game teen view point synced 10 minutes_1"
  )
  expect_identical(
    metadata$subject,
    "Participant 10_8892_Analysis 1_video_20260721_125850_detailed"
  )
})

test_that("extract_subject_id_metadata applies patterns to their source names", {
  path <- file.path(
    TEST_DATA,
    "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
  )
  metadata <- extract_subject_id_metadata(
    path,
    id_pattern = "8892",
    subject_pattern = "detailed"
  )

  expect_identical(metadata$id, "8892")
  expect_identical(metadata$subject, "detailed")
})

test_that("extract_subject_id_metadata applies patterns to full paths", {
  path <- list.files(
    file.path(TEST_DATA, "brazil"),
    pattern = "Participant 10.*detailed\\.xlsx$",
    full.names = TRUE
  )[[1L]]
  metadata <- extract_subject_id_metadata(
    path,
    id_pattern = "Downloads[/\\\\]#8883",
    subject_pattern = "brazil[/\\\\]Participant 10",
    use_full_path = TRUE
  )

  expect_match(metadata$id, "Downloads[/\\\\]#8883")
  expect_match(metadata$subject, "brazil[/\\\\]Participant 10")
})

test_that("extract_subject_id_metadata returns NA for missing media filename", {
  path <- file.path(TEST_DATA, "testdata_detailed.csv")
  metadata <- extract_subject_id_metadata(path)

  expect_true(is.na(metadata$id))
  expect_identical(metadata$subject, tools::file_path_sans_ext(basename(path)))
})

test_that("extract_id_subject_metadata remains a compatibility alias", {
  expect_identical(
    extract_id_subject_metadata(file.path(
      TEST_DATA,
      "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
    )),
    extract_subject_id_metadata(file.path(
      TEST_DATA,
      "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
    ))
  )
})
