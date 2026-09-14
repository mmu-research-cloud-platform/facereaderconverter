TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

test_that("extract_subject_id_metadata trims four-digit ids and extracts subjects", {
  id_pattern <- "(?<![0-9])\\s*[0-9]{4}\\s*(?![0-9])"
  subject_pattern <- "(?<![a-z])(mum|teen)(?![a-z])"

  mum <- extract_subject_id_metadata(
    "FR9  1218  mum_Analysis_detailed.xlsx",
    id_pattern,
    subject_pattern
  )
  teen <- extract_subject_id_metadata(
    "FR9  0042  TEEN_Analysis_state.xlsx",
    id_pattern,
    subject_pattern
  )

  expect_identical(mum$id, "1218")
  expect_identical(mum$subject, "mum")
  expect_identical(teen$id, "0042")
  expect_identical(teen$subject, "teen")
})

test_that("extract_subject_id_metadata returns missing values when patterns do not match", {
  metadata <- extract_subject_id_metadata(
    "researcher_Analysis_detailed.xlsx",
    id_pattern = "(?<![0-9])\\s*[0-9]{4}\\s*(?![0-9])",
    subject_pattern = "(?<![a-z])(mum|teen)(?![a-z])"
  )

  expect_true(is.na(metadata$id))
  expect_true(is.na(metadata$subject))
})

test_that("extract_id_subject_metadata remains a compatibility alias", {
  expect_identical(
    extract_id_subject_metadata("FR9 1218 mum_Analysis_detailed.xlsx"),
    extract_subject_id_metadata("FR9 1218 mum_Analysis_detailed.xlsx")
  )
})
