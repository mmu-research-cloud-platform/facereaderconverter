TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))


test_that("loadFRfile adds scalar id and subject metadata", {
  path <- testthat::test_path("testdata", "testdata_detailed.txt")

  data <- loadFRfile(path, id = 1, subject = "child")

  expect_true(all(data$id == 1))
  expect_true(all(data$subject == "child"))
})

test_that("FR9 conversion derives four-digit ids and subjects from filenames", {
  input_dir <- file.path(TEST_DATA, "FR9")
  skip_if(
    !dir.exists(input_dir),
    "The FR9 directory fixture is not available"
  )

  input_files <- list.files(
    input_dir,
    pattern = "\\.(txt|xlsx|csv)$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  skip_if(
    length(input_files) == 0L,
    "The FR9 directory contains no supported files"
  )

  metadata <- lapply(input_files, extract_id_subject_metadata)
  valid <- vapply(
    metadata,
    function(x) !is.na(x$id) && !is.na(x$subject),
    logical(1)
  )
  skip_if(
    !any(valid),
    "No FR9 filenames contain both a four-digit id and mum/teen subject"
  )
  input_files <- input_files[valid]
  metadata <- metadata[valid]
  expect_true(all(grepl(
    "^[0-9]{4}$",
    vapply(metadata, `[[`, character(1), "id")
  )))
  expect_true(all(
    vapply(metadata, `[[`, character(1), "subject") %in% c("mum", "teen")
  ))

  converted <- Map(
    loadFRfile,
    input_files,
    MoreArgs = list(
      id = function(path) extract_id_subject_metadata(path)$id,
      subject = function(path) extract_id_subject_metadata(path)$subject
    )
  )
  expect_true(all(vapply(
    seq_along(converted),
    function(i) all(as.character(converted[[i]]$id) == metadata[[i]]$id),
    logical(1)
  )))
  expect_true(all(vapply(
    seq_along(converted),
    function(i) all(converted[[i]]$subject == metadata[[i]]$subject),
    logical(1)
  )))
})
