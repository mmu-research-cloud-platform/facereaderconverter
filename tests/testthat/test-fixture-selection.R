test_that("fixture selection is real-first with an explicit synthetic override", {
  public <- testthat::test_path("..", "fixtures", "synthetic")
  expected <- normalizePath(public, winslash = "/", mustWork = TRUE)
  expect_identical(select_test_data_fixture(public, "auto", public), expected)
  expect_identical(select_test_data_fixture("", "auto", public), expected)
  expect_identical(select_test_data_fixture("missing", "auto", public), expected)
  expect_identical(select_test_data_fixture("missing", "synthetic", public), expected)
  expect_error(select_test_data_fixture("", "real", public), "Real fixtures requested")
  expect_error(select_test_data_fixture("", "invalid", public), "must be auto, real, or synthetic")
  expect_error(select_test_data_fixture("", "synthetic", "missing"), "bundled synthetic fixture")

  real <- tempfile(".fixture-work-selection-", tmpdir = testthat::test_path())
  dir.create(real)
  withr::defer(unlink(real, recursive = TRUE))
  file.create(file.path(real, "test_data.RDa"))
  expect_identical(
    select_test_data_fixture(real, "auto", public),
    normalizePath(real, winslash = "/", mustWork = TRUE)
  )
  expect_identical(select_test_data_fixture(real, "synthetic", public), expected)
})

test_that("public RDa contains internally consistent invented coding", {
  fixture <- new.env(parent = emptyenv())
  load(testthat::test_path("..", "fixtures", "synthetic", "test_data.RDa"), fixture)
  expect_setequal(names(fixture), c(
    "test_coding", "test_coding_wide", "test_data_sync2", "test_deltas"
  ))
  expect_setequal(unique(fixture$test_coding$id), c(1L, 3L, 6L))
  expect_setequal(unique(fixture$test_coding$subject), c("parent", "teen"))
  expect_true(anyNA(fixture$test_coding$value))
  expect_true(all(na.omit(fixture$test_coding$value) >= 0))
  expect_true(all(na.omit(fixture$test_coding$value) <= 1))
  expect_equal(nrow(fixture$test_coding), 7L * nrow(fixture$test_coding_wide))
  expect_s3_class(fixture$test_data_sync2, "fr_coding")
  expect_gt(nrow(fixture$test_data_sync2$episodes), 0L)
  expect_named(fixture$test_deltas, c(
    "id", "subject", "video_time", "behavior", "duration",
    "modifier_1", "modifier_2", "modifier_3", "modifier_4", "comment",
    "emotion", "value", "frame", "delta"
  ))
})

test_that("the common fixture loader populates its caller", {
  fixture <- local({
    load_test_data_fixture()
    test_coding
  })
  expect_true(all(c("id", "subject", "video_time", "emotion", "value") %in% names(fixture)))
})

test_that("invented public exports exercise CSV, TXT and XLSX conversion", {
  public <- testthat::test_path("..", "fixtures", "synthetic")
  csv <- loadFRfile(file.path(public, "testdata_detailed.csv"))
  txt <- loadFRfile(file.path(public, "FR10", "synthetic_txt_detailed.txt"))
  xlsx <- loadFRfile(file.path(public, "FR10", "synthetic_xlsx_detailed.xlsx"))
  state <- loadFRfile(file.path(public, "FR9", "synthetic_fr9_state.xlsx"))
  expect_gt(nrow(csv), 0L)
  expect_equal(txt[setdiff(names(txt), "subject")], xlsx[setdiff(names(xlsx), "subject")])
  expect_true(all(c("stimulus", "event_marker", "speech_rate") %in% names(txt)))
  expect_true(all(state$id == "synthetic-fr9"))
  expect_true("dominant_expression" %in% names(state))
  expect_equal(
    fr_inherited_fps(loadFRfile(file.path(public, "FR9", "synthetic_16fps_detailed.xlsx"))),
    16
  )
  metadata <- extract_subject_id_metadata(
    file.path(public, "FR10", "synthetic_txt_detailed.txt")
  )
  expect_identical(metadata$id, "synthetic-fr10")
  expect_identical(metadata$subject, "synthetic_txt_detailed")
})

test_that("invented directory exports produce combined episodes", {
  public <- testthat::test_path("..", "fixtures", "synthetic", "c2e-directory")
  output <- tempfile(".fixture-work-episodes-", tmpdir = testthat::test_path(), fileext = ".RDa")
  withr::defer(unlink(output))
  result <- convert_directory_to_episodes(public, outpath = output, cores = 1L)
  expect_s3_class(result, "fr_coding")
  expect_equal(length(unique(result$coding$id)), 3L)
  expect_equal(length(unique(result$coding$subject)), 6L)
  expect_gt(nrow(result$episodes), 0L)
  expect_true(file.exists(output))
})
