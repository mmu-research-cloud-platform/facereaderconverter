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

test_that("synthetic four-digit IDs use a stable, reversible derangement", {
  ids <- synthetic_fixture_id_map()
  expected_ids <- as.character(1000:9999)

  expect_length(ids, 9000L)
  expect_setequal(names(ids), expected_ids)
  expect_true(all(as.integer(ids) %in% 1000:9999))
  expect_equal(length(unique(unname(ids))), 9000L)
  expect_false(any(names(ids) == unname(ids)))
  expect_identical(synthetic_fixture_id_map(), ids)
  expect_identical(
    map_synthetic_fixture_id(c("1234", "8892", "1234")),
    unname(ids[c("1234", "8892", "1234")])
  )
  expect_identical(
    unmap_synthetic_fixture_id(unname(ids[c("1234", "8892")])),
    c("1234", "8892")
  )

  set.seed(841)
  expected_next <- runif(1)
  set.seed(841)
  map_synthetic_fixture_id(c("1234", "8892"))
  expect_identical(runif(1), expected_next)

  public <- testthat::test_path("..", "fixtures", "synthetic")
  export <- file.path(
    public,
    "FR9",
    paste0(
      map_synthetic_fixture_id("8895"),
      " invented FR9 Participant 1_00024 invented_Analysis 2_video_20260101_120000_detailed.xlsx"
    )
  )
  metadata <- synchrony_fr_header_metadata(export)
  expect_true(grepl(map_synthetic_fixture_id("8895"), basename(export), fixed = TRUE))
  expect_true(grepl(
    paste0("#", map_synthetic_fixture_id("8895")),
    metadata$video_filename,
    fixed = TRUE
  ))
})

test_that("the common fixture loader populates its caller", {
  fixture <- local({
    load_test_data_fixture()
    test_coding
  })
  expect_true(all(c("id", "subject", "video_time", "emotion", "value") %in% names(fixture)))
})

test_that("synthetic Brazil has two mapped XLSX pairs and valid artificial media", {
  skip_if_not_installed("readxl")
  public <- testthat::test_path("..", "fixtures", "synthetic", "brazil")
  media <- test_fixture_brazil_media_names(public, "synthetic")
  top_level <- list.files(public, pattern = "detailed\\.xlsx$", full.names = TRUE)
  expect_length(top_level, 4L)
  headers <- lapply(top_level, synchrony_fr_header_metadata)
  metadata_names <- vapply(headers, `[[`, character(1), "video_filename")
  filenames <- basename(gsub("\\\\", "/", metadata_names))
  expected_ids <- map_synthetic_fixture_id(c("8883", "8884"))
  expect_length(unique(expected_ids), 2L)
  expect_setequal(
    media$matched,
    paste0("#", expected_ids, "_invented-session.mp4")
  )
  expect_setequal(filenames, media$matched)
  expect_equal(as.integer(table(filenames)), c(2L, 2L))
  for (i in seq_along(top_level)) {
    id <- unmap_synthetic_fixture_id(sub("^#([0-9]{4}).*$", "\\1", filenames[[i]]))
    expect_true(id %in% c("8883", "8884"))
    expect_match(basename(top_level[[i]]), map_synthetic_fixture_id(id), fixed = TRUE)
  }
  partial <- list.files(
    file.path(public, "partial"), pattern = "detailed\\.xlsx$", full.names = TRUE
  )
  expect_length(partial, 1L)
  expect_identical(
    synchrony_fr_header_metadata(partial)$video_filename,
    paste0("C:\\Users\\Public\\Downloads\\", media$partial)
  )
  expect_setequal(
    metadata_names,
    paste0("C:\\Users\\Public\\Downloads\\", media$matched)
  )
  expect_false(media$unmatched %in% c(filenames, media$partial))
  videos <- list.files(public, pattern = "\\.mp4$")
  expect_setequal(videos, unlist(media, use.names = FALSE))
  for (video in videos) {
    path <- file.path(public, video)
    expect_gt(file.info(path)$size, 0)
    connection <- file(path, "rb")
    signature <- readBin(connection, "raw", n = 12L)
    close(connection)
    expect_identical(rawToChar(signature[5:8]), "ftyp")
  }
  if (nzchar(Sys.which("ffprobe"))) {
    for (video in videos) {
      probe <- system2(
        Sys.which("ffprobe"),
        c("-v", "error", "-show_entries", "format=duration",
          "-of", "default=noprint_wrappers=1:nokey=1",
          shQuote(file.path(public, video))),
        stdout = TRUE
      )
      expect_null(attr(probe, "status"))
      expect_equal(as.numeric(probe), 70)
    }
  }
})

test_that("invented public exports exercise CSV, TXT and XLSX conversion", {
  public <- testthat::test_path("..", "fixtures", "synthetic")
  csv <- loadFRfile(file.path(public, "testdata_detailed.csv"))
  fr10_id <- map_synthetic_fixture_id("8895")
  txt <- loadFRfile(file.path(
    public, "FR10", paste0(fr10_id, "_invented_txt_detailed.txt")
  ))
  xlsx <- loadFRfile(file.path(
    public, "FR10", paste0(fr10_id, "_invented_xlsx_detailed.xlsx")
  ))
  state <- loadFRfile(file.path(
    public,
    "FR9",
    paste0(
      fr10_id,
      " invented FR9 Participant 1_00024 invented_Analysis 2_video_20260101_120000_state.xlsx"
    )
  ))
  expect_gt(nrow(csv), 0L)
  expect_equal(txt[setdiff(names(txt), "subject")], xlsx[setdiff(names(xlsx), "subject")])
  expect_true(all(c("stimulus", "event_marker", "speech_rate") %in% names(txt)))
  expect_true(all(state$id == paste0("#", fr10_id, "_invented-session")))
  expect_true("dominant_expression" %in% names(state))
  expect_equal(
    fr_inherited_fps(loadFRfile(file.path(
      public,
      "FR9",
      paste0(map_synthetic_fixture_id("1218"), "_invented_FR9_16fps_detailed.xlsx")
    ))),
    16
  )
  metadata <- extract_subject_id_metadata(
    file.path(public, "FR10", paste0(fr10_id, "_invented_txt_detailed.txt"))
  )
  expect_identical(metadata$id, paste0("#", fr10_id, "_invented-session"))
  expect_identical(
    metadata$subject,
    paste0(fr10_id, "_invented_txt_detailed")
  )
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
