TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

# Regenerate time stamps for one id at a given rate
retime <- function(fps, n = 600L, id = 1) {
  secs <- (seq_len(n) - 1L) / fps
  data.frame(
    id = id,
    subject = "parent",
    video_time = sprintf(
      "%02d:%02d:%06.3f",
      secs %/% 3600,
      (secs %% 3600) %/% 60,
      secs %% 60
    )
  )
}

test_that("infer_fps recovers the fixture's 30 fps for every id", {
  res <- infer_fps(test_coding_wide)
  expect_setequal(res$id, as.character(unique(test_coding_wide$id)))
  expect_true(all(res$fps == 30))
  expect_true(all(res$snapped))
  expect_true(all(abs(res$fps_raw - 30) < 0.1))
})

test_that("infer_fps works on fr_coding input and restricts to ids", {
  res <- infer_fps(test_data_sync2, ids = c(1, 3))
  expect_setequal(res$id, c("1", "3"))
  expect_true(all(res$fps == 30))
})

test_that("infer_fps recovers a non-integer rate from regenerated stamps", {
  res <- infer_fps(retime(29.97))
  expect_equal(res$fps, 29.97)
  expect_true(res$snapped)
})

test_that("infer_fps returns unsnapped estimates for unusual rates", {
  res <- infer_fps(retime(12))
  expect_equal(res$fps, 12, tolerance = 0.01)
  expect_false(res$snapped)
})

test_that("infer_fps warns for too few stamps, gaps and subject disagreement", {
  expect_warning(
    res <- infer_fps(retime(30, n = 5L)),
    "too few distinct time stamps"
  )
  expect_true(is.na(res$fps))

  gappy <- retime(30)[-seq(10, 590, by = 5), ]
  expect_warning(infer_fps(gappy), "gaps")

  mixed <- rbind(retime(30), transform(retime(25), subject = "child"))
  expect_warning(infer_fps(mixed), "Subjects disagree")
})

test_that("infer_fps validates input", {
  expect_error(infer_fps(data.frame(id = 1)), "must have columns")
})

test_that("resolve_fr_fps infers only for ids without metadata or argument", {
  x <- test_data_sync2
  x$metadata$fps <- c("1" = 25)
  expect_message(
    resolve_fr_fps(x = x, ids = c(1, 3), data = test_coding_wide),
    "inferred"
  )
  res <- suppressMessages(
    resolve_fr_fps(x = x, ids = c(1, 3), data = test_coding_wide)
  )
  expect_equal(res$fps, c(25, 30))
  expect_equal(res$fps_source, c("metadata", "inferred"))
})

test_that("resolve_fr_fps records the source and ranks inference above default", {
  res <- resolve_fr_fps(30, ids = 1)
  expect_equal(res$fps_source, "argument")

  res <- resolve_fr_fps(ids = 1)
  expect_equal(res$fps_source, "default")

  res <- suppressMessages(
    resolve_fr_fps(ids = 1, data = retime(25, id = 1))
  )
  expect_equal(res$fps, 25)
  expect_equal(res$fps_source, "inferred")

  # Argument wins over inference
  res <- resolve_fr_fps(60, ids = 1, data = retime(25, id = 1))
  expect_equal(res$fps, 60)
})

test_that("resolve_fr_fps falls back to the default when inference fails", {
  res <- resolve_fr_fps(ids = 1, data = retime(30, n = 5L))
  expect_equal(res$fps, 30)
  expect_equal(res$fps_source, "default")
})
