TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

library(testthat)
library(data.table)

make_multi_subject <- function(x) {
  out <- copy(x)
  sibling_coding <- out$coding[id == 1]
  sibling_coding[, subject := "sibling"]

  sibling_episodes <- out$episodes[id == 1]
  sibling_episodes[, subject := "sibling"]

  out$coding <- rbind(out$coding, sibling_coding, fill = TRUE)
  out$episodes <- rbind(out$episodes, sibling_episodes, fill = TRUE)
  out
}

make_singleton_id <- function(x) {
  out <- copy(x)
  out$coding <- out$coding[!(id == 3 & subject == "parent")]
  out
}

sort_coded_data <- function(x) {
  out <- copy(x)
  data.table::setorder(out$coding, id, subject, emotion, video_time, run_id)
  data.table::setorder(
    out$episodes,
    id,
    subject,
    emotion,
    start_frame,
    end_frame,
    run_id
  )
  out
}

test_that("episodes are not dropped when missing_threshold = 0", {
  result <- synchrony(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0
  ) |>
    dplyr::select(id, denominator, emotion, n_episodes) |>
    dplyr::arrange(id, denominator) |>
    as.data.frame()

  control <- test_data_sync$episodes |>
    dplyr::filter_out(emotion == "neutral") |>
    dplyr::group_by(id, subject, emotion) |>
    dplyr::summarise(n_episodes = dplyr::n(), .groups = "drop") |>
    dplyr::rename(denominator = subject) |>
    dplyr::mutate(
      denominator = as.character(denominator),
      emotion = as.character(emotion)
    ) |>
    dplyr::arrange(id, denominator, emotion) |>
    as.data.frame()
  expect_equal(result, control, ignore_attr = TRUE)

  result2 <- synchrony(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0.5
  ) |>
    dplyr::select(id, denominator, emotion, n_episodes) |>
    dplyr::arrange(id, denominator) |>
    as.data.frame()

  test_frame <- merge(
    result,
    result2,
    by = c("id", "denominator", "emotion"),
    suffixes = c("_0", "_05")
  ) |>
    dplyr::mutate(
      n_episodes_0 = as.integer(n_episodes_0),
      n_episodes_05 = as.integer(n_episodes_05)
    ) |>
    dplyr::filter(n_episodes_0 < n_episodes_05) |>
    dplyr::arrange(id, denominator, emotion) |>
    as.data.frame()

  expect_true(nrow(test_frame) == 0)
})
