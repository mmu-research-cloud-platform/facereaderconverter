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

test_that("synchrony returns pairwise subject columns", {
  result <- synchrony(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0
  )

  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "n_episodes",
      "synchrony"
    ) %in%
      names(result)
  ))
  expect_true(all(
    c("teen", "parent") %in% unique(c(result$denominator, result$numerator))
  ))
  expect_true(all(result$denominator != result$numerator))
  expect_false(any(result$emotion == "neutral"))
  expect_true(all(result$n_episodes >= 0L))
  expect_true(all(result$synchrony >= 0 | is.na(result$synchrony)))
})

test_that("synchrony supports more than two subjects and filtering", {
  multi_subject <- make_multi_subject(test_data_sync)

  all_subjects <- synchrony(multi_subject, missing_threshold = 1)
  filtered <- synchrony(
    multi_subject,
    subject = "subject",
    id = "id",
    missing_threshold = 1
  )

  expect_true(any(all_subjects$denominator == "sibling"))
  expect_true(any(all_subjects$numerator == "sibling"))
  expect_identical(filtered, all_subjects)
})

test_that("synchrony applies the missing-data threshold", {
  relaxed <- synchrony(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0
  )
  strict <- synchrony(
    test_data_sync,
    subject = "subject",
    id = "id",
    missing_threshold = 0.2
  )

  expect_true(
    sum(strict$n_episodes, na.rm = TRUE) < sum(relaxed$n_episodes, na.rm = TRUE)
  )
  expect_true(any(strict$n_episodes < relaxed$n_episodes))
})

test_that("synchrony warns when an id retains only one subject", {
  singleton_case <- make_singleton_id(test_data_sync)

  expect_warning(
    result <- synchrony(
      singleton_case,
      subject = "subject",
      id = "id",
      missing_threshold = 1
    ),
    "fewer than two subjects after filtering"
  )
  expect_true(all(result$id != 3))
})

test_that("synchrony validates its inputs", {
  expect_error(
    synchrony(test_data_sync, subject = 1),
    "`subject` must be a character scalar with no missing values\\."
  )

  expect_error(
    synchrony(test_data_sync, id = 1),
    "`id` must be a character scalar with no missing values\\."
  )

  expect_error(
    synchrony(test_data_sync, subject = "missing_subject"),
    "`coded_data\\$coding` is missing required columns: missing_subject\\."
  )

  expect_error(
    synchrony(test_data_sync, missing_threshold = -0.1),
    "`missing_threshold` must be a numeric scalar in \\[0, 1\\]\\."
  )
})

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
