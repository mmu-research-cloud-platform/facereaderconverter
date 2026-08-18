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
    test_data,
    subject_names = c("teen", "parent"),
    missing_threshold = 1
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
  multi_subject <- make_multi_subject(test_data)

  all_subjects <- synchrony(multi_subject, missing_threshold = 1)
  filtered <- synchrony(
    multi_subject,
    subject_names = c("teen", "parent"),
    missing_threshold = 1
  )

  expect_true(any(all_subjects$denominator == "sibling"))
  expect_true(any(all_subjects$numerator == "sibling"))
  expect_false(any(filtered$denominator == "sibling"))
  expect_false(any(filtered$numerator == "sibling"))
  expect_true(nrow(filtered) < nrow(all_subjects))
})

test_that("synchrony applies the missing-data threshold", {
  relaxed <- synchrony(
    test_data,
    subject_names = c("teen", "parent"),
    missing_threshold = 1
  )
  strict <- synchrony(
    test_data,
    subject_names = c("teen", "parent"),
    missing_threshold = 0.2
  )

  expect_true(
    sum(strict$n_episodes, na.rm = TRUE) < sum(relaxed$n_episodes, na.rm = TRUE)
  )
  expect_true(any(strict$n_episodes < relaxed$n_episodes))
})

test_that("synchrony warns when an id retains only one subject", {
  singleton_case <- make_singleton_id(test_data)

  expect_warning(
    result <- synchrony(singleton_case, missing_threshold = 1),
    "fewer than two subjects after filtering"
  )
  expect_true(all(result$id != 3))
})

test_that("synchrony validates its inputs", {
  expect_error(
    synchrony(test_data, subject_names = 1),
    "`subject_names` must be a character vector with no missing values\\."
  )

  expect_error(
    synchrony(test_data, subject_names = c("teen", "teen")),
    "`subject_names` must not contain duplicates\\."
  )

  expect_error(
    synchrony(test_data, missing_threshold = -0.1),
    "`missing_threshold` must be a numeric scalar in \\[0, 1\\]\\."
  )
})
