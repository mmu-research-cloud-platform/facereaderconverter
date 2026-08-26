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
