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
