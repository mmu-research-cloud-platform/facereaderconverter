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
