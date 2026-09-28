library(testthat)

write_ambiguous_id_export <- function(path) {
  writeLines(
    c(
      "Video analysis detailed log",
      "",
      "Face Model\tGeneral",
      "Calibration\t-",
      "Start time\t6/4/2026 13:31:06.331",
      "Filename\tsession.mp4",
      "Frame rate\t30",
      "",
      "Video Time\tNeutral\tHappy",
      sprintf("00:00:00.%03d\t0\t0.6", seq(0L, 363L, by = 33L))
    ),
    path
  )
}

test_that("4402 directory rejects distinct IDs within one filename", {
  root <- tempfile("ambiguous-filename-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_ambiguous_id_export(file.path(root, "ID-1234-ID-5678-mum.txt"))

  error <- tryCatch(
    convert_directory_to_episodes(root, id_pattern = "ID-[0-9]{4}"),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Multiple different IDs match")
  expect_match(conditionMessage(error), "ID-1234, ID-5678", fixed = TRUE)
  expect_equal(file.exists(file.path(root, "episodes.RDa")), FALSE)
})

test_that("4403 directory rejects distinct IDs between filename and folder", {
  root <- tempfile("ambiguous-folder-")
  dir.create(file.path(root, "ID-1234", "mum"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_ambiguous_id_export(file.path(root, "ID-1234", "mum", "ID-5678.txt"))

  error <- tryCatch(
    convert_directory_to_episodes(
      root,
      id_pattern = "ID-[0-9]{4}",
      subject_pattern = "mum"
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Multiple different IDs match")
  expect_equal(file.exists(file.path(root, "episodes.RDa")), FALSE)
})

test_that("4404 repeated identical IDs remain unambiguous", {
  root <- tempfile("repeated-id-")
  dir.create(file.path(root, "ID-1234", "mum"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  write_ambiguous_id_export(file.path(
    root,
    "ID-1234",
    "mum",
    "ID-1234-ID-1234.txt"
  ))

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "ID-[0-9]{4}",
    subject_pattern = "mum",
    cores = 1L
  )
  expect_equal(unique(as.character(result$coding$id)), "ID-1234")
  expect_equal(unique(as.character(result$coding$subject)), "mum")
  expect_equal(file.exists(file.path(root, "episodes.RDa")), TRUE)
})

test_that("4405 real c2e-directory exports produce one combined result", {
  root <- file.path(Sys.getenv("TEST_DATA"), "c2e-directory")
  expect_equal(dir.exists(root), TRUE)
  detailed <- list.files(
    root,
    recursive = TRUE,
    pattern = "_detailed\\.txt$",
    full.names = TRUE
  )
  expect_equal(length(detailed), 6L)
  output <- tempfile(fileext = ".RDa")
  on.exit(unlink(output), add = TRUE)

  result <- convert_directory_to_episodes(
    root,
    outpath = output,
    id_pattern = "(?<![0-9])[0-9]{4}(?![0-9])",
    subject_pattern = "mum|teen",
    cores = 1L
  )
  expect_s3_class(result, "fr_coding")
  expect_identical(result$metadata$fps, 30L)
  groups <- unique(data.frame(
    id = as.character(result$coding$id),
    subject = as.character(result$coding$subject)
  ))
  expect_setequal(
    paste(groups$id, groups$subject),
    c("8892 mum", "8894 mum", "8948 mum", "8951 mum", "8954 mum", "8895 teen")
  )
  expect_gt(nrow(result$episodes), 0L)
  saved <- new.env(parent = emptyenv())
  expect_identical(load(output, envir = saved), "coded_data")
  expect_equal(saved$coded_data, result)
})

test_that("4406 real c2e-fail exports reject filename-folder ID conflict", {
  root <- file.path(Sys.getenv("TEST_DATA"), "c2e-fail")
  expect_equal(dir.exists(root), TRUE)
  detailed <- list.files(
    root,
    recursive = TRUE,
    pattern = "_detailed\\.txt$",
    full.names = TRUE
  )
  expect_equal(length(detailed), 1L)
  output <- tempfile(fileext = ".RDa")
  on.exit(unlink(output), add = TRUE)

  error <- tryCatch(
    convert_directory_to_episodes(
      root,
      outpath = output,
      id_pattern = "(?<![0-9])[0-9]{4}(?![0-9])",
      subject_pattern = "mum|teen",
      cores = 1L
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Multiple different IDs match")
  expect_match(conditionMessage(error), "8895, 8894", fixed = TRUE)
  expect_equal(file.exists(output), FALSE)
})
