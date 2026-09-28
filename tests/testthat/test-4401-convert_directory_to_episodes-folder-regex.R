library(testthat)

write_folder_regex_export <- function(path) {
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

test_that("directory episodes match ID and subject independently in folder names", {
  parent <- tempfile("folder-regex-")
  root <- file.path(parent, "study-ID-7711")
  dir.create(file.path(root, "mum"), recursive = TRUE)
  dir.create(file.path(root, "teen"), recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  write_folder_regex_export(file.path(root, "mum", "analysis.txt"))
  write_folder_regex_export(file.path(root, "teen", "analysis.txt"))

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "ID-[0-9]{4}",
    subject_pattern = "mum|teen",
    cores = 1L
  )
  expect_equal(unique(as.character(result$coding$id)), "ID-7711")
  expect_setequal(as.character(unique(result$coding$subject)), c("mum", "teen"))
  expect_gt(nrow(result$episodes), 0L)
  saved <- new.env(parent = emptyenv())
  expect_identical(
    load(file.path(root, "episodes.RDa"), envir = saved),
    "coded_data"
  )
  expect_equal(saved$coded_data, result)
})

test_that("filename wins over folders and nearest folder wins over outer folder", {
  parent <- tempfile("folder-precedence-")
  root <- file.path(parent, "study-ID-7711-teen")
  dir.create(file.path(root, "mum"), recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  write_folder_regex_export(file.path(root, "mum", "ID-7711-teen.txt"))

  filename_first <- convert_directory_to_episodes(
    root,
    id_pattern = "ID-[0-9]{4}",
    subject_pattern = "mum|teen",
    cores = 1L
  )
  expect_equal(unique(as.character(filename_first$coding$id)), "ID-7711")
  expect_equal(unique(as.character(filename_first$coding$subject)), "teen")

  nested_root <- file.path(parent, "second-ID-7711-teen")
  dir.create(file.path(nested_root, "mum"), recursive = TRUE)
  write_folder_regex_export(file.path(nested_root, "mum", "analysis.txt"))
  folder_first <- convert_directory_to_episodes(
    nested_root,
    id_pattern = "ID-[0-9]{4}",
    subject_pattern = "mum|teen",
    cores = 1L
  )
  expect_equal(unique(as.character(folder_first$coding$id)), "ID-7711")
  expect_equal(unique(as.character(folder_first$coding$subject)), "mum")
})

test_that("directory regex does not inspect folders above inpath", {
  parent <- tempfile("outer-ID-8822-")
  root <- file.path(parent, "study")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  write_folder_regex_export(file.path(root, "analysis.txt"))

  expect_match(
    conditionMessage(tryCatch(
      convert_directory_to_episodes(root, id_pattern = "ID-8822"),
      error = identity
    )),
    "ID or subject pattern did not match"
  )
  expect_equal(file.exists(file.path(root, "episodes.RDa")), FALSE)
})
