library(testthat)

copy_ambiguous_id_fixture <- function(destination) {
  file.copy(
    file.path(
      Sys.getenv("TEST_DATA"),
      "c2e-directory/mum/8892/Participant 10_8892_Analysis 1_video_20260721_125850_detailed.txt"
    ),
    destination
  )
}

test_that("4402 directory ID patterns read media metadata only", {
  root <- tempfile("ambiguous-filename-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_ambiguous_id_fixture(file.path(root, "ID-8892-ID-8895-mum.txt"))

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "#[0-9]{4}",
    subject_pattern = "mum"
  )
  expect_equal(unique(as.character(result$coding$id)), "#8892")
  expect_equal(unique(as.character(result$coding$subject)), "mum")
})

test_that("4403 directory ID regex applies to media filename", {
  root <- tempfile("ambiguous-folder-")
  dir.create(file.path(root, "ID-9999", "mum"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_ambiguous_id_fixture(file.path(root, "ID-9999", "mum", "ID-8895.txt"))

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "#[0-9]{4}",
    subject_pattern = "ID-8895"
  )
  expect_equal(unique(as.character(result$coding$id)), "#8892")
  expect_equal(unique(as.character(result$coding$subject)), "ID-8895")
})

test_that("4404 directory ID regex applies to full media filename", {
  root <- tempfile("full-media-path-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(
    file.path(
      Sys.getenv("TEST_DATA"),
      "brazil",
      "Participant 10_Participant 10_Analysis 1_video_20260918_143140_detailed.xlsx"
    ),
    file.path(
      root,
      "Participant 10_Participant 10_Analysis 1_video_20260918_143140_detailed.xlsx"
    )
  )

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "Downloads[/\\\\]#8883",
    subject_pattern = "Participant 10_Analysis",
    use_full_path = TRUE,
    cores = 1L
  )

  expect_equal(
    unique(as.character(result$coding$id)),
    "Downloads\\#8883"
  )
  expect_equal(
    unique(as.character(result$coding$subject)),
    "Participant 10_Analysis"
  )
})

test_that("4404 repeated fixture IDs remain unambiguous", {
  root <- tempfile("repeated-id-")
  dir.create(file.path(root, "ID-9999", "mum"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  copy_ambiguous_id_fixture(file.path(
    root,
    "ID-9999",
    "mum",
    "ID-8892-ID-8892.txt"
  ))

  result <- convert_directory_to_episodes(
    root,
    id_pattern = "#[0-9]{4}",
    subject_pattern = "ID-8892-ID-8892",
    cores = 1L
  )
  expect_equal(unique(as.character(result$coding$id)), "#8892")
  expect_equal(unique(as.character(result$coding$subject)), "ID-8892-ID-8892")
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
    id_pattern = "#[0-9]{4}",
    subject_pattern = "^(Participant .*_detailed)$",
    cores = 1L
  )
  expect_s3_class(result, "fr_coding")
  expect_identical(result$metadata$fps, 30L)
  groups <- unique(data.frame(
    id = as.character(result$coding$id),
    subject = as.character(result$coding$subject)
  ))
  expect_equal(nrow(groups), 6L)
  expect_gt(nrow(result$episodes), 0L)
  saved <- new.env(parent = emptyenv())
  expect_identical(load(output, envir = saved), "coded_data")
  expect_equal(saved$coded_data, result)
})

test_that("4406 real c2e-fail uses media filename despite folder ID", {
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

  result <- convert_directory_to_episodes(
    root,
    outpath = output,
    id_pattern = "#[0-9]{4}",
    subject_pattern = "Participant",
    cores = 1L
  )
  expect_equal(unique(as.character(result$coding$id)), "#1267")
  expect_equal(file.exists(output), TRUE)
})
