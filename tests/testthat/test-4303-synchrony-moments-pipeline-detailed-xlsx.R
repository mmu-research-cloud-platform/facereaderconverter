TEST_DATA <- Sys.getenv("TEST_DATA")

library(testthat)

skip_if_not_installed("openxlsx")
skip_if_not_installed("readxl")

write_pipeline_detailed_xlsx <- function(path, video, participant) {
  metadata <- data.frame(
    V1 = c(
      "Video analysis detailed log",
      "",
      "Face Model",
      "Calibration",
      "Start time",
      "Filename",
      "Frame rate"
    ),
    V2 = c(
      "",
      "",
      "General",
      "-",
      "6/4/2026 13:31:06.331",
      video,
      "30.000000000"
    )
  )
  header <- data.frame(
    `Video Time` = "Video Time",
    Neutral = "Neutral",
    Happy = "Happy",
    `Participant Name` = "Participant Name",
    check.names = FALSE
  )
  values <- data.frame(
    `Video Time` = c("00:00:00.000", "00:00:00.033"),
    Neutral = c("0", "0"),
    Happy = c("0.5", "0.6"),
    `Participant Name` = participant,
    check.names = FALSE
  )
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "Sheet1")
  openxlsx::writeData(workbook, "Sheet1", metadata, colNames = FALSE)
  openxlsx::writeData(
    workbook,
    "Sheet1",
    header,
    startRow = 11,
    colNames = FALSE
  )
  openxlsx::writeData(
    workbook,
    "Sheet1",
    values,
    startRow = 12,
    colNames = FALSE
  )
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
}

test_that("synchrony_moments_pipeline matches detailed XLSX files and skips copies", {
  root <- tempfile("synchrony-pipeline-xlsx-")
  dir.create(file.path(root, "videos"), recursive = TRUE)
  dir.create(file.path(root, "exports", "nested"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  video_names <- c("Session-A.MP4", "session-b.mp4")
  file.create(file.path(root, "videos", video_names))
  copy_video <- file.path(root, "videos", "Session-A - Copy.MP4")
  file.create(copy_video)
  for (video_name in video_names) {
    for (participant in c("parent", "teen")) {
      write_pipeline_detailed_xlsx(
        file.path(
          root,
          "exports",
          "nested",
          paste(video_name, participant, "detailed.xlsx", sep = "-")
        ),
        paste0("C:\\FaceReader\\source\\", tolower(video_name)),
        participant
      )
    }
  }

  result <- NULL
  expect_message(
    result <- synchrony_moments_pipeline(root),
    "Skipped videos without matching detailed FaceReader exports:\\n.*Session-A - Copy.MP4"
  )

  expect_setequal(result$videos$video_filename, video_names)
  expect_equal(nrow(result$videos), 2L)
  expect_true(all(vapply(
    seq_len(nrow(result$videos)),
    function(i) {
      data.table::uniqueN(unlist(result$videos[
        i,
        .(participant1, participant2)
      ])) ==
        2L
    },
    logical(1)
  )))
  expect_named(result$results, c("video_001", "video_002"))
  expect_false(basename(copy_video) %in% result$videos$video_filename)
  expect_true(all(vapply(
    result$results,
    function(video) nrow(video$clip_manifest) == 0L,
    logical(1)
  )))
})

test_that("synchrony_moments_pipeline processes two matched Brazil XLSX video pairs", {
  brazil_dir <- file.path(TEST_DATA, "brazil")
  skip_if_not(dir.exists(brazil_dir))
  detailed_files <- list.files(
    brazil_dir,
    pattern = "detailed\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(
    length(detailed_files) != 4L,
    "Brazil fixture must contain two matched detailed XLSX pairs."
  )

  result <- NULL
  expect_message(
    result <- synchrony_moments_pipeline(
      brazil_dir,
      output_dir = tempfile("brazil-xlsx-clips-"),
      subject_from_filename = TRUE,
      emotion = "not_an_emotion"
    ),
    "Skipped videos without matching detailed FaceReader exports:\\n.*ID100024_side_by_side - Copy\\.mp4"
  )

  expect_equal(nrow(result$videos), 2L)
  expect_length(result$results, 2L)
  expect_true(all(vapply(
    result$results,
    function(video) nrow(video$clip_manifest) == 0L,
    logical(1)
  )))
  expect_false(
    "ID100024_side_by_side - Copy.mp4" %in% result$videos$video_filename
  )
})

test_that("synchrony_moments_pipeline filters videos before matching XLSX exports", {
  root <- tempfile("synchrony-pipeline-xlsx-filter-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "keep.mp4"), file.path(root, "drop.mp4"))
  for (participant in c("parent", "teen")) {
    write_pipeline_detailed_xlsx(
      file.path(root, paste0("keep-", participant, "-detailed.xlsx")),
      "C:/FaceReader/keep.mp4",
      participant
    )
  }

  result <- synchrony_moments_pipeline(root, video_pattern = "^keep\\.mp4$")

  expect_equal(result$videos$video_filename, "keep.mp4")
  expect_length(result$results, 1L)
})

test_that("synchrony_moments_pipeline matches video and metadata base names", {
  root <- tempfile("synchrony-pipeline-xlsx-extension-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(root, "recording.mov"))
  for (participant in c("parent", "teen")) {
    write_pipeline_detailed_xlsx(
      file.path(root, paste0("recording-", participant, "-detailed.xlsx")),
      "C:/FaceReader/recording.mp4",
      participant
    )
  }

  result <- synchrony_moments_pipeline(root)

  expect_equal(result$videos$video_filename, "recording.mov")
  expect_length(result$results, 1L)
})
