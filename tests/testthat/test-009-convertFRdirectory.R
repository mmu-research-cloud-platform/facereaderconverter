test_that("convertFRDirectory", {
  make_fr_txt <- function(
    path,
    kind = c("detailed", "state"),
    participant = "Rebecca",
    analysis = "Analysis 5",
    video_time = "00:00:00.000",
    duplicate_timecode = FALSE,
    include_metadata = TRUE
  ) {
    kind <- match.arg(kind)

    header <- switch(
      kind,
      detailed = c(
        "Video Time",
        "Neutral",
        "Happy",
        "Sad",
        "Angry",
        "Surprised",
        "Scared",
        "Disgusted",
        "Participant Name",
        "Analysis Index"
      ),
      state = c(
        "Video Time",
        "Dominant Expression",
        "Participant Name",
        "Analysis Index"
      )
    )

    data_rows <- switch(
      kind,
      detailed = c(
        paste(
          c(video_time, rep("0", 7), participant, analysis),
          collapse = "\t"
        ),
        if (duplicate_timecode) {
          paste(
            c(video_time, rep("0", 7), participant, analysis),
            collapse = "\t"
          )
        } else {
          paste(
            c("00:00:00.033", rep("0", 7), "Alex", "Analysis 6"),
            collapse = "\t"
          )
        }
      ),
      state = c(
        paste(c(video_time, "Happy", participant, analysis), collapse = "\t"),
        if (duplicate_timecode) {
          paste(c(video_time, "Happy", participant, analysis), collapse = "\t")
        } else {
          paste(
            c("00:00:00.033", "Neutral", "Alex", "Analysis 6"),
            collapse = "\t"
          )
        }
      )
    )

    lines <- c(
      if (include_metadata) {
        c(
          "Video analysis detailed log",
          "",
          "Face Model\tGeneral",
          "Calibration\t-",
          "Start time\t6/4/2026 13:31:06.331",
          "Filename\tC:/tmp/example.mp4",
          "Frame rate\t30.000000000",
          "",
          "",
          ""
        )
      },
      paste(header, collapse = "\t"),
      data_rows
    )

    writeLines(lines, path)
  }

  base <- tempfile("frdir_")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(base, "testdata2"))
  dir.create(file.path(base, "junk"))

  make_fr_txt(file.path(base, "testdata_detailed.txt"), kind = "detailed")
  make_fr_txt(file.path(base, "testdata_state.txt"), kind = "state")
  make_fr_txt(
    file.path(base, "testdata_extracols_detailed.txt"),
    kind = "detailed"
  )
  make_fr_txt(file.path(base, "testdata_extracols_state.txt"), kind = "state")
  make_fr_txt(
    file.path(base, "testdata_fail.txt"),
    kind = "detailed",
    include_metadata = FALSE
  )
  make_fr_txt(
    file.path(base, "testdata_duplicate_timecode.txt"),
    kind = "detailed",
    duplicate_timecode = TRUE
  )
  make_fr_txt(
    file.path(base, "testdata2", "testdata_state2.txt"),
    kind = "state"
  )

  expect_no_error(convertFRDirectory(base, cores = 2L))
  expect_true(file.exists(file.path(base, "metadata.csv")))

  expect_no_error(convertFRDirectory(
    base,
    metadata_filename = "metadata2.csv"
  ))
  expect_true(file.exists(file.path(base, "metadata2.csv")))

  x_extra <- convertFRDirectory(base, pattern = "extracols", cores = 2L)
  expect_true(any(grepl("extracols_detailed", x_extra$inpath, fixed = TRUE)))
  expect_true(any(grepl("extracols_state", x_extra$inpath, fixed = TRUE)))
  expect_true(any(x_extra$status == "Success"))

  x <- read.csv(file.path(base, "metadata.csv"))
  x <- dplyr::filter(
    x,
    status == "Success",
    !grepl("metadata", basename(outpath))
  )

  expect_true(file.exists(file.path(base, "testdata_detailed.csv")))
  expect_true(file.exists(file.path(base, "testdata_state.csv")))
  expect_true(file.exists(file.path(base, "testdata2", "testdata_state2.csv")))

  expect_no_error(convertFRDirectory(
    base,
    values_as_numeric = TRUE,
    cores = 2L
  ))
  expect_no_error(convertFRDirectory(
    base,
    file.path(base, "junk"),
    values_as_numeric = TRUE,
    cores = 2L
  ))

  expect_true(file.exists(file.path(base, "junk", "testdata_detailed.csv")))
  expect_true(file.exists(file.path(base, "junk", "testdata_state.csv")))
  expect_true(file.exists(file.path(
    base,
    "junk",
    "testdata2",
    "testdata_state2.csv"
  )))

  x <- read.csv(file.path(base, "junk", "testdata_detailed.csv"))
  expect_true(is.numeric(x$neutral))

  expect_no_error(convertFRDirectory(
    base,
    clean_names = TRUE
  ))
  x <- read.csv(file.path(base, "testdata_detailed.csv"))
  expect_all_true(names(x) == names(janitor::clean_names(x)))

  expect_no_error(convertFRDirectory(
    base,
    clean_names = TRUE,
    case = "all_caps"
  ))
  x <- read.csv(file.path(base, "testdata_detailed.csv"))
  expect_all_true(
    names(x) == janitor::make_clean_names(names(x), case = "all_caps")
  )

  x <- convertFRDirectory(base)
  expect_true(nrow(x) == 7)
  expect_true(sum(x$status == "Fail") == 2)
  expect_true(any(grepl(
    "FaceReader metadata missing",
    x$error,
    fixed = TRUE
  )))
  expect_true(any(grepl("Duplicate timecodes", x$error, fixed = TRUE)))

  x <- convertFRDirectory(base, pattern = "state", cores = 2L)
  expect_true(nrow(x) == 3)

  x <- convertFRDirectory(
    base,
    duplicate_timecodes_as_error = FALSE,
    cores = 2L
  )

  expect_true(sum(x$status == "Fail") == 1)
  expect_true(x$error[x$status == "Fail"] == "FaceReader metadata missing")
  expect_true(sum(x$status == "Success" & !is.na(x$error)) == 0)
})
