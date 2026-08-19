TEST_DATA <- Sys.getenv("TEST_DATA")
load(file.path(TEST_DATA, "test_data.RDa"))

skip_if_not_installed("openxlsx")

test_that("convertFRFiles", {
  remove_csv_in_dir <- function(dir, recursive = FALSE, dry_run = TRUE) {
    csvs <- list.files(
      dir,
      pattern = "\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = recursive
    )
    if (length(csvs) == 0) {
      invisible(tibble::tibble(file = character(), removed = logical()))
    }
    if (dry_run) {
      tibble::tibble(file = csvs, removed = NA)
    } else {
      removed <- file.remove(csvs)
      invisible(tibble::tibble(file = csvs, removed = removed))
    }
  }
  remove_csv_in_dir("testdata", recursive = TRUE, dry_run = FALSE)
  remove_csv_in_dir("junk", recursive = TRUE, dry_run = FALSE)
  expect_false(file.exists("junk/testdata_detailed.csv"))
  expect_false(file.exists("junk/testdata_state.csv"))
  expect_false(file.exists("junk/testdata2/testdata_state2.csv"))

  expect_no_error(convertFRFiles("testdata/testdata_detailed.txt"))
  expect_true(file.exists("testdata/testdata_detailed.csv"))
  expect_no_error(convertFRFiles("testdata/testdata_state.txt"))
  expect_true(file.exists("testdata/testdata_state.csv"))

  remove_csv_in_dir("testdata", recursive = TRUE, dry_run = FALSE)
  expect_no_error(convertFRFiles(
    "testdata/testdata_detailed.txt",
    values_as_numeric = TRUE
  ))

  x <- read.csv("testdata/testdata_detailed.csv")
  expect_equal(class(x$neutral), "numeric")
  expect_no_error(convertFRFiles(
    "testdata/testdata_detailed.txt",
    clean_names = TRUE
  ))

  x1 <- x |> mutate(time_length = stringr::str_length(video_time))
  expect_equal(max(x1$time_length, na.rm = TRUE), 12)

  x <- read.csv("testdata/testdata_detailed.csv")
  expect_all_true(names(x) == names(janitor::clean_names(x)))

  expect_no_error(convertFRFiles(
    "testdata/testdata_detailed.txt",
    case = "all_caps"
  ))
  x <- read.csv("testdata/testdata_detailed.csv")
  expect_all_true(names(x) == names(janitor::clean_names(x, case = "all_caps")))

  x <- convertFRFiles("testdata/testdata_detailed.txt")
  expect_true(ncol(x) == 5)
  expect_true(grepl("csv", x$outpath))
  x <- convertFRFiles("testdata/testdata_state.txt")
  expect_true(ncol(x) == 5)

  x <- convertFRFiles(
    "testdata/testdata_detailed.txt",
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  )
  expect_true(nrow(x) > 1000)

  y <- convertFRFiles(
    "testdata/testdata_extracols_detailed.txt",
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE,
    duplicate_timecodes_as_error = FALSE
  )
  expect_true(all(c("participant_name", "analysis_index") %in% names(y)))

  expect_no_error(convertFRFiles(
    "testdata/testdata_detailed.txt",
    outpath = "junk/testdata_detailed.csv",
    clean_names = TRUE,
    values_as_numeric = TRUE
  ))
  expect_true(file.exists("junk/testdata_detailed.csv"))
  y <- read.csv("junk/testdata_detailed.csv") |>
    mutate(video_time = as_hms(video_time))
  expect_true(all.equal(x |> as.data.frame(), y))

  z <- convertFRFiles(
    "testdata/testdata_detailed_line_change.txt",
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  )
  expect_equal(z, x, tolerance = 1e-4)

  expect_error(
    convertFRFiles("testdata/testdata_detailed_fake.txt"),
    "File does not exist: testdata/testdata_detailed_fake.txt"
  )
  expect_error(
    convertFRFiles("testdata/testdata_detailed_fail.txt"),
    "FaceReader header row not found"
  )

  expect_error(
    convertFRFiles("testdata/testdata_detailed.csv"),
    "Input file must have a .txt extension."
  )

  expect_error(
    convertFRFiles("testdata/testdata_detailed_duplicate_timecode.txt"),
    "Duplicate timecodes"
  )

  expect_error(
    convertFRFiles(
      "testdata/testdata_detailed_duplicate_timecode.txt",
      duplicate_timecodes_as_error = TRUE
    ),
    "Duplicate timecodes"
  )

  expect_warning(
    convertFRFiles(
      "testdata/testdata_detailed_duplicate_timecode.txt",
      duplicate_timecodes_as_error = FALSE
    ),
    "Duplicate timecodes"
  )

  tmp_txt_ok <- tempfile(fileext = ".txt")
  tmp_txt_bad <- tempfile(fileext = ".txt")
  txt_ok <- c(
    "Video analysis detailed log",
    "",
    "Face Model\tGeneral",
    "Calibration\t-",
    "Start time\t6/4/2026 13:31:06.331",
    "Filename\tC:/tmp/example.mp4",
    "Frame rate\t30.000000000",
    "",
    "",
    "",
    paste(
      c(
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
      collapse = "\t"
    ),
    paste(
      c("00:00:00.000", rep("0", 7), "Rebecca", "Analysis 5"),
      collapse = "\t"
    ),
    paste(c("00:00:00.000", rep("0", 7), "Alex", "Analysis 6"), collapse = "\t")
  )
  txt_bad <- txt_ok
  txt_bad[length(txt_bad)] <- paste(
    c("00:00:00.000", rep("0", 7), "Rebecca", "Analysis 5"),
    collapse = "\t"
  )
  writeLines(txt_ok, tmp_txt_ok)
  writeLines(txt_bad, tmp_txt_bad)
  expect_no_error(convertFRFiles(
    tmp_txt_ok,
    return_data = TRUE,
    clean_names = TRUE,
    values_as_numeric = TRUE
  ))
  expect_error(
    convertFRFiles(
      tmp_txt_bad,
      return_data = TRUE,
      clean_names = TRUE,
      values_as_numeric = TRUE
    ),
    "Duplicate timecodes"
  )

  x <- convertFRFiles(
    "testdata/testdata_detailed.txt",
    fail_codes = TRUE
  )
  y <- read.csv("testdata/testdata_detailed.csv")
  expect_true(ncol(y) == 9)
  expect_equal(
    y |> count(fail_code) |> pull(fail_code),
    c(0, 1, 2)
  )
})
