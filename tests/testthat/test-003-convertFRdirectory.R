test_that("convertFRDirectory", {
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

  expect_no_error(convertFRDirectory("testdata", cores = 2L))
  expect_true(file.exists("testdata/metadata.csv"))

  expect_no_error(convertFRDirectory(
    "testdata",
    metadata_filename = "metadata2.csv"
  ))
  expect_true(file.exists("testdata/metadata2.csv"))

  x <- read.csv("testdata/metadata.csv")
  x <- dplyr::filter(
    x,
    status == "Success",
    !grepl("metadata", basename(outpath))
  )
  expect_true(all(grepl("csv", x$outpath, fixed = TRUE)))

  expect_true(file.exists("testdata/testdata_detailed.csv"))
  expect_true(file.exists("testdata/testdata_state.csv"))
  expect_true(file.exists("testdata/testdata2/testdata_state2.csv"))

  expect_no_error(convertFRDirectory(
    "testdata",
    values_as_numeric = TRUE,
    cores = 2L
  ))
  expect_no_error(convertFRDirectory(
    "testdata",
    "junk",
    values_as_numeric = TRUE,
    cores = 2L
  ))

  expect_true(file.exists("junk/testdata_detailed.csv"))
  expect_true(file.exists("junk/testdata_state.csv"))
  expect_true(file.exists("junk/testdata2/testdata_state2.csv"))

  x <- read.csv("junk/testdata_detailed.csv")
  expect_equal(class(x$neutral), "numeric")
  expect_no_error(convertFRDirectory(
    "testdata",
    clean_names = TRUE
  ))
  x <- read.csv("testdata/testdata_detailed.csv")
  expect_all_true(names(x) == names(janitor::clean_names(x)))

  expect_no_error(convertFRDirectory(
    "testdata",
    clean_names = TRUE,
    case = "all_caps"
  ))
  x <- read.csv("testdata/testdata_detailed.csv")
  expect_all_true(
    names(x) == janitor::make_clean_names(names(x), case = "all_caps")
  )

  x <- convertFRDirectory("testdata")
  expect_true(nrow(x) == 8)
  expect_true(sum(x$status == "Fail") == 2)

  x <- convertFRDirectory("testdata", pattern = "state", cores = 2L)
  expect_true(nrow(x) == 3)

  x <- convertFRDirectory(
    "testdata",
    duplicate_timecodes_as_error = FALSE,
    cores = 2L
  )

  expect_true(sum(x$status == "Fail") == 1)
  expect_true(sum(x$status == "Success" & !is.na(x$error)) == 1)
})
