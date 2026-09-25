TEST_DATA <- Sys.getenv("TEST_DATA")

test_data_path <- file.path(TEST_DATA, "test_data.RDa")
test_data_error <- tryCatch(
  {
    load(test_data_path)
    NULL
  },
  error = identity
)
if (inherits(test_data_error, "error")) {
  testthat::skip(sprintf(
    "Could not load test data fixtures: %s",
    test_data_error$message
  ))
}

test_that("convertFRDirectory processes fixtures with Linux workers", {
  skip_if(Sys.info()[["sysname"]] != "Linux", "Linux/WSL only")

  input_dir <- tempfile("fr_linux_workers_input_")
  output_dir <- tempfile("fr_linux_workers_output_")
  dir.create(input_dir)
  on.exit(
    unlink(c(input_dir, output_dir), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  fr10_dir <- file.path(TEST_DATA, "FR10")
  source_files <- list.files(
    fr10_dir,
    pattern = "\\.txt$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if(length(source_files) < 2L, "Two FR10 TXT fixtures are required")
  file.copy(source_files[1:2], input_dir)

  par_lapply_lb <- parallel::parLapplyLB
  par_lapply_calls <- 0L
  par_lapply_workers <- NA_integer_
  par_lapply_succeeded <- FALSE
  testthat::local_mocked_bindings(
    parLapplyLB = function(cl, ...) {
      par_lapply_calls <<- par_lapply_calls + 1L
      par_lapply_workers <<- length(cl)
      result <- par_lapply_lb(cl, ...)
      par_lapply_succeeded <<- TRUE
      result
    },
    .package = "parallel"
  )

  result <- convertFRDirectory(
    input_dir,
    output_dir,
    cores = 2L,
    save_metadata = NULL
  )

  expect_true(par_lapply_succeeded)
  expect_identical(par_lapply_calls, 1L)
  expect_identical(par_lapply_workers, 2L)
  expect_true(all(result$status == "Success"))
  expect_length(result$outpath, 2L)
  expect_true(all(file.exists(result$outpath)))
})
