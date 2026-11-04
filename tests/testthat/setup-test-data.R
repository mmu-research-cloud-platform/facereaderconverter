# Select before test files perform their top-level load(). Tests write conversion
# outputs, so stage every fixture in a disposable, writable copy.
local({
  fixture <- select_test_data_fixture()
  source_root <- fixture
  runtime_parent <- testthat::test_path()
  if (.Platform$OS.type == "windows") {
    source_root <- utils::shortPathName(source_root)
    runtime_parent <- utils::shortPathName(runtime_parent)
  }
  runtime <- tempfile(".fixture-work-", tmpdir = runtime_parent)
  if (!dir.create(runtime)) {
    stop("Could not create the disposable fixture directory.")
  }
  cleanup_runtime <- normalizePath(runtime, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type == "windows") {
    cleanup_runtime <- utils::shortPathName(cleanup_runtime)
  }
  withr::defer(
    unlink(cleanup_runtime, recursive = TRUE),
    envir = testthat::teardown_env()
  )
  if (.Platform$OS.type == "windows" && nzchar(Sys.which("robocopy"))) {
    status <- suppressWarnings(system2(
        Sys.which("robocopy"),
        args = c(
          shQuote(source_root),
          shQuote(runtime),
          "/E",
          "/COPY:DAT",
          "/DCOPY:DAT",
          "/R:0",
          "/W:0",
          "/NFL",
          "/NDL",
          "/NJH",
          "/NJS",
          "/NP"
        ),
        stdout = TRUE,
        stderr = TRUE
      )
    )
    if (!is.null(attr(status, "status")) && attr(status, "status") >= 8L) {
      stop("Could not stage the selected test fixtures with robocopy.")
    }
  } else {
    files <- list.files(
      source_root,
      full.names = TRUE,
      recursive = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    files <- files[!dir.exists(files)]
    relative <- substring(files, nchar(source_root) + 2L)
    destinations <- file.path(runtime, relative)
    directories <- unique(dirname(destinations))
    for (directory in directories) {
      dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    }
    copied <- file.copy(files, destinations, overwrite = TRUE)
    if (length(files) && !all(copied)) {
      stop("Could not stage ", sum(!copied), " selected test fixture file(s).")
    }
  }
  fixture <- if (.Platform$OS.type == "windows") {
    utils::shortPathName(runtime)
  } else {
    normalizePath(runtime, winslash = "/", mustWork = TRUE)
  }
  withr::local_envvar(TEST_DATA = fixture, .local_envir = testthat::teardown_env())
})
