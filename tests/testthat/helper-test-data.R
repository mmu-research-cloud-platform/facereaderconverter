select_test_data_fixture <- function(
  configured = Sys.getenv("TEST_DATA"),
  mode = Sys.getenv("FACEREADER_TEST_FIXTURE", "auto"),
  synthetic = testthat::test_path("..", "fixtures", "synthetic")
) {
  if (!mode %in% c("auto", "real", "synthetic")) {
    stop("FACEREADER_TEST_FIXTURE must be auto, real, or synthetic.")
  }
  real_available <- nzchar(configured) &&
    file.exists(file.path(configured, "test_data.RDa"))
  if (mode == "real" && !real_available) {
    stop("Real fixtures requested but TEST_DATA/test_data.RDa is unavailable.")
  }
  if (mode != "synthetic" && real_available) {
    return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  }
  if (!file.exists(file.path(synthetic, "test_data.RDa"))) {
    stop("The bundled synthetic fixture is missing; regenerate it with tools/generate-test-fixtures.R.")
  }
  normalizePath(synthetic, winslash = "/", mustWork = TRUE)
}

test_fixture_path <- function(role, index = 1L) {
  root <- Sys.getenv("TEST_DATA")
  candidates <- switch(
    role,
    detailed_csv = file.path(root, "testdata_detailed.csv"),
    brazil_detailed_xlsx = list.files(
      file.path(root, "brazil"),
      pattern = "_detailed\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ),
    c2e_detailed_txt = list.files(
      file.path(root, "c2e-directory"),
      pattern = "_detailed\\.txt$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ),
    c2e_failure_txt = list.files(
      file.path(root, "c2e-fail"),
      pattern = "_detailed\\.txt$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ),
    fr9_detailed_xlsx = list.files(
      file.path(root, "FR9"),
      pattern = "_detailed\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ),
    fr9_16fps_detailed_xlsx = {
      files <- list.files(
        file.path(root, "FR9"),
        pattern = "_detailed\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
      files[vapply(
        files,
        function(path) {
          isTRUE(all.equal(
            synchrony_fr_header_metadata(path)$fps,
            16,
            tolerance = 1e-4
          ))
        },
        logical(1)
      )]
    },
    fr9_state_xlsx = list.files(
      file.path(root, "FR9"),
      pattern = "_state\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ),
    fr10_detailed_xlsx = list.files(
      file.path(root, "FR10"),
      pattern = "_detailed\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ),
    stop("Unknown test fixture role: ", role, call. = FALSE)
  )

  if (length(candidates) < index || index < 1L) {
    stop("The selected fixture role is unavailable: ", role, call. = FALSE)
  }
  candidates <- sort(candidates)
  candidates[[index]]
}

load_test_data_fixture <- function() {
  fixture <- file.path(Sys.getenv("TEST_DATA"), "test_data.RDa")
  if (!file.exists(fixture)) {
    testthat::skip("The TEST_DATA test fixture is not available.")
  }

  load(fixture, envir = parent.frame())
}
