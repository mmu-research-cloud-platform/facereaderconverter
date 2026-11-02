load_test_data_fixture <- function() {
  fixture <- file.path(Sys.getenv("TEST_DATA"), "test_data.RDa")
  if (!file.exists(fixture)) {
    testthat::skip("The TEST_DATA test fixture is not available.")
  }

  load(fixture, envir = parent.frame())
}
