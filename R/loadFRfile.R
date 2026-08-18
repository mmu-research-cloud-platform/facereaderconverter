#' Load FaceReader files into memory
#'
#' Reads a FaceReader export file into memory by dispatching to the existing
#' TXT, Excel, or CSV readers based on file extension. The wrapper always
#' returns the parsed data and never writes an output file.
#'
#' @param inpath Path to an existing FaceReader export file.
#' @param values_as_numeric Convert `Video Time` to `hms` and detailed values
#'   to numeric where applicable.
#' @param clean_names Apply `janitor::clean_names()` to the data.
#' @param fail_codes Add a `fail_code` column for detailed exports.
#' @param duplicate_timecodes_as_error Throw an error if duplicate timecodes are
#'   found.
#' @param sheet Excel sheet to read when importing `.xlsx` files.
#' @param ... Additional arguments passed to the underlying importer.
#'
#' @return Invisibly returns the parsed data frame.
#' @examples
#' \dontrun{
#' loadFRfile(
#'   inpath = "FaceReaderOutput.txt",
#'   values_as_numeric = TRUE
#' )
#' }
#'
#' @export
loadFRfile <- function(
  inpath,
  values_as_numeric = TRUE,
  clean_names = TRUE,
  fail_codes = FALSE,
  duplicate_timecodes_as_error = TRUE,
  sheet = 1,
  ...
) {
  if (!is.character(inpath) || length(inpath) != 1) {
    stop("`inpath` must be a single string path.")
  }
  if (!file.exists(inpath)) {
    stop("File does not exist: ", inpath)
  }

  ext <- tolower(tools::file_ext(inpath))

  if (ext == "txt") {
    tryCatch(
      convertFRFiles(
        inpath = inpath,
        return_data = TRUE,
        values_as_numeric = values_as_numeric,
        clean_names = clean_names,
        fail_codes = fail_codes,
        duplicate_timecodes_as_error = duplicate_timecodes_as_error,
        ...
      ),
      error = function(e) {
        if (identical(conditionMessage(e), "FaceReader header row not found")) {
          message("FaceReader header row not found in ", inpath)
          return(NULL)
        }
        stop(e)
      }
    )
  } else if (ext == "xlsx") {
    tryCatch(
      convertFRExcelFiles(
        inpath = inpath,
        return_data = TRUE,
        values_as_numeric = values_as_numeric,
        clean_names = clean_names,
        fail_codes = fail_codes,
        duplicate_timecodes_as_error = duplicate_timecodes_as_error,
        sheet = sheet,
        ...
      ),
      error = function(e) {
        if (identical(conditionMessage(e), "FaceReader header row not found")) {
          message("FaceReader header row not found in ", inpath)
          return(NULL)
        }
        stop(e)
      }
    )
  } else if (ext == "csv") {
    df <- readr::read_csv(inpath, show_col_types = FALSE, ...)
    if (clean_names) {
      df <- janitor::clean_names(df, ...)
    }
    df
  } else {
    stop("Unsupported file extension: .", ext)
  }
}
