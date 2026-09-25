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
#' @param id Optional scalar ID or function of `inpath` returning one.
#' @param subject Optional scalar subject or function of `inpath` returning one.
#' @param csv_args Named list of additional arguments for [readr::read_csv()].
#' @param clean_names_args Named list of additional arguments for
#'   [janitor::clean_names()].
#' @param ... Legacy additional arguments passed to `janitor::clean_names()` for
#'   TXT and XLSX imports and to [readr::read_csv()] for CSV imports.
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
  ...,
  id = NULL,
  subject = NULL,
  csv_args = list(),
  clean_names_args = list()
) {
  if (
    !is.list(csv_args) ||
      (length(csv_args) > 0L &&
        (is.null(names(csv_args)) || any(!nzchar(names(csv_args)))))
  ) {
    stop("`csv_args` must be a named list.", call. = FALSE)
  }
  if (
    !is.list(clean_names_args) ||
      (length(clean_names_args) > 0L &&
        (is.null(names(clean_names_args)) ||
          any(!nzchar(names(clean_names_args)))))
  ) {
    stop("`clean_names_args` must be a named list.", call. = FALSE)
  }
  dots <- list(...)
  if (!is.character(inpath) || length(inpath) != 1) {
    stop("`inpath` must be a single string path.")
  }
  if (!file.exists(inpath)) {
    stop("File does not exist: ", inpath)
  }

  ext <- tolower(tools::file_ext(inpath))

  if (ext == "txt") {
    tryCatch(
      do.call(
        convertFRFiles,
        c(
          list(
            inpath = inpath,
            return_data = TRUE,
            values_as_numeric = values_as_numeric,
            clean_names = clean_names,
            fail_codes = fail_codes,
            duplicate_timecodes_as_error = duplicate_timecodes_as_error,
            id = id,
            subject = subject
          ),
          dots,
          clean_names_args
        )
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
      do.call(
        convertFRExcelFiles,
        c(
          list(
            inpath = inpath,
            return_data = TRUE,
            values_as_numeric = values_as_numeric,
            clean_names = clean_names,
            fail_codes = fail_codes,
            duplicate_timecodes_as_error = duplicate_timecodes_as_error,
            sheet = sheet,
            id = id,
            subject = subject
          ),
          dots,
          clean_names_args
        )
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
    df <- do.call(
      readr::read_csv,
      c(list(file = inpath, show_col_types = FALSE), dots, csv_args)
    )
    if (clean_names) {
      df <- do.call(janitor::clean_names, c(list(dat = df), clean_names_args))
    }
    df <- add_fr_metadata(df, resolve_conversion_metadata(id, subject, inpath))
    invisible(df)
  } else {
    stop("Unsupported file extension: .", ext)
  }
}
