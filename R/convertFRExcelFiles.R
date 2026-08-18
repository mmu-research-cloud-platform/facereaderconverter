#' Load FaceReader Excel exports
#'
#' Reads a FaceReader `.xlsx` file, detects the header row automatically,
#' and returns the parsed data frame. The function follows the same
#' post-processing conventions as `convertFRFiles()` where applicable.
#'
#' @param inpath Path to an existing `.xlsx` file.
#' @param return_data Return the parsed data instead of metadata.
#' @param values_as_numeric Convert `Video Time` to `hms` and detailed values to
#'   numeric.
#' @param clean_names Apply `janitor::clean_names()` to the data.
#' @param fail_codes Add a `fail_code` column for detailed exports.
#' @param duplicate_timecodes_as_error Throw an error if duplicate timecodes are
#'   found.
#' @param sheet Excel sheet to read. Defaults to the first sheet.
#' @param ... Additional arguments passed to `janitor::clean_names()`.
#'
#' @return Invisibly returns the parsed data when `return_data = TRUE`;
#'   otherwise returns metadata about the import.
#' @examples
#' \dontrun{
#' convertFRExcelFiles(
#'   inpath = "FaceReaderOutput.xlsx",
#'   return_data = TRUE,
#'   values_as_numeric = TRUE
#' )
#' }
#'
#' @export
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr case_when mutate count pull across
#' @importFrom stringr str_trim
#' @importFrom janitor clean_names
#' @importFrom hms as_hms
#' @importFrom tidyselect any_of
convertFRExcelFiles <- function(
  inpath,
  return_data = TRUE,
  values_as_numeric = TRUE,
  clean_names = TRUE,
  fail_codes = FALSE,
  duplicate_timecodes_as_error = TRUE,
  sheet = 1,
  ...
) {
  if (!is.character(inpath) || length(inpath) != 1) {
    stop("`inpath` must be a single string to a .xlsx file.")
  }
  if (!file.exists(inpath)) {
    stop("File does not exist: ", inpath)
  }
  ext <- tolower(tools::file_ext(inpath))
  if (ext != "xlsx") {
    stop("Input file must have a .xlsx extension.")
  }
  if (!is.numeric(sheet) || length(sheet) != 1L || is.na(sheet) || sheet < 1L) {
    stop("`sheet` must be a positive numeric scalar.")
  }

  sheets <- readxl::excel_sheets(inpath)
  sheet <- as.integer(sheet)
  if (sheet > length(sheets)) {
    stop("`sheet` is out of range for the workbook.")
  }

  md <- readxl::read_excel(
    inpath,
    sheet = sheet,
    n_max = 200,
    col_names = FALSE
  )
  md_vals <- as.data.frame(md, stringsAsFactors = FALSE)
  header_row <- detect_fr_header_row(unlist(md_vals[1], use.names = FALSE))

  md_type <- dplyr::case_when(
    grepl("detailed", md_vals[[1]][1], ignore.case = TRUE) ~ "detailed",
    grepl("state", md_vals[[1]][1], ignore.case = TRUE) ~ "state",
    TRUE ~ "other"
  )

  if (!grepl("video analysis", md_vals[[1]][1], ignore.case = TRUE)) {
    stop("FaceReader metadata missing")
  }

  md_videoname <- md_vals[[2]][6] |> stringr::str_trim()
  md_time <- md_vals[[2]][5] |>
    stringr::str_trim() |>
    as.POSIXct(format = "%m/%d/%Y %H:%M:%OS")

  df <- readxl::read_excel(inpath, sheet = sheet, skip = header_row - 1)
  df <- df |>
    dplyr::select(
      tidyselect::any_of(c(
        "Video Time",
        "Dominant Expression",
        "Neutral",
        "Happy",
        "Sad",
        "Angry",
        "Surprised",
        "Scared",
        "Disgusted"
      ))
    )

  n_vals <- df |> dplyr::count(`Video Time`) |> dplyr::pull(n)
  timecount <- if (length(n_vals) == 0) 0L else max(n_vals)
  if (timecount > 1 && duplicate_timecodes_as_error) {
    stop("Duplicate timecodes")
  } else if (timecount > 1) {
    warning("Duplicate timecodes")
  }

  if (md_type == "detailed" && fail_codes) {
    df <- df |>
      dplyr::mutate(
        fail_code = dplyr::case_when(
          Neutral == "FIT_FAILED" ~ 1,
          Neutral == "FIND_FAILED" ~ 2,
          .default = 0
        )
      )
  }

  if (values_as_numeric) {
    df <- df |>
      dplyr::mutate(`Video Time` = hms::as_hms(`Video Time`))

    if (md_type == "detailed") {
      df <- df |>
        dplyr::mutate(
          dplyr::across(
            -tidyselect::any_of(c("Video Time")),
            ~ suppressWarnings(as.numeric(.))
          )
        )
    }
  }

  if (clean_names) {
    df <- janitor::clean_names(df, ...)
  }

  if (return_data) {
    invisible(df)
  } else {
    metadata <- data.frame(
      video_filename = md_videoname,
      time = md_time,
      type = md_type,
      inpath = inpath,
      outpath = inpath,
      stringsAsFactors = FALSE
    )
    invisible(metadata)
  }
}
