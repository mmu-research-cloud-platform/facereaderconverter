#' Extract subject and ID metadata from a FaceReader filename
#'
#' Extracts the media filename from a FaceReader export's metadata for `id` and
#' the FaceReader export filename for `subject`. Both values are basenames
#' without extensions unless an optional regular expression is supplied.
#'
#' @param path Path to a FaceReader file.
#' @param id_pattern Optional regular expression applied to the associated media
#'   filename. Defaults to the complete media basename without extension.
#' @param subject_pattern Optional regular expression applied to the FaceReader
#'   export basename. Defaults to the complete basename without extension.
#' @param use_full_path If `TRUE`, supplied patterns search the media filename
#'   as stored in the export for `id` and the full export path for `subject`.
#'   Default values remain basenames without extensions.
#'
#' @return A list with character elements `id` and `subject`. Each element is
#'   `NA` when its pattern does not match.
#' @examples
#' \dontrun{
#' extract_subject_id_metadata("path/to/face_reader_export_detailed.txt")
#' }
#' @export
extract_subject_id_metadata <- function(
  path,
  id_pattern = NULL,
  subject_pattern = NULL,
  use_full_path = FALSE
) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be one non-missing file path.", call. = FALSE)
  }
  if (
    !is.logical(use_full_path) ||
      length(use_full_path) != 1L ||
      is.na(use_full_path)
  ) {
    stop("`use_full_path` must be TRUE or FALSE.", call. = FALSE)
  }
  media_filename <- if (!file.exists(path)) {
    NA_character_
  } else {
    switch(
      tolower(tools::file_ext(path)),
      txt = synchrony_fr_txt_filename(readr::read_lines(path, n_max = 200L)),
      xlsx = synchrony_fr_xlsx_filename(as.data.frame(
        suppressMessages(
          readxl::read_excel(path, n_max = 200L, col_names = FALSE)
        ),
        stringsAsFactors = FALSE
      )),
      NA_character_
    )
  }
  id_source <- fr_media_id(media_filename)
  subject_source <- fr_filename_stem(path)
  if (use_full_path && !is.null(id_pattern)) {
    id_source <- media_filename
  }
  if (use_full_path && !is.null(subject_pattern)) {
    subject_source <- path
  }
  extract_one <- function(source, pattern) {
    if (is.null(source) || is.na(source) || !nzchar(source)) {
      return(NA_character_)
    }
    if (is.null(pattern)) {
      return(source)
    }
    match <- stringr::str_extract(source, pattern)
    if (is.na(match)) NA_character_ else stringr::str_trim(match)
  }

  list(
    id = extract_one(id_source, id_pattern),
    subject = extract_one(subject_source, subject_pattern)
  )
}

#' Extract ID and subject metadata from a FaceReader filename
#'
#' Compatibility alias for `extract_subject_id_metadata()`.
#'
#' @inheritParams extract_subject_id_metadata
#' @inherit extract_subject_id_metadata return
#' @examples
#' \dontrun{
#' extract_id_subject_metadata("FR9 1218 mum_Analysis_detailed.xlsx")
#' }
#' @export
extract_id_subject_metadata <- function(
  path,
  id_pattern = NULL,
  subject_pattern = NULL,
  use_full_path = FALSE
) {
  extract_subject_id_metadata(path, id_pattern, subject_pattern, use_full_path)
}
