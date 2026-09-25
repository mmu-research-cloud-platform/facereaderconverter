#' Extract subject and ID metadata from a FaceReader filename
#'
#' Extracts an identifier and subject label from a file path using caller-supplied
#' regular expressions. The identifier is trimmed after extraction, while the
#' subject is matched case-insensitively.
#'
#' @param path Path to a FaceReader file.
#' @param id_pattern Regular expression used to extract the identifier. Defaults
#'   to a four-digit identifier with optional surrounding whitespace.
#' @param subject_pattern Regular expression used to extract the subject. Defaults
#'   to `mum` or `teen` as a standalone word.
#'
#' @return A list with character elements `id` and `subject`. Each element is
#'   `NA` when its pattern does not match.
#' @examples
#' \dontrun{
#' extract_subject_id_metadata(
#'   "8895 mum FR9_00024 mum_Analysis_detailed.xlsx",
#'   id_pattern = "(?<![0-9])\\s*[0-9]{4}\\s*(?![0-9])",
#'   subject_pattern = "(?<![a-z])(mum|teen)(?![a-z])"
#' )
#' }
#' @export
extract_subject_id_metadata <- function(
  path,
  id_pattern = "(?<![0-9])\\s*[0-9]{4}\\s*(?![0-9])",
  subject_pattern = "(?<![a-z])(mum|teen)(?![a-z])"
) {
  filename <- basename(path)
  id <- stringr::str_extract(filename, id_pattern) |>
    stringr::str_trim()
  subject <- stringr::str_extract(
    filename,
    stringr::regex(subject_pattern, ignore_case = TRUE)
  ) |>
    tolower()

  list(id = id, subject = subject)
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
  id_pattern = "(?<![0-9])\\s*[0-9]{4}\\s*(?![0-9])",
  subject_pattern = "(?<![a-z])(mum|teen)(?![a-z])"
) {
  extract_subject_id_metadata(path, id_pattern, subject_pattern)
}
