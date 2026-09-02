#' Convert FaceReader time strings to seconds
#'
#' Parses time values formatted as `hh:mm:ss` or `hh:mm:ss.mmm` and returns the
#' equivalent number of seconds.
#'
#' @param x Character vector of time strings.
#' @param digits Number of fractional digits required after the decimal point.
#'   Set to `0` to allow whole-second timestamps without a decimal part. Default
#'   is `3`.
#'
#' @return A numeric vector of seconds.
#'
#' @examples
#' \dontrun{
#' to_seconds(c("00:00:10.000", "00:00:10.500"))
#' }
#' @export
to_seconds <- \(x, digits = 3L) {
  if (
    !is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0
  ) {
    stop("`digits` must be a non-negative integer scalar.")
  }
  digits <- as.integer(digits)
  if (digits == 0L) {
    stringr::str_match(x, "^(\\d+):(\\d+):(\\d+)$") |>
      as_tibble(.name_repair = ~ c("match", "hh", "mm", "ss")) |>
      mutate(across(-match, as.numeric)) |>
      dplyr::transmute(time_sec = hh * 3600 + mm * 60 + ss) |>
      pull(time_sec)
  } else {
    pattern <- sprintf("^(\\d+):(\\d+):(\\d+)\\.(\\d{%d})$", digits)
    stringr::str_match(x, pattern) |>
      as_tibble(.name_repair = ~ c("match", "hh", "mm", "ss", "ms")) |>
      mutate(across(-match, as.numeric), ms = tidyr::replace_na(ms, 0)) |>
      dplyr::transmute(time_sec = hh * 3600 + mm * 60 + ss + ms / (10 ^ digits)) |>
      pull(time_sec)
  }
}
