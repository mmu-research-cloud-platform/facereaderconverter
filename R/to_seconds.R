#' Convert FaceReader time strings to seconds
#'
#' Parses time values formatted as `hh:mm:ss` or `hh:mm:ss.s` and returns the
#' equivalent number of seconds. Fractional seconds are truncated or padded to
#' the number of digits specified by `digits`.
#'
#' @param x Character vector of time strings.
#' @param digits Number of fractional digits to retain after the decimal point.
#'   Set to `0` to discard fractional seconds. Default is `3`.
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
  matches <- stringr::str_match(
    x,
    "^(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$"
  )
  fractional <- matches[, 5L]

  if (digits == 0L) {
    fractional <- rep("0", length(x))
  } else {
    fractional <- ifelse(is.na(fractional), "0", fractional)
    fractional <- stringr::str_pad(
      stringr::str_sub(fractional, 1L, digits),
      width = digits,
      side = "right",
      pad = "0"
    )
  }

  tibble::tibble(
    hh = matches[, 2L],
    mm = matches[, 3L],
    ss = matches[, 4L],
    fractional = fractional
  ) |>
    mutate(
      across(everything(), as.numeric)
    ) |>
    dplyr::transmute(
      time_sec = hh * 3600 + mm * 60 + ss + fractional / (10^digits)
    ) |>
    pull(time_sec)
}
