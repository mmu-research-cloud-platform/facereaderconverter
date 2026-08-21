#' Convert FaceReader time strings to seconds
#'
#' Parses time values formatted as `hh:mm:ss` or `hh:mm:ss.mmm` and returns the
#' equivalent number of seconds.
#'
#' @param x Character vector of time strings.
#'
#' @return A numeric vector of seconds.
#'
#' @examples
#' to_seconds(c("00:00:10", "00:00:10.500"))
#' @export
to_seconds <- \(x) {
  stringr::str_match(x, "^(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$") |>
    as_tibble(.name_repair = ~ c("match", "hh", "mm", "ss", "ms")) |>
    mutate(across(-match, as.numeric), ms = tidyr::replace_na(ms, 0)) |>
    dplyr::transmute(time_sec = hh * 3600 + mm * 60 + ss + ms / 1000) |>
    pull(time_sec)
}
