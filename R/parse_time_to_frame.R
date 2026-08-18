#' Parse video time to frame index
#'
#' Convert a time string into a frame index given frames-per-second (fps).
#' Supports "HH:MM:SS", "MM:SS", or plain seconds and is vectorised over `video_time`.
#'
#' @param video_time Character or numeric. Time strings in "HH:MM:SS", "MM:SS", or numeric seconds.
#' @param fps Numeric. Frames per second.
#' @return Integer vector. Frame indices computed as round(seconds * fps).
#' @examples
#' parse_time_to_frame("00:01:23.5", fps = 30)
#' parse_time_to_frame("1:23.5", fps = 30)
#' parse_time_to_frame("83.5", fps = 30)
#' @export
#' @importFrom data.table tstrsplit
# parser: use existing if available, otherwise local

parse_time_to_frame <- function(video_time, fps) {
  video_time_chr <- as.character(video_time)
  sec <- rep(NA_real_, length(video_time_chr))

  has_two_colons <- grepl("^[^:]+:[^:]+:[^:]+$", video_time_chr)
  has_one_colon <- grepl("^[^:]+:[^:]+$", video_time_chr) & !has_two_colons
  has_no_colon <- !grepl(":", video_time_chr, fixed = TRUE)

  if (any(has_two_colons)) {
    parts <- data.table::tstrsplit(video_time_chr[has_two_colons], ":", fixed = TRUE)
    hh <- as.integer(parts[[1L]])
    mm <- as.integer(parts[[2L]])
    ss <- as.numeric(parts[[3L]])
    sec[has_two_colons] <- hh * 3600 + mm * 60 + ss
  }

  if (any(has_one_colon)) {
    parts <- data.table::tstrsplit(video_time_chr[has_one_colon], ":", fixed = TRUE)
    mm <- as.integer(parts[[1L]])
    ss <- as.numeric(parts[[2L]])
    sec[has_one_colon] <- mm * 60 + ss
  }

  if (any(has_no_colon)) {
    sec[has_no_colon] <- as.numeric(video_time_chr[has_no_colon])
  }

  if (any(!(has_two_colons | has_one_colon | has_no_colon))) {
    stop("Unrecognized video_time format.")
  }
  as.integer(round(sec * fps))
}
