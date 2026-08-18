#' Add delta column to coding_df
#'
#' @param coding Data frame with columns: id, subject, emotion, frame (or video_time), value, or an `fr_coding` object
#' @param delta Threshold for both up and down
#' @param delta_window Window size in seconds
#' @param fps Frames per second
#' @param cores integer Number of threads to use. Default 0 is auto.
#' @return coding_df with extra column 'delta' where 1 means up and 0 means down
#' @examples
#' \dontrun{
#' coding_df = read.csv("testdata_detailed.csv")
#' add_delta_column(
#'     coding_df,
#'     delta_window = 0.2,
#'     delta = 0.1,
#'     fps = 30
#'  )
#' }
#'
#' @export
#'
add_delta_column <- function(
  coding,
  delta = 0.1,
  delta_window = 0.2,
  fps = 30L,
  cores = 0L
) {
  # --- Multithreading ---
  old_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  data.table::setDTthreads(threads = cores)

  is_scalar <- function(x) length(x) == 1 && !is.na(x)
  is_whole <- function(x) {
    is.numeric(x) && is_scalar(x) && abs(x - round(x)) < .Machine$double.eps^0.5
  }
  stopifnot(requireNamespace("data.table"))
  if ("fr_coding" %in% class(coding)) {
    coding_df = coding$coding
    fps = coding$metadata$fps
  } else {
    coding_df = coding
  }
  if (!is_whole(fps) || fps <= 0) {
    stop("`fps` must be a positive integer scalar.")
  }
  if (
    !is.numeric(delta_window) || !is_scalar(delta_window) || delta_window < 0
  ) {
    stop("`delta_window` must be a numeric scalar >= 0.")
  }
  if (!is.numeric(delta) || !is_scalar(delta) || delta <= 0) {
    stop("`delta` must be a numeric scalar > 0.")
  }

  if (!"id" %in% names(coding_df)) {
    coding_df <- dplyr::mutate(coding_df, id = 1L)
  }
  if (!"subject" %in% names(coding_df)) {
    coding_df <- dplyr::mutate(coding_df, subject = "unknown")
  }
  if (!all(c("emotion", "value") %in% names(coding_df))) {
    coding_df <- coding_df |>
      pivot_longer(
        cols = -tidyselect::any_of(c("id", "subject", "video_time", "frame")),
        names_to = "emotion",
        values_to = "value"
      )
  }
  requireNamespace("data.table")
  dt <- data.table::as.data.table(coding_df)
  if (!"frame" %in% names(dt)) {
    if (!"video_time" %in% names(dt)) {
      stop("Need either 'frame' or 'video_time' column.")
    }
    dt[, frame := parse_time_to_frame(video_time, fps = fps)]
  }

  has_duplicate_frame <- dt[,
    any(duplicated(frame)),
    by = .(id, subject, emotion)
  ][["V1"]]
  if (any(has_duplicate_frame)) {
    stop(
      "Each `frame` must be unique within each `id`/`subject`/`emotion` group."
    )
  }

  if ("video_time" %in% names(dt)) {
    has_duplicate_video_time <- dt[,
      any(duplicated(video_time)),
      by = .(id, subject, emotion)
    ][["V1"]]
    if (any(has_duplicate_video_time)) {
      stop(
        "Each `video_time` must be unique within each `id`/`subject`/`emotion` group. ",
        "Duplicate timestamps can make delta direction ambiguous."
      )
    }
  }
  k <- as.integer(round(delta_window * fps))
  dt[, delta := all_deltas(value, k, delta), by = .(id, subject, emotion)]
  return(dt)
}
