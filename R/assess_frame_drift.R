#' Assess drift between frame indices and video time stamps
#'
#' For each `id`, compares the time implied by each `frame` index
#' (`frame / fps`) with the parsed `video_time` in seconds. Frame indices are
#' integers, so a drift of up to half a frame is expected when `frame` was
#' derived by rounding; larger drift suggests an incorrect fps, dropped frames
#' or irregular time stamps.
#'
#' @param x An `fr_coding` object, or a data frame with columns `id`,
#'   `video_time` and `frame`.
#' @param fps Optional fps used for ids without stored metadata: a single
#'   number, a named numeric vector, or a data frame with columns `id` and
#'   `fps`. Stored metadata takes precedence (with a warning on conflict);
#'   the default is 30.
#' @return A data.table with one row per `id`: `fps`, `n` (distinct time
#'   stamps), `max_abs_drift_s`, `mean_abs_drift_s` (seconds) and
#'   `max_abs_drift_frames`. A warning is issued if any id drifts by more than
#'   half a frame.
#' @examples
#' \dontrun{
#' assess_frame_drift(converted, fps = 30)
#' assess_frame_drift(coding_df, fps = c("1" = 29.97, "2" = 30))
#' }
#' @export
assess_frame_drift <- function(x, fps = NULL) {
  dt <- if (is_fr_coding(x)) {
    # `coding` carries no frame index; `deltas` does
    if ("frame" %in% names(x$coding)) x$coding else x$deltas
  } else {
    x
  }
  if (
    !is.data.frame(dt) || !all(c("id", "video_time", "frame") %in% names(dt))
  ) {
    stop(
      "`x` must have columns `id`, `video_time` and `frame`.",
      call. = FALSE
    )
  }
  dt <- unique(data.table::as.data.table(dt)[, c("id", "video_time", "frame")])
  dt <- dt[!is.na(dt$video_time) & !is.na(dt$frame)]

  fps_tbl <- resolve_fr_fps(fps, x = x, ids = dt$id)
  row_fps <- fps_for_ids(dt$id, fps_tbl)

  seconds <- parse_time_to_seconds(dt$video_time)
  drift_s <- abs(dt$frame / row_fps - seconds)
  work <- data.table::data.table(
    id = as.character(dt$id),
    drift_s = drift_s,
    drift_frames = drift_s * row_fps
  )
  out <- work[,
    list(
      n = .N,
      max_abs_drift_s = max(drift_s),
      mean_abs_drift_s = mean(drift_s),
      max_abs_drift_frames = max(drift_frames)
    ),
    by = "id"
  ]
  out <- merge(fps_tbl, out, by = "id", sort = FALSE)
  data.table::setcolorder(out, c("id", "fps", "n"))

  flagged <- out$max_abs_drift_frames > 0.5 + 1e-8
  if (any(flagged)) {
    warning(
      "Drift exceeds half a frame for id(s): ",
      paste(utils::head(out$id[flagged], 5L), collapse = ", "),
      call. = FALSE
    )
  }
  out[]
}
