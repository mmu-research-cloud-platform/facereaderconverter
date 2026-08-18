#' Convert coding_df to episodes
#'
#' @param coding_df a dataframe or otherwise from a FaceReader output. id and subject should be present.
#' @param T_up numeric Upper threshold for entering an episode. Default: 0.2.
#' @param T_down numeric Lower threshold for exiting an episode. Default: 0.1.
#' @param delta numeric Minimum k-step change required, computed as the current value minus the value \code{k} frames earlier. Default: 0.1.
#' @param delta_window numeric Time window (in seconds) used to derive \code{k}, the lag for the k-step difference rule. Default: 0.1.
#' @param min_dur_sec numeric Minimum episode duration (in seconds). Default: 0.1.
#' @param consecutive_missing integer Maximum allowed consecutive missing (NA) frames while in-state before forcing episode end. Default: 150L.
#' @param fps integer Frames per second (sampling rate of the data). Default: 30L.
#' @return A list with two elements:
#' \describe{
#'   \item{episodes}{data.table of detected episodes with columns \code{start_frame}, \code{end_frame}, \code{n_frames}, \code{duration_s}, \code{id}, \code{subject}, \code{emotion}, and \code{run_id}.}
#'   \item{coding}{Annotated data.table containing the original columns plus \code{id}, \code{subject}, \code{emotion}, \code{value}, \code{run_id}, \code{status}, and \code{in_state}. \code{status} marks episode boundaries with \code{1L} at the start frame and \code{0L} at the end frame; \code{in_state} is \code{TRUE} for frames inside detected episodes.}
#' }
#' @details The function uses the exported native binding \code{hysteresis_state} if available; otherwise it will error.
#' It relies on \pkg{data.table} for fast grouping and joins.
#' @examples
#' \dontrun{
#' coding_df = read.csv("testdata_detailed.csv")
#' convert_to_episodes(coding_df)
#' }
#' @import data.table
#' @importFrom tidyr pivot_longer
#' @export
convert_to_episodes <- function(
  coding_df,
  T_up = 0.20,
  T_down = 0.1,
  delta = 0.10,
  delta_window = 0.1,
  min_dur_sec = 0.1,
  consecutive_missing = 150L,
  fps = 30L
) {
  original_cols <- names(coding_df)

  is_scalar <- function(x) length(x) == 1 && !is.na(x)
  is_whole <- function(x) {
    is.numeric(x) && is_scalar(x) && abs(x - round(x)) < .Machine$double.eps^0.5
  }

  if (!is_whole(fps) || fps <= 0) {
    stop("`fps` must be a positive integer scalar.")
  }
  if (
    !is.numeric(delta_window) || !is_scalar(delta_window) || delta_window < 0
  ) {
    stop("`delta_window` must be a numeric scalar >= 0.")
  }
  if (!is.numeric(T_up) || !is_scalar(T_up) || T_up < 0 || T_up > 1) {
    stop("`T_up` must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(T_down) || !is_scalar(T_down) || T_down < 0 || T_down > 1) {
    stop("`T_down` must be a numeric scalar in [0, 1].")
  }
  if (T_up < T_down) {
    stop("`T_up` must be >= `T_down`.")
  }
  if (!is.numeric(delta) || !is_scalar(delta) || delta <= 0) {
    stop("`delta` must be a numeric scalar > 0.")
  }
  if (!is.numeric(min_dur_sec) || !is_scalar(min_dur_sec) || min_dur_sec <= 0) {
    stop("`min_dur_sec` must be a numeric scalar > 0.")
  }
  if (is.infinite(consecutive_missing)) {
    stop("`consecutive_missing` cannot be infinite.")
  }
  if (!is_whole(consecutive_missing) || consecutive_missing < 0) {
    stop("`consecutive_missing` must be a non-negative integer scalar.")
  }

  if (!"id" %in% names(coding_df)) {
    coding_df <- dplyr::mutate(coding_df, id = 1L)
  }
  if (!"subject" %in% names(coding_df)) {
    coding_df <- dplyr::mutate(coding_df, subject = "unknown")
  }
  if (!"video_time" %in% names(coding_df) && !"frame" %in% names(coding_df)) {
    stop(
      "coding_df must contain either `video_time` or `frame`.",
      call. = FALSE
    )
  }

  if (!all(c("emotion", "value") %in% names(coding_df))) {
    coding_df <- coding_df |>
      tidyr::pivot_longer(
        cols = -tidyselect::any_of(c("id", "subject", "video_time", "frame")),
        names_to = "emotion",
        values_to = "value"
      )
  }

  stopifnot(requireNamespace("data.table"))
  dt <- data.table::as.data.table(coding_df)
  k <- as.integer(round(delta_window * fps))
  min_len <- as.integer(ceiling(min_dur_sec * fps))

  if ("video_time" %in% names(dt)) {
    duplicate_video_time <- dt[,
      .N,
      by = .(id, subject, emotion, video_time)
    ][N > 1L]

    if (nrow(duplicate_video_time) > 0L) {
      stop(
        "Duplicate `video_time` values found within `id`/`subject`/`emotion` groups.",
        call. = FALSE
      )
    }
  }

  if (!"frame" %in% names(dt)) {
    if (!"video_time" %in% names(dt)) {
      stop("Need either 'frame' or 'video_time' column.")
    }
    dt[, frame := parse_time_to_frame(video_time, fps = fps)]
  }

  if ("subject" %in% names(dt) && !is.factor(dt$subject)) {
    dt[, subject := as.factor(subject)]
  }
  if ("emotion" %in% names(dt) && !is.factor(dt$emotion)) {
    dt[, emotion := as.factor(emotion)]
  }
  if (!is.integer(dt$frame)) {
    dt[, frame := as.integer(frame)]
  }

  data.table::setkey(dt, id, subject, emotion, frame)

  if (!exists("hysteresis_state", mode = "function")) {
    stop("Cpp not found")
  }

  dt[,
    state := hysteresis_state(
      value,
      k,
      T_up,
      T_down,
      delta,
      min_len,
      consecutive_missing
    ),
    by = .(id, subject, emotion)
  ]

  dt[, state_run := data.table::rleid(state), by = .(id, subject, emotion)]

  episodes <- dt[
    state == TRUE,
    .(
      start_frame = first(frame),
      end_frame = last(frame),
      n_frames = .N,
      duration_s = .N / fps
    ),
    by = .(id, subject, emotion, state_run)
  ]
  data.table::setorder(episodes, id, subject, emotion, start_frame)
  episodes[, run_id := as.integer(.I)]

  valid_in_state <- dt[
    state == TRUE & !is.na(value),
    .(id, subject, emotion, state_run, frame)
  ]

  if (nrow(valid_in_state) > 0L && nrow(episodes) > 0L) {
    data.table::setkey(valid_in_state, id, subject, emotion, state_run, frame)

    end_map <- valid_in_state[
      episodes,
      on = .(
        id,
        subject,
        emotion,
        state_run,
        frame >= start_frame,
        frame <= end_frame
      ),
      mult = "last",
      .(run_id = i.run_id, end_frame_nonNA = frame),
      by = .EACHI
    ]

    episodes[end_map, on = "run_id", end_frame := i.end_frame_nonNA]
    episodes <- episodes[!is.na(end_frame)]

    n_map <- dt[state == TRUE][
      episodes,
      on = .(
        id,
        subject,
        emotion,
        state_run,
        frame >= start_frame,
        frame <= end_frame
      ),
      .(run_id = i.run_id, n = .N),
      by = .EACHI
    ]

    episodes[n_map, on = "run_id", n := i.n]
    episodes[, duration_s := n / fps]
  }

  episodes[, `:=`(
    n_frames = as.integer(end_frame - start_frame + 1L)
  )]
  episodes[, duration_s := n_frames / fps]

  if (nrow(episodes) > 0L) {
    episodes <- episodes[n_frames >= min_len]
  }
  if ("n" %in% names(episodes)) {
    episodes[, n := NULL]
  }
  if ("state_run" %in% names(episodes)) {
    episodes[, state_run := NULL]
  }

  dt[, `:=`(status = NA_integer_, in_state = FALSE, run_id = NA_integer_)]

  if (nrow(episodes) > 0L) {
    dt[
      episodes,
      on = .(id, subject, emotion, frame = start_frame),
      status := 1L
    ]
    dt[
      episodes,
      on = .(id, subject, emotion, frame = end_frame),
      status := 0L
    ]
    dt[
      episodes,
      on = .(id, subject, emotion, frame >= start_frame, frame <= end_frame),
      `:=`(in_state = TRUE, run_id = i.run_id)
    ]
  }

  dt[in_state == FALSE, run_id := NA_integer_]
  dt[, state_run := NULL]

  coding_cols <- unique(c(
    intersect(original_cols, names(dt)),
    "id",
    "subject",
    "emotion",
    "value",
    "run_id",
    "status",
    "in_state"
  ))

  list(
    episodes = episodes,
    coding = dt[, .SD, .SDcols = coding_cols]
  )
}
