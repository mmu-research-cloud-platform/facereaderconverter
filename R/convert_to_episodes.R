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
#' @param cores integer Number of threads to use. Default 0 is auto.
#' @return A list with two elements:
#' \describe{
#'   \item{episodes}{data.table of detected episodes with columns \code{start_frame}, \code{end_frame}, \code{n_frames}, \code{duration_s}, \code{id}, \code{subject}, \code{emotion},\code{start_time}, \code{end_time}, and \code{run_id}.}
#'   \item{coding}{Annotated data.table containing the original columns plus \code{id}, \code{subject}, \code{emotion}, \code{value}, \code{run_id}, \code{status}, and \code{in_state}. \code{status} marks episode boundaries with \code{1L} at the start frame and \code{0L} at the end frame; \code{in_state} is \code{TRUE} for frames inside detected episodes.}
#'   \item{fps}{Frames per second (sampling rate of the data).}
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
#' @importFrom hms as_hms
#' @export
convert_to_episodes <- function(
  coding_df,
  T_up = 0.20,
  T_down = 0.1,
  delta = 0.10,
  delta_window = 0.2,
  min_dur_sec = 0.1,
  consecutive_missing = 150L,
  fps = 30L,
  cores = 0L
) {
  # --- Multithreading ---
  old_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  data.table::setDTthreads(threads = cores)

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
  if (
    !is.numeric(delta_window) || !is_scalar(delta_window) || delta_window <= 0
  ) {
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
    stop("`id` column required.", call. = FALSE)
  }
  if (!"subject" %in% names(coding_df)) {
    stop("`subject` column required.", call. = FALSE)
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
      duplicate_groups <- duplicate_video_time[, unique(sprintf(
        "id=%s, subject=%s",
        as.character(id),
        as.character(subject)
      ))]
      stop(
        paste0(
          "Duplicate `video_time` values found within `id`/`subject` groups: ",
          paste(duplicate_groups, collapse = "; ")
        ),
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

  if ("video_time" %in% names(dt)) {
    episodes <- dt[
      state == TRUE,
      {
        non_missing <- which(!is.na(value))
        if (length(non_missing) == 0L) {
          NULL
        } else {
          end_idx <- non_missing[[length(non_missing)]]
          .(
            start_frame = first(frame),
            end_frame = frame[[end_idx]],
            start_time = first(video_time),
            end_time = last(video_time)
          )
        }
      },
      by = .(id, subject, emotion, state_run)
    ]
  } else {
    episodes <- dt[
      state == TRUE,
      {
        non_missing <- which(!is.na(value))
        if (length(non_missing) == 0L) {
          NULL
        } else {
          end_idx <- non_missing[[length(non_missing)]]
          .(
            start_frame = first(frame),
            end_frame = frame[[end_idx]],
            start_time = NA,
            end_time = NA
          )
        }
      },
      by = .(id, subject, emotion, state_run)
    ]
  }

  data.table::setorder(episodes, id, subject, emotion, start_frame)
  episodes[, run_id := as.integer(.I)]
  episodes[, n_frames := as.integer(end_frame - start_frame + 1L)]
  episodes[, duration_s := n_frames / fps]

  if (nrow(episodes) > 0L) {
    episodes <- episodes[n_frames >= min_len]
  }

  dt[, `:=`(status = NA_integer_, in_state = FALSE, run_id = NA_integer_)]

  if (nrow(episodes) > 0L) {
    dt[
      episodes,
      on = .(id, subject, emotion, state_run),
      `:=`(
        run_id = i.run_id,
        start_frame_episode = i.start_frame,
        end_frame_episode = i.end_frame
      )
    ]

    dt[
      state == TRUE & frame >= start_frame_episode & frame <= end_frame_episode,
      in_state := TRUE
    ]
    dt[state == TRUE & frame == start_frame_episode, status := 1L]
    dt[state == TRUE & frame == end_frame_episode, status := 0L]
    dt[in_state == FALSE, run_id := NA_integer_]
    dt[, c("start_frame_episode", "end_frame_episode") := NULL]
  }

  dt[, c("state", "state_run") := NULL]
  if ("state_run" %in% names(episodes)) {
    episodes[, state_run := NULL]
  }
  episodes <- episodes[, .(
    id,
    subject,
    emotion,
    start_frame,
    end_frame,
    start_time,
    end_time,
    duration_s,
    run_id,
    n_frames
  )]

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
  structure(
    list(
      episodes = episodes,
      coding = dt[, .SD, .SDcols = coding_cols],
      metadata = list(
        fps = fps,
        consecutive_missing = consecutive_missing,
        delta = delta,
        delta_window = delta_window,
        min_dur_sec = min_dur_sec,
        T_down = T_down,
        T_up = T_up
      )
    ),
    class = c("fr_coding", "list")
  )
}
