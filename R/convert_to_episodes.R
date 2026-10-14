#' Convert coding_df to episodes
#'
#' @param coding_df a dataframe or an \code{fr_coding} object from a
#'   FaceReader output. For an \code{fr_coding} object, its \code{$coding}
#'   table is reprocessed using the supplied parameters. id and subject should
#'   be present.
#' @param T_up numeric Upper threshold for entering an episode. Default: 0.2.
#' @param T_down numeric Lower threshold for exiting an episode. Default: 0.1.
#' @param delta numeric Threshold used when computing the returned
#'   \code{delta} column, as the minimum k-step change required. It no longer
#'   controls episode boundaries. Default: 0.1.
#' @param delta_window numeric Time window (in seconds) used to derive
#'   \code{k}, the lag for the returned k-step difference \code{delta}
#'   column. It no longer controls episode boundaries. Default: 0.1.
#' @param min_dur_sec numeric Minimum episode duration (in seconds). Default: 0.1.
#' @param consecutive_missing integer Maximum allowed consecutive missing (NA) frames while in-state before forcing episode end. Default: 150L.
#' @param fps Frames per second (sampling rate of the data); may be
#'   non-integer (e.g. 29.97). A single number applies to all ids; a named
#'   numeric vector or a data frame with columns \code{id} and \code{fps} sets
#'   it per \code{id}. Used only for ids whose rate is not already stored in
#'   metadata (a warning is issued on conflict). If omitted, the rate is
#'   inferred from \code{video_time} (see \code{\link{infer_fps}}), falling back
#'   to 30. Data returned by \code{loadFRfile()} and the \code{convertFR*}
#'   functions carry the frame rate from the file header (as an
#'   \code{fr_metadata} attribute) and this is used before \code{fps}; supply
#'   \code{fps} only when the file has no frame rate. (Combining several files
#'   with e.g. \code{rbind()} drops the attribute; use
#'   \code{convert_directory_to_episodes()} for mixed rates.) A single shared rate is stored in \code{metadata$fps} as a scalar;
#'   mixed rates are stored as a table with columns \code{id} and \code{fps}.
#' @param cores integer Number of threads to use. Default 0 is auto.
#' @return A list with four elements:
#' \describe{
#'   \item{episodes}{data.table of detected episodes with columns \code{start_frame}, \code{end_frame}, \code{n_frames}, \code{duration_s}, \code{id}, \code{subject}, \code{emotion}, \code{start_time}, \code{end_time}, \code{run_id}, and \code{max_value}.}
#'   \item{deltas}{data.table of delta-up reaction events with the same columns as \code{episodes}, except \code{delta_id} replaces \code{run_id}, and with \code{max_delta}, the value range from one delta window before the event start through the event end.}
#'   \item{coding}{Annotated data.table containing the original columns plus \code{id}, \code{subject}, \code{emotion}, \code{value}, \code{delta}, \code{delta_id}, \code{run_id}, \code{status}, and \code{in_state}. \code{status} marks episode boundaries with \code{1L} at the start frame and \code{0L} at the end frame; \code{in_state} is \code{TRUE} for frames inside detected episodes.}
#'   \item{metadata}{Metadata used to create the returned object.}
#' }
#' @details Episode boundaries are determined only by \code{T_up} and
#'   \code{T_down}. The returned \code{delta} column is computed separately and
#'   delta-up rows are also returned in \code{deltas} for downstream functions
#'   such as \code{reaction_rate()}.
#'   The function uses the exported native binding \code{hysteresis_state} if
#'   available; otherwise it will error. It relies on \pkg{data.table} for fast
#'   grouping and joins.
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
  fps = NULL,
  cores = 0L
) {
  if (is_fr_coding(coding_df)) {
    source_obj <- coding_df
    coding_df <- data.table::copy(coding_df$coding)
  } else {
    source_obj <- NULL
  }

  # --- Multithreading ---
  old_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  data.table::setDTthreads(threads = cores)

  original_cols <- names(coding_df)

  is_scalar <- function(x) length(x) == 1 && !is.na(x)
  is_whole <- function(x) {
    is.numeric(x) && is_scalar(x) && abs(x - round(x)) < .Machine$double.eps^0.5
  }

  if (
    is.numeric(fps) &&
      length(fps) == 1L &&
      is.null(names(fps)) &&
      (!is.finite(fps) || fps <= 0)
  ) {
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

  stopifnot(requireNamespace("data.table"))
  dt <- data.table::as.data.table(coding_df)
  is_long <- all(c("emotion", "value") %in% names(dt))
  delta_threshold <- delta

  # fps per id: stored metadata, then `fps`, then inferred from time stamps,
  # then 30. Rounding to whole frames happens only where an integer is needed.
  fps_tbl <- resolve_fr_fps(
    value = fps,
    x = source_obj,
    ids = dt$id,
    data = if ("video_time" %in% names(dt)) dt else NULL,
    inherited = fr_inherited_fps(coding_df)
  )
  lag_k <- function(f) as.integer(round(delta_window * f))
  min_len_for <- function(f) as.integer(ceiling(min_dur_sec * f))

  if ("video_time" %in% names(dt)) {
    duplicate_by <- c("id", "subject", "video_time")
    if (is_long) {
      duplicate_by <- append(duplicate_by, "emotion", after = 2L)
    }
    duplicate_video_time <- dt[, .N, by = duplicate_by][N > 1L]

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
    dt[,
      frame := parse_time_to_frame(
        video_time,
        fps = fps_for_ids(id, fps_tbl)
      )
    ]
  }

  if (!is_long) {
    id_cols <- intersect(c("id", "subject", "video_time", "frame"), names(dt))
    dt <- suppressWarnings(data.table::melt(
      dt,
      id.vars = id_cols,
      variable.name = "emotion",
      value.name = "value",
      variable.factor = FALSE
    ))
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
  dt[, fps_row := fps_for_ids(id, fps_tbl)]
  if ("delta" %in% names(dt)) {
    dt[, delta := NULL]
  }
  dt[,
    delta := all_deltas(value, lag_k(fps_row[1L]), delta_threshold),
    by = .(id, subject, emotion)
  ]

  if (!exists("hysteresis_state", mode = "function")) {
    stop("Cpp not found")
  }

  dt[,
    state := hysteresis_state(
      value,
      lag_k(fps_row[1L]),
      T_up,
      T_down,
      delta_threshold,
      min_len_for(fps_row[1L]),
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
            end_time = last(video_time),
            max_value = max(value, na.rm = TRUE)
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
            end_time = NA,
            max_value = max(value, na.rm = TRUE)
          )
        }
      },
      by = .(id, subject, emotion, state_run)
    ]
  }

  if (nrow(episodes) == 0L) {
    episodes <- data.table::data.table(
      id = coding_df$id[0],
      subject = coding_df$subject[0],
      emotion = character(),
      state_run = integer(),
      start_frame = integer(),
      end_frame = integer(),
      start_time = coding_df$video_time[0],
      end_time = coding_df$video_time[0],
      max_value = numeric()
    )
  }
  data.table::setorder(episodes, id, subject, emotion, start_frame)
  episodes[, run_id := as.integer(.I)]
  episodes[, n_frames := as.integer(end_frame - start_frame + 1L)]
  episodes[, fps_row := fps_for_ids(id, fps_tbl)]
  episodes[, duration_s := n_frames / fps_row]

  if (nrow(episodes) > 0L) {
    episodes <- episodes[n_frames >= min_len_for(fps_row)]
  }
  if (!"max_value" %in% names(episodes)) {
    episodes[, max_value := numeric()]
  }

  dt[, source_row := .I]
  dt[, group_start_row := first(source_row), by = .(id, subject, emotion)]
  dt[,
    c("delta_start_frame", "delta_start_row") := {
      hit <- which(delta == 1L)
      start_frame <- rep(NA_integer_, .N)
      start_row <- rep(NA_integer_, .N)
      if (length(hit) > 0L) {
        for (hit_idx in hit) {
          window <- seq.int(max(1L, hit_idx - lag_k(fps_row[1L])), hit_idx)
          valid <- window[!is.na(value[window])]
          if (length(valid) > 0L) {
            min_idx <- valid[[which.min(value[valid])]]
            start_frame[hit_idx] <- frame[[min_idx]]
            start_row[hit_idx] <- source_row[[min_idx]]
          }
        }
      }
      list(start_frame, start_row)
    },
    by = .(id, subject, emotion)
  ]
  if ("video_time" %in% names(dt)) {
    dt[,
      delta_start_time := video_time[match(delta_start_frame, frame)],
      by = .(id, subject, emotion)
    ]
  }
  dt[, source_row := .I]
  dt[,
    delta_run := data.table::rleid(delta == 1L),
    by = .(id, subject, emotion)
  ]

  if ("video_time" %in% names(dt)) {
    deltas <- dt[
      delta == 1L,
      .(
        start_frame = if (all(is.na(delta_start_frame))) {
          NA_integer_
        } else {
          min(delta_start_frame, na.rm = TRUE)
        },
        end_frame = last(frame),
        start_row = if (all(is.na(delta_start_row))) {
          NA_integer_
        } else {
          min(delta_start_row, na.rm = TRUE)
        },
        end_row = last(source_row),
        group_start_row = first(group_start_row),
        start_time = first(delta_start_time),
        end_time = last(video_time)
      ),
      by = .(id, subject, emotion, delta_run)
    ]
  } else {
    deltas <- dt[
      delta == 1L,
      .(
        start_frame = if (all(is.na(delta_start_frame))) {
          NA_integer_
        } else {
          min(delta_start_frame, na.rm = TRUE)
        },
        end_frame = last(frame),
        start_row = if (all(is.na(delta_start_row))) {
          NA_integer_
        } else {
          min(delta_start_row, na.rm = TRUE)
        },
        end_row = last(source_row),
        group_start_row = first(group_start_row),
        start_time = NA,
        end_time = NA
      ),
      by = .(id, subject, emotion, delta_run)
    ]
  }
  data.table::setorder(deltas, id, subject, emotion, start_frame)
  deltas[, n_frames := as.integer(end_frame - start_frame + 1L)]
  deltas <- deltas[n_frames > 1L]
  deltas[, delta_id := as.integer(.I)]
  deltas[, fps_row := fps_for_ids(id, fps_tbl)]
  deltas[, duration_s := n_frames / fps_row]
  deltas[, max_delta := NA_real_]
  deltas[, k_row := lag_k(fps_row)]
  for (k_value in unique(deltas$k_row)) {
    rows <- which(deltas$k_row == k_value)
    deltas[
      rows,
      max_delta := max_delta_ranges(
        dt$value,
        start_row,
        end_row,
        group_start_row,
        k_value
      )
    ]
  }

  dt[, `:=`(
    status = NA_integer_,
    in_state = FALSE,
    run_id = NA_integer_,
    delta_id = NA_integer_
  )]

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

  if (nrow(deltas) > 0L) {
    dt[
      deltas,
      on = .(id, subject, emotion, delta_run),
      delta_id := i.delta_id
    ]
  }

  dt[,
    intersect(
      c(
        "state",
        "state_run",
        "delta_run",
        "delta_start_frame",
        "delta_start_row",
        "delta_start_time",
        "delta_range_id",
        "source_row",
        "group_start_row"
      ),
      names(dt)
    ) := NULL
  ]
  if ("state_run" %in% names(episodes)) {
    episodes[, state_run := NULL]
  }
  if ("delta_run" %in% names(deltas)) {
    deltas[, delta_run := NULL]
  }
  deltas[, c("start_row", "end_row", "group_start_row") := NULL]
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
    n_frames,
    max_value
  )]

  deltas <- deltas[, .(
    id,
    subject,
    emotion,
    start_frame,
    end_frame,
    start_time,
    end_time,
    duration_s,
    delta_id,
    n_frames,
    max_delta
  )]

  coding_cols <- unique(c(
    intersect(original_cols, names(dt)),
    "id",
    "subject",
    "emotion",
    "value",
    "frame",
    "delta",
    "delta_id",
    "run_id",
    "status",
    "in_state"
  ))
  # A single shared rate stays a scalar (compatible with older objects);
  # mixed rates are stored as a per-id table
  fps_meta <- fps_to_metadata(fps_tbl)
  fr_coding(
    coding = dt[, .SD, .SDcols = coding_cols],
    episodes = episodes,
    deltas = deltas,
    metadata = list(
      schema_version = 1L,
      fps = fps_meta,
      consecutive_missing = consecutive_missing,
      delta = delta_threshold,
      delta_window = delta_window,
      min_dur_sec = min_dur_sec,
      T_down = T_down,
      T_up = T_up,
      cores = as.integer(cores)
    )
  )
}
