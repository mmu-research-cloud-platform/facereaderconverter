#' Calculate synchrony for matched negative-control intervals
#'
#' Samples control intervals with the same inclusive frame length as supplied
#' episodes, then calculates synchrony for the sampled intervals. The `episodes`
#' argument can be the output of [synchrony_by_episode()] or a compatible episode
#' table. Control intervals cannot overlap supplied episodes or previously
#' accepted controls for the same ID.
#'
#' @param coded_data Output from [convert_to_episodes()].
#' @param episodes A data frame containing episode ranges. It must contain the
#'   selected ID column, either the selected subject column or `denominator`, and
#'   `emotion`, `run_id`, `start_frame`, and `end_frame`. Frame bounds are
#'   inclusive.
#' @param mutually_exclusive Logical scalar retained for compatibility. Controls
#'   cannot overlap supplied episodes for the same ID regardless of this value.
#' @param subject Character scalar giving the subject column. Defaults to
#'   `"subject"`.
#' @param id Character scalar giving the ID column. Defaults to `"id"`.
#' @inheritParams synchrony
#' @details When `coded_data` contains metadata, its `metadata$fps` value is
#' used by the synchrony calculations, including when `fps` is supplied.
#' @param max_tries Maximum number of random control starts attempted for each
#'   episode. Defaults to `1000`.
#'
#' @return A data.table containing the usual [synchrony_by_episode()] columns
#'   for matched controls, plus source and control identifiers, frame bounds,
#'   attempt count, and matching status. Unmatched source episodes have missing
#'   synchrony fields and `control_status = "unmatched"`.
#' @examples
#' \dontrun{
#' coded_data <- convert_to_episodes(coding_data)
#' episodes <- synchrony_by_episode(coded_data)
#' negative_controls(coded_data, episodes)
#' }
#' @export
negative_controls <- function(
  coded_data,
  episodes,
  mutually_exclusive = FALSE,
  subject = "subject",
  id = "id",
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  fps = 30L,
  missing_threshold = 0,
  exclude_emotions = "neutral",
  max_tries = 1000L
) {
  if (
    !is.logical(mutually_exclusive) ||
      length(mutually_exclusive) != 1L ||
      is.na(mutually_exclusive)
  ) {
    stop(
      "`mutually_exclusive` must be a non-missing logical scalar.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(max_tries) ||
      length(max_tries) != 1L ||
      is.na(max_tries) ||
      max_tries < 1L ||
      abs(max_tries - round(max_tries)) > .Machine$double.eps^0.5
  ) {
    stop("`max_tries` must be a positive integer scalar.", call. = FALSE)
  }

  inputs <- prepare_synchrony_inputs(
    coded_data = coded_data,
    episodes = episodes,
    subject = subject,
    id = id,
    time_limit = time_limit,
    time_limit_frames = time_limit_frames,
    constraint_method = constraint_method,
    fps = fps,
    missing_threshold = missing_threshold,
    exclude_emotions = exclude_emotions
  )
  source_episodes <- unique(inputs$episodes[, .(
    id,
    subject,
    emotion,
    source_run_id = run_id,
    start_frame,
    end_frame
  )])
  source_episodes[, source_row_id := .I]
  frame_limits <- inputs$coding[,
    .(
      first_frame = min(frame, na.rm = TRUE),
      last_frame = max(frame, na.rm = TRUE)
    ),
    by = .(id, subject)
  ]
  source_episodes <- frame_limits[source_episodes, on = .(id, subject)]
  source_episodes[, `:=`(
    control_start_frame = as.integer(NA),
    control_end_frame = as.integer(NA),
    control_tries_used = as.integer(max_tries),
    control_status = "unmatched"
  )]

  known_ranges <- unique(inputs$known_episodes[, .(id, start_frame, end_frame)])
  used_ranges <- data.table::data.table(
    id = source_episodes$id[0],
    start_frame = integer(),
    end_frame = integer()
  )
  for (row in seq_len(nrow(source_episodes))) {
    source <- source_episodes[row]
    duration <- source$end_frame - source$start_frame + 1L
    latest_start <- source$last_frame - duration + 1L
    if (
      !is.finite(source$first_frame) ||
        !is.finite(latest_start) ||
        latest_start < source$first_frame
    ) {
      next
    }
    starts <- seq.int(source$first_frame, latest_start)
    for (attempt in seq_len(max_tries)) {
      control_start <- sample(starts, size = 1L)
      control_end <- control_start + duration - 1L
      overlaps <- known_ranges[
        id == source$id &
          start_frame <= control_end &
          end_frame >= control_start
      ]
      if (nrow(overlaps) > 0L) {
        next
      }
      reused_frames <- used_ranges[
        id == source$id &
          start_frame <= control_end &
          end_frame >= control_start
      ]
      if (nrow(reused_frames) > 0L) {
        next
      }
      source_episodes[
        row,
        `:=`(
          control_start_frame = control_start,
          control_end_frame = control_end,
          control_tries_used = attempt,
          control_status = "matched"
        )
      ]
      used_ranges <- data.table::rbindlist(
        list(
          used_ranges,
          data.table::data.table(
            id = source$id,
            start_frame = control_start,
            end_frame = control_end
          )
        ),
        use.names = TRUE
      )
      break
    }
  }

  matched <- source_episodes[control_status == "matched"]
  if (nrow(matched) > 0L) {
    matched[, control_run_id := source_row_id]
    control_episodes <- matched[, .(
      id,
      subject,
      emotion,
      run_id = control_run_id,
      start_frame = control_start_frame,
      end_frame = control_end_frame
    )]
    control_synchrony <- synchrony_by_episode(
      coded_data = coded_data,
      episodes = control_episodes,
      subject = subject,
      id = id,
      time_limit = time_limit,
      time_limit_frames = time_limit_frames,
      constraint_method = constraint_method,
      fps = fps,
      missing_threshold = missing_threshold,
      exclude_emotions = exclude_emotions
    )
    out <- merge(
      control_synchrony,
      matched[, .(
        id,
        run_id = control_run_id,
        source_run_id,
        control_start_frame,
        control_end_frame,
        control_tries_used,
        control_status
      )],
      by = c("id", "run_id"),
      all.x = TRUE,
      sort = FALSE
    )
    data.table::setnames(out, "run_id", "control_run_id")
  } else {
    out <- data.table::data.table()
  }

  unmatched <- source_episodes[
    control_status == "unmatched",
    .(
      id,
      denominator = subject,
      numerator = NA_character_,
      emotion,
      control_run_id = source_row_id,
      start_frame = as.integer(NA),
      end_frame = as.integer(NA),
      present_prop = NA_real_,
      synchrony = NA,
      source_run_id,
      control_start_frame,
      control_end_frame,
      control_tries_used,
      control_status
    )
  ]
  out <- data.table::rbindlist(
    list(out, unmatched),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setcolorder(
    out,
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "control_run_id",
      "start_frame",
      "end_frame",
      "present_prop",
      "synchrony",
      "source_run_id",
      "control_start_frame",
      "control_end_frame",
      "control_tries_used",
      "control_status"
    )
  )
  data.table::setorder(out, id, source_run_id, numerator)
  out
}
