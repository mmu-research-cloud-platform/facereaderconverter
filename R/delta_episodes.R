#' Add create delta episodes
#'
#' @param coding Data frame with columns: id, subject, emotion, frame (or video_time), value, or an `fr_coding` object
#' @param fps Frames per second
#' @param cores integer Number of threads to use. Default 0 is auto.
#' @return episodes data.table of detected episodes with columns \code{start_frame}, \code{end_frame},\code{start_time}, \code{end_time}, \code{n_frames}, \code{duration_s}, \code{id}, \code{subject}, \code{emotion}, and \code{run_id}.
#' @examples
#' \dontrun{
#' coding_df = read.csv("testdata_detailed.csv")
#' x=add_delta_column(
#'     coding_df,
#'     delta_window = 0.2,
#'     delta = 0.1,
#'     fps = 30
#'  )
#' delta_episodes(x)
#'
#' }
#'
#' @export
#'
delta_episodes <- function(
  coding,
  fps = 30L,
  cores = 0L
) {
  old_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  data.table::setDTthreads(threads = cores)
  if (!"delta" %in% names(coding)) {
    stop("`delta` column required")
  }
  dt <- setDT(coding)
  dt[, delta_run := data.table::rleid(delta), by = .(id, subject, emotion)]

  episodes <- dt[
    delta == 1,
    .(
      start_frame = first(frame),
      end_frame = last(frame),
      start_time = first(video_time),
      end_time = last(video_time),
      n_frames = .N,
      duration_s = .N / fps
    ),
    by = .(id, subject, emotion, delta_run)
  ]
  data.table::setorder(episodes, id, subject, emotion, start_frame)
  episodes[, run_id := as.integer(.I)]

  valid_in_state <- dt[
    delta == 1 & !is.na(value),
    .(id, subject, emotion, delta_run, frame)
  ]

  if (nrow(valid_in_state) > 0L && nrow(episodes) > 0L) {
    data.table::setkey(valid_in_state, id, subject, emotion, delta_run, frame)

    end_map <- valid_in_state[
      episodes,
      on = .(
        id,
        subject,
        emotion,
        delta_run,
        frame >= start_frame,
        frame <= end_frame
      ),
      mult = "last",
      .(run_id = i.run_id, end_frame_nonNA = frame),
      by = .EACHI
    ]

    episodes[end_map, on = "run_id", end_frame := i.end_frame_nonNA]
    episodes <- episodes[!is.na(end_frame)]

    n_map <- dt[delta == TRUE][
      episodes,
      on = .(
        id,
        subject,
        emotion,
        delta_run,
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

  if ("n" %in% names(episodes)) {
    episodes[, n := NULL]
  }
  if ("delta_run" %in% names(episodes)) {
    episodes[, delta_run := NULL]
  }
  episodes
}
