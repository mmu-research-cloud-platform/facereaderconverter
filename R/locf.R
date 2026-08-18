#' Apply LOCF and Extract Episodes
#'
#' Applies Last Observation Carried Forward (LOCF) logic to the `coding` data.table (from `convert_to_episodes`), imputing missing `run_id` values within blocks of non-missing `value`, grouped by `id`, `subject`, and `emotion`. Returns both the imputed coding table and a summary of contiguous episodes.
#'
#' @param coding A datatable, dataframe or fr_coding object as returned by `convert_to_episodes`, containing columns: `id`, `subject`, `emotion`, `video_time`, `value`, and `run_id`.
#' @param fps Frames per second (sampling rate of the data). Default: 30L or inherited from an fr_coding object
#' @param consecutive_missing Maximum allowed consecutive missing (NA) frames while in-state before forcing episode end. Default: 150L or inherited from an fr_coding object
#' @return A list with three elements:
#'   \describe{
#'     \item{episodes}{data.table of contiguous episodes after LOCF, with columns \code{start_frame}, \code{end_frame}, \code{n_frames}, \code{duration_s}, \code{id}, \code{subject}, \code{emotion}, and \code{run_id}.}
#'     \item{coding}{Annotated data.table with LOCF-imputed \code{run_id}.}
#'     \item{metadata}{Metadata from fr_coding object.}
#'   }
#' @import data.table
#' @export
locf <- function(coding, fps = 30, consecutive_missing = NULL) {
  stopifnot(requireNamespace("data.table"))
  if ("fr_coding" %in% class(coding)) {
    table = coding$coding
    fps = coding$metadata$fps
    if (is.null(consecutive_missing)) {
      consecutive_missing = coding$metadata$consecutive_missing
    }
    metadata = coding$metadata
  } else {
    table = coding
  }
  dt_sensitivity <- data.table::copy(table)

  data.table::setorder(dt_sensitivity, id, subject, emotion, video_time)

  dt_sensitivity[,
    value_block := cumsum(!is.na(value)),
    by = .(id, subject, emotion)
  ]

  dt_sensitivity[,
    run_id := data.table::fifelse(is.na(value), run_id[1L], run_id),
    by = .(id, subject, emotion, value_block)
  ]

  dt_sensitivity[, value_block := NULL]

  if (!is.null(consecutive_missing)) {
    dt_sensitivity[, row_id := seq_len(.N), by = .(id, subject, emotion)]
    dt_sensitivity[
      !is.na(run_id),
      last_true_row := {
        if (any(in_state)) max(row_id[in_state]) else NA_integer_
      },
      by = .(id, subject, emotion, run_id)
    ]
    dt_sensitivity[
      !is.na(run_id) &
        is.na(value) &
        !in_state &
        !is.na(last_true_row) &
        row_id > last_true_row + consecutive_missing,
      run_id := NA_integer_
    ]
    dt_sensitivity[, c("row_id", "last_true_row") := NULL]
  }

  episodes <- dt_sensitivity[
    !is.na(run_id),
    .(
      start_time = video_time[1],
      end_time = video_time[.N],
      n_frames = .N,
      duration_s = .N / fps,
      id = first(id),
      subject = first(subject),
      emotion = first(emotion),
      run_id = first(run_id)
    ),
    by = .(id, subject, emotion, run_id)
  ][]
  if (!exists("metadata")) {
    metadata = list(fps = fps, consecutive_missing = consecutive_missing)
  } else if (!is.null(consecutive_missing)) {
    metadata$consecutive_missing = consecutive_missing
  }
  structure(
    list(
      episodes = episodes,
      coding = dt_sensitivity[],
      metadata = metadata
    ),
    class = c("fr_coding", "list")
  )
}
