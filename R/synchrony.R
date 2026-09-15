#' Calculate synchrony from converted episodes
#'
#' @param coded_data Output from `convert_to_episodes()`.
#' @param episodes Optional replacement episode data frame. It must contain the
#'   selected ID and subject columns plus `emotion`, `run_id`, `start_frame`, and
#'   `end_frame`. Frame bounds are inclusive. A `synchrony_by_episode()` result
#'   is also accepted, using its `denominator` column as the episode subject.
#' @param subject Character scalar giving the column in `coded_data$coding` and
#'   `coded_data$episodes` that identifies the subject. Default is `"subject"`.
#' @param id Character scalar giving the column in `coded_data$coding` and
#'   `coded_data$episodes` that identifies the case or dyad. Default is `"id"`.
#' @param time_limit Maximum episode-window length in seconds. `NULL` is
#'   allowed when `time_limit_frames` is supplied or when `constraint_method`
#'   is `"episode"`. Default is `3`.
#' @param time_limit_frames Optional maximum episode-window length in frames.
#'   If supplied, this takes precedence over `time_limit`.
#' @param constraint_method Character scalar controlling how the episode window
#'   ends: `"strict"`, `"episode"`, `"loose"`, or `"frames"`. Default is
#'   `"episode"`.
#' @param fps Frames per second used to convert `time_limit` to frames.
#'   Default is `30`.
#' @param missing_threshold Numeric scalar in `[0, 1]`. Denominator and numerator episodes are
#'   kept only when the comparison subject is present for at least this
#'   proportion of frames within the episode. Default is `0`.
#' @param exclude_emotions Character vector of emotions to exclude from the
#'   denominator calculation. Default is `"neutral"`.
#'
#' @return A data.table with columns `id`, `denominator`, `numerator`,
#'   `emotion`, `n_episodes`, and `synchrony`.
#' @examples
#' library(data.table)
#'
#' coding <- data.table(
#'   id = rep(1L, 6),
#'   subject = rep(c("teen", "parent"), each = 3),
#'   emotion = "happy",
#'   video_time = rep(1:3, 2),
#'   value = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
#'   in_state = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
#'   run_id = c(1L, 1L, 1L, 2L, 2L, 2L)
#' )
#' episodes <- data.table(
#'   id = 1L,
#'   subject = c("teen", "parent"),
#'   emotion = "happy",
#'   run_id = c(1L, 2L),
#'   start_frame = c(2L, 3L),
#'   end_frame = c(2L, 3L)
#' )
#' coded_data <- structure(
#'   list(coding = coding, episodes = episodes),
#'   class = c("fr_coding", "list")
#' )
#' synchrony(coded_data, subject = "subject", id = "id", missing_threshold = 0)
#' @seealso [synchrony_by_episode()]
#' @export
synchrony <- function(
  coded_data,
  episodes = NULL,
  subject = "subject",
  id = "id",
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  fps = 30L,
  missing_threshold = 0,
  exclude_emotions = "neutral"
) {
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
  episode_table <- build_synchrony_episode_table(inputs)

  empty_result <- data.table::data.table(
    id = inputs$coding$id[0],
    denominator = character(),
    numerator = character(),
    emotion = character(),
    n_episodes = integer(),
    synchrony = numeric()
  )

  if (nrow(episode_table) == 0L) {
    return(empty_result)
  }

  out <- episode_table[,
    .(
      n_episodes = .N,
      numerator_count = sum(synchrony)
    ),
    by = .(id, denominator, numerator, emotion)
  ]

  base_grid <- unique(inputs$episodes[, .(id, denominator = subject, emotion)])
  base_grid <- merge(
    base_grid,
    unique(inputs$coding[, .(id, numerator = subject)]),
    by = "id",
    allow.cartesian = TRUE,
    sort = FALSE
  )
  base_grid <- base_grid[denominator != numerator]

  out <- merge(
    base_grid,
    out,
    by = c("id", "denominator", "numerator", "emotion"),
    all.x = TRUE,
    sort = FALSE
  )
  out[is.na(n_episodes), `:=`(n_episodes = 0L, numerator_count = 0L)]
  out[,
    synchrony := ifelse(
      n_episodes > 0L,
      numerator_count / n_episodes,
      NA_real_
    )
  ]
  out[, `:=`(
    n_episodes = as.integer(n_episodes),
    synchrony = as.numeric(synchrony)
  )]
  out <- out[, .(
    id,
    denominator,
    numerator,
    emotion,
    n_episodes,
    synchrony
  )]
  data.table::setorder(out, id, denominator, numerator, emotion)
  out
}
#' Calculate synchrony by denominator episode
#'
#' @inheritParams synchrony
#' @details When `coded_data` contains metadata, its `metadata$fps` value is
#' used by the synchrony calculations, including when `fps` is supplied.
#'
#' @return A data.table with columns `id`, `denominator`, `numerator`,
#'   `emotion`, `run_id`, `start_frame`, `end_frame`, `present_prop`, and
#'   `synchrony`.
#' @examples
#' library(data.table)
#'
#' coding <- data.table(
#'   id = rep(1L, 6),
#'   subject = rep(c("teen", "parent"), each = 3),
#'   emotion = "happy",
#'   video_time = rep(1:3, 2),
#'   value = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
#'   in_state = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
#'   run_id = c(1L, 1L, 1L, 2L, 2L, 2L)
#' )
#' episodes <- data.table(
#'   id = 1L,
#'   subject = c("teen", "parent"),
#'   emotion = "happy",
#'   run_id = c(1L, 2L),
#'   start_frame = c(2L, 3L),
#'   end_frame = c(2L, 3L)
#' )
#' coded_data <- structure(
#'   list(coding = coding, episodes = episodes),
#'   class = c("fr_coding", "list")
#' )
#' synchrony_by_episode(coded_data, subject = "subject", id = "id", missing_threshold = 0)
#' @seealso [synchrony()]
#' @export
synchrony_by_episode <- function(
  coded_data,
  episodes = NULL,
  subject = "subject",
  id = "id",
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  fps = 30L,
  missing_threshold = 0,
  exclude_emotions = "neutral"
) {
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
  attach_fr_metadata(
    build_synchrony_episode_table(inputs),
    inputs$metadata
  )
}

#' Map shared synchronous episodes
#'
#' Maps pairs of same-emotion episodes that overlap while both subjects are in
#' state. Each unordered pair of source episodes is returned once. A source
#' episode may therefore occur in multiple rows when it overlaps more than one
#' episode of the other subject.
#'
#' @inheritParams synchrony
#' @param episodes Optional replacement episode data frame. It must contain the
#'   selected ID and subject columns plus `emotion`, `run_id`, `start_frame`,
#'   `end_frame`, and `max_value`. Frame bounds are inclusive.
#'
#' @return A data.table with episode-pair identifiers, their inclusive shared
#'   frame interval, each source episode's maximum value, the larger maximum,
#'   and their combined value.
#' @examples
#' \dontrun{
#' coded_data <- convert_to_episodes(coding_data)
#' shared_synchronous_episodes(coded_data)
#' }
#' @seealso [synchrony_by_episode()]
#' @export
shared_synchronous_episodes <- function(
  coded_data,
  episodes = NULL,
  subject = "subject",
  id = "id",
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  fps = 30L,
  missing_threshold = 0,
  exclude_emotions = "neutral"
) {
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
  if (!"max_value" %in% names(inputs$episodes)) {
    stop(
      "`coded_data$episodes` is missing required column: max_value.",
      call. = FALSE
    )
  }

  empty_result <- data.table::data.table(
    id = inputs$coding$id[0],
    emotion = character(),
    subject1 = character(),
    subject2 = character(),
    subject1_run_id = integer(),
    subject2_run_id = integer(),
    start_frame = integer(),
    end_frame = integer(),
    subject1_max_value = numeric(),
    subject2_max_value = numeric(),
    max_value = numeric(),
    combined_value = numeric()
  )
  synchrony_table <- build_synchrony_episode_table(inputs)[synchrony == TRUE]
  if (nrow(synchrony_table) == 0L || nrow(inputs$episodes) == 0L) {
    return(empty_result)
  }

  episodes <- inputs$episodes[, .(
    id,
    emotion,
    subject,
    run_id,
    start_frame,
    end_frame,
    max_value
  )]
  episode_pairs <- merge(
    episodes[, .(
      id,
      emotion,
      subject1 = subject,
      subject1_run_id = run_id,
      subject1_start_frame = start_frame,
      subject1_end_frame = end_frame,
      subject1_max_value = max_value
    )],
    episodes[, .(
      id,
      emotion,
      subject2 = subject,
      subject2_run_id = run_id,
      subject2_start_frame = start_frame,
      subject2_end_frame = end_frame,
      subject2_max_value = max_value
    )],
    by = c("id", "emotion"),
    allow.cartesian = TRUE,
    sort = FALSE
  )[
    subject1 < subject2 &
      subject1_start_frame <= subject2_end_frame &
      subject2_start_frame <= subject1_end_frame
  ]
  if (nrow(episode_pairs) == 0L) {
    return(empty_result)
  }

  synchronous_as_subject1 <- merge(
    episode_pairs,
    synchrony_table[, .(
      id,
      emotion,
      subject1 = denominator,
      subject2 = numerator,
      subject1_run_id = run_id,
      comparison_start_frame = start_frame,
      comparison_end_frame = end_frame
    )],
    by = c("id", "emotion", "subject1", "subject2", "subject1_run_id"),
    allow.cartesian = TRUE,
    sort = FALSE
  )[
    subject2_start_frame <= comparison_end_frame &
      subject2_end_frame >= comparison_start_frame
  ]
  synchronous_as_subject2 <- merge(
    episode_pairs,
    synchrony_table[, .(
      id,
      emotion,
      subject1 = numerator,
      subject2 = denominator,
      subject2_run_id = run_id,
      comparison_start_frame = start_frame,
      comparison_end_frame = end_frame
    )],
    by = c("id", "emotion", "subject1", "subject2", "subject2_run_id"),
    allow.cartesian = TRUE,
    sort = FALSE
  )[
    subject1_start_frame <= comparison_end_frame &
      subject1_end_frame >= comparison_start_frame
  ]

  out <- unique(data.table::rbindlist(
    list(
      synchronous_as_subject1,
      synchronous_as_subject2
    ),
    fill = TRUE
  ))
  if (nrow(out) == 0L) {
    return(empty_result)
  }
  out[, `:=`(
    start_frame = pmax(subject1_start_frame, subject2_start_frame),
    end_frame = pmin(subject1_end_frame, subject2_end_frame),
    max_value = pmax(subject1_max_value, subject2_max_value),
    combined_value = subject1_max_value + subject2_max_value
  )]
  out <- unique(out[, .(
    id,
    emotion,
    subject1,
    subject2,
    subject1_run_id,
    subject2_run_id,
    start_frame,
    end_frame,
    subject1_max_value,
    subject2_max_value,
    max_value,
    combined_value
  )])
  data.table::setorder(
    out,
    id,
    emotion,
    subject1,
    subject2,
    subject1_run_id,
    subject2_run_id
  )
  attach_fr_metadata(out, inputs$metadata)
}
