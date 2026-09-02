#' Calculate reaction rate from converted episodes
#'
#' Reaction rate mirrors `synchrony()` but uses the numerator subject's
#' delta-up events as the reaction signal. Denominator episodes come from
#' `convert_to_episodes()`, and a reaction is counted when the numerator subject
#' has at least one delta event within the constrained denominator episode
#' window.
#'
#' @param coded_data Output from `convert_to_episodes()`, which includes
#'   frame-level `delta` values in `coded_data$coding` and a reaction-event
#'   table in `coded_data$deltas`.
#' @param time_limit Maximum episode-window length in seconds when a frame
#'   constraint is applied.
#' @param time_limit_frames Optional maximum episode-window length in frames.
#'   If supplied, this takes precedence over `time_limit`.
#' @param constraint_method Character scalar controlling how the reaction
#'   window ends. `"strict"` uses the earlier of the observed episode end and
#'   the requested time limit. `"episode"` uses the observed episode end and
#'   ignores `time_limit`. `"loose"` uses the later of the observed episode
#'   end and the requested time limit. `"frames"` uses only the requested
#'   time limit and ignores the observed episode end. Default is
#'   `"episode"`.
#' @param exclude_start Minimum delay, in seconds from the denominator episode
#'   start, before numerator `delta == 1` values count as a reaction.
#' @param exclude_start_frames Optional minimum delay in frames. If supplied,
#'   this takes precedence over `exclude_start`.
#' @param fps Frames per second used to convert between seconds and frames when
#'   `coded_data$metadata$fps` is unavailable.
#' @param subject_names Character vector of subject labels to compare. If
#'   `NULL`, all unique subjects in `coded_data$coding` are used.
#' @param exclude_emotions Character vector of emotions to exclude from the
#'   denominator calculation. Default is `"neutral"`.
#' @param minimum_threshold Numeric scalar in `[0, 1]`. Denominator episodes are
#'   kept only when at least this proportion of numerator frames in the
#'   constrained window have non-missing `value`s. Default is `0`.
#'
#' @return A data.table with columns `id`, `denominator`, `numerator`,
#'   `emotion`, `n_episodes`, `n_reactions`, and `reaction_rate`.
#' @examples
#' \dontrun{
#' coded_data <- convert_to_episodes(coding_df)
#' reaction_rate(coded_data)
#' }
#' @seealso [reaction_rate_by_episode()]
#' @export
reaction_rate <- function(
  coded_data,
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  exclude_start = 0.1,
  exclude_start_frames = NULL,
  fps = 30L,
  subject_names = NULL,
  exclude_emotions = "neutral",
  minimum_threshold = 0
) {
  inputs <- prepare_reaction_rate_inputs(
    coded_data = coded_data,
    time_limit = time_limit,
    time_limit_frames = time_limit_frames,
    exclude_start = exclude_start,
    exclude_start_frames = exclude_start_frames,
    fps = fps,
    subject_names = subject_names,
    exclude_emotions = exclude_emotions,
    minimum_threshold = minimum_threshold,
    constraint_method = constraint_method
  )
  episode_table <- build_reaction_rate_episode_table(inputs)

  if (nrow(episode_table) == 0L) {
    return(inputs$empty_summary_result)
  }

  out <- episode_table[,
    .(
      n_episodes = .N,
      n_reactions = sum(reaction)
    ),
    by = .(id, denominator, numerator, emotion)
  ]

  out[,
    reaction_rate := ifelse(
      n_episodes > 0L,
      n_reactions / n_episodes,
      NA_real_
    )
  ]
  out[, `:=`(
    n_episodes = as.integer(n_episodes),
    n_reactions = as.integer(n_reactions),
    reaction_rate = as.numeric(reaction_rate)
  )]
  out <- out[, .(
    id,
    denominator,
    numerator,
    emotion,
    n_episodes,
    n_reactions,
    reaction_rate
  )]
  data.table::setorder(out, id, denominator, numerator, emotion)
  out
}

#' Calculate reaction rate by denominator episode
#'
#' @inheritParams reaction_rate
#'
#' @return A data.table with columns `id`, `denominator`, `numerator`,
#'   `emotion`, `run_id`, `start_frame`, `end_frame`, `n_frames`,
#'   `present_prop`, and `reaction`.
#' @examples
#' \dontrun{
#' coded_data <- convert_to_episodes(coding_df)
#' reaction_rate_by_episode(coded_data)
#' }
#' @seealso [reaction_rate()]
#' @export
reaction_rate_by_episode <- function(
  coded_data,
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  exclude_start = 0.1,
  exclude_start_frames = NULL,
  fps = 30L,
  subject_names = NULL,
  exclude_emotions = "neutral",
  minimum_threshold = 0
) {
  inputs <- prepare_reaction_rate_inputs(
    coded_data = coded_data,
    time_limit = time_limit,
    time_limit_frames = time_limit_frames,
    exclude_start = exclude_start,
    exclude_start_frames = exclude_start_frames,
    fps = fps,
    subject_names = subject_names,
    exclude_emotions = exclude_emotions,
    minimum_threshold = minimum_threshold,
    constraint_method = constraint_method
  )
  build_reaction_rate_episode_table(inputs)
}
