#' Calculate reaction rate from delta episodes
#'
#' Reaction rate is the proportion of eligible episodes that contain at least one
#' delta reaction after an initial exclusion window.
#'
#' @param coded_data Output from `add_delta_column()` or a compatible data frame.
#' @param episode_limit Maximum episode (in seconds) length to include in the denominator.
#' @param episode_limit_frames Optional maximum episode length in frames. If
#'   supplied, this takes precedence over `episode_limit`.
#' @param exclude_start Minimum reaction onset to count. Reactions that begin
#'   earlier than this are excluded from the numerator.
#' @param exclude_start_frames Optional minimum reaction onset in frames. If
#'   supplied, this takes precedence over `exclude_start`.
#' @param fps Frames per second used to convert between seconds and frames.
#' @param subject_names Character vector of subject labels to compare. If `NULL`,
#'   all unique subjects in `coded_data` are used. If supplied, data are filtered
#'   to those subject levels before reaction rate is calculated.
#' @param exclude_emotions Character vector of emotions to exclude. Default is
#'   `"neutral"`.
#' @param minimum_threshold Numeric scalar in `[0, 1]`. Episodes are included
#'   only when at least this proportion of delta frames have non-missing
#'   `value`s. Default is `0`.
#' @param constraint_method Character scalar controlling how the reaction
#'   window ends. `"strict"` uses the earlier of the observed episode end and
#'   the requested episode limit. `"episode"` uses the observed episode end and
#'   ignores `episode_limit`. `"loose"` uses the later of the observed episode
#'   end and the requested episode limit. `"frames"` uses only the requested
#'   episode limit and ignores the observed episode end. Default is
#'   `"episode"`.
#'
#' @return A data.table with columns `id`, `subject`, `emotion`, `n_episodes`,
#'   `n_reactions`, and `reaction_rate`.
#' @examples
#' library(data.table)
#'
#' coded_data <- data.table(
#'   id = rep(1L, 8),
#'   subject = rep(c("teen", "parent"), each = 4),
#'   emotion = "happy",
#'   frame = rep(1:4, 2),
#'   value = c(0.1, 0.2, NA, 0.4, 0.2, 0.3, 0.4, 0.5),
#'   delta = c(1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L)
#' )
#' reaction_rate(coded_data, fps = 30)
#' @seealso [reaction_rate_by_episode()]
#' @export
reaction_rate <- function(
  coded_data,
  episode_limit = 3,
  episode_limit_frames = NULL,
  exclude_start = 0.1,
  exclude_start_frames = NULL,
  fps = 30L,
  subject_names = NULL,
  exclude_emotions = "neutral",
  minimum_threshold = 0,
  constraint_method = "episode"
) {
  inputs <- prepare_reaction_rate_inputs(
    coded_data = coded_data,
    episode_limit = episode_limit,
    episode_limit_frames = episode_limit_frames,
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
    by = .(id, subject, emotion)
  ]

  out[, `:=`(
    n_episodes = as.integer(n_episodes),
    n_reactions = as.integer(n_reactions),
    reaction_rate = as.numeric(n_reactions / n_episodes)
  )]
  out <- out[, .(
    id,
    subject,
    emotion,
    n_episodes,
    n_reactions,
    reaction_rate
  )]
  data.table::setorder(out, id, subject, emotion)
  out
}

#' Calculate reaction rate by episode
#'
#' @inheritParams reaction_rate
#'
#' @return A data.table with columns `id`, `subject`, `emotion`, `episode_id`,
#'   `start_frame`, `end_frame`, `n_frames`, `present_prop`, and `reaction`.
#' @examples
#' library(data.table)
#'
#' coded_data <- data.table(
#'   id = rep(1L, 8),
#'   subject = rep(c("teen", "parent"), each = 4),
#'   emotion = "happy",
#'   frame = rep(1:4, 2),
#'   value = c(0.1, 0.2, NA, 0.4, 0.2, 0.3, 0.4, 0.5),
#'   delta = c(1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L)
#' )
#' reaction_rate_by_episode(coded_data, fps = 30)
#' @seealso [reaction_rate()]
#' @export
reaction_rate_by_episode <- function(
  coded_data,
  episode_limit = 3,
  episode_limit_frames = NULL,
  exclude_start = 0.1,
  exclude_start_frames = NULL,
  fps = 30L,
  subject_names = NULL,
  exclude_emotions = "neutral",
  minimum_threshold = 0,
  constraint_method = "episode"
) {
  inputs <- prepare_reaction_rate_inputs(
    coded_data = coded_data,
    episode_limit = episode_limit,
    episode_limit_frames = episode_limit_frames,
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
