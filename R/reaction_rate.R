#' Calculate reaction rate from delta episodes
#'
#' Reaction rate is the proportion of eligible episodes that contain at least one
#' delta reaction after an initial exclusion window.
#'
#' @param coded_data Output from `add_delta_column()` or a compatible data frame.
#' @param episode_limit Maximum episode length to include in the denominator.
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
#'   delta = c(1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L)
#' )
#' reaction_rate(coded_data, fps = 30)
#' @export
reaction_rate <- function(
  coded_data,
  episode_limit = 3,
  episode_limit_frames = NULL,
  exclude_start = 0.1,
  exclude_start_frames = NULL,
  fps = 30L,
  subject_names = NULL,
  exclude_emotions = "neutral"
) {
  is_scalar <- function(x) {
    length(x) == 1L && !is.na(x)
  }
  is_whole <- function(x) {
    is.numeric(x) && is_scalar(x) && abs(x - round(x)) < .Machine$double.eps^0.5
  }

  empty_result <- data.table::data.table(
    id = integer(),
    subject = character(),
    emotion = character(),
    n_episodes = integer(),
    n_reactions = integer(),
    reaction_rate = numeric()
  )

  if (inherits(coded_data, "fr_coding")) {
    fps <- coded_data$metadata$fps
    coded_data <- coded_data$coding
  }

  if (!is_whole(fps) || fps <= 0) {
    stop("`fps` must be a positive integer scalar.", call. = FALSE)
  }
  if (
    !is.numeric(episode_limit) ||
      !is_scalar(episode_limit) ||
      episode_limit <= 0
  ) {
    stop("`episode_limit` must be a numeric scalar > 0.", call. = FALSE)
  }
  if (
    !is.null(episode_limit_frames) &&
      (!is_whole(episode_limit_frames) || episode_limit_frames <= 0)
  ) {
    stop(
      "`episode_limit_frames` must be a positive integer scalar or `NULL`.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(exclude_start) || !is_scalar(exclude_start) || exclude_start < 0
  ) {
    stop("`exclude_start` must be a numeric scalar >= 0.", call. = FALSE)
  }
  if (
    !is.null(exclude_start_frames) &&
      (!is_whole(exclude_start_frames) || exclude_start_frames < 0)
  ) {
    stop(
      "`exclude_start_frames` must be a non-negative integer scalar or `NULL`.",
      call. = FALSE
    )
  }
  if (!is.null(subject_names)) {
    if (
      !is.character(subject_names) ||
        anyNA(subject_names) ||
        length(subject_names) < 1L
    ) {
      stop(
        "`subject_names` must be a character vector with no missing values.",
        call. = FALSE
      )
    }
    if (length(unique(subject_names)) != length(subject_names)) {
      stop("`subject_names` must not contain duplicates.", call. = FALSE)
    }
  }
  if (!is.null(exclude_emotions)) {
    if (!is.character(exclude_emotions) || anyNA(exclude_emotions)) {
      stop(
        "`exclude_emotions` must be a character vector or `NULL`.",
        call. = FALSE
      )
    }
  }

  if (all(c("delta", "frame") %in% names(coded_data))) {
    dt <- data.table::as.data.table(coded_data)
  } else {
    dt <- add_delta_column(coded_data, fps = fps)
  }

  required <- c("id", "subject", "emotion", "frame", "delta")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "`coded_data` is missing required columns: %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  dt[, subject := as.character(subject)]
  dt[, emotion := as.character(emotion)]

  if (!is.null(subject_names)) {
    available_subjects <- unique(dt$subject)
    missing_subjects <- setdiff(subject_names, available_subjects)
    if (length(missing_subjects) > 0L) {
      stop(
        sprintf(
          "`subject_names` must be present in `coded_data`: %s.",
          paste(missing_subjects, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    dt <- dt[subject %chin% subject_names]
  }

  if (!is.null(exclude_emotions)) {
    dt <- dt[!emotion %chin% exclude_emotions]
  }

  if (nrow(dt) == 0L) {
    return(empty_result)
  }

  limit_frames <- if (!is.null(episode_limit_frames)) {
    as.integer(episode_limit_frames)
  } else {
    as.integer(round(episode_limit * fps))
  }
  start_frames <- if (!is.null(exclude_start_frames)) {
    as.integer(exclude_start_frames)
  } else {
    as.integer(round(exclude_start * fps))
  }

  dt[,
    episode_id := data.table::rleid(delta == 1),
    by = .(id, subject, emotion)
  ]

  episodes <- dt[
    delta == 1,
    .(
      start_frame = min(frame),
      n_frames = .N,
      first_reaction_frame = min(frame)
    ),
    by = .(id, subject, emotion, episode_id)
  ][n_frames <= limit_frames]

  if (nrow(episodes) == 0L) {
    dt[, episode_id := NULL]
    return(empty_result)
  }

  summary <- episodes[,
    .(
      n_episodes = .N,
      n_reactions = sum(first_reaction_frame >= start_frames, na.rm = TRUE)
    ),
    by = .(id, subject, emotion)
  ]

  out <- data.table::as.data.table(summary)
  out[, `:=`(
    n_episodes = as.integer(n_episodes),
    n_reactions = as.integer(n_reactions),
    reaction_rate = ifelse(n_episodes > 0L, n_reactions / n_episodes, NA_real_)
  )]
  out <- out[, .(
    id,
    subject,
    emotion,
    n_episodes,
    n_reactions,
    reaction_rate = as.numeric(reaction_rate)
  )]
  data.table::setorder(out, id, subject, emotion)
  dt[, episode_id := NULL]
  out
}
