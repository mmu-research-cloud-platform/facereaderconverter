is_reaction_rate_scalar <- function(x) {
  length(x) == 1L && !is.na(x)
}

is_reaction_rate_whole <- function(x) {
  is.numeric(x) &&
    is_reaction_rate_scalar(x) &&
    abs(x - round(x)) < .Machine$double.eps^0.5
}

prepare_reaction_rate_inputs <- function(
  coded_data,
  episode_limit = 3,
  episode_limit_frames = NULL,
  exclude_start = 0.1,
  exclude_start_frames = NULL,
  fps = 30L,
  subject_names = NULL,
  exclude_emotions = "neutral",
  minimum_threshold = 0
) {
  if (inherits(coded_data, "fr_coding")) {
    fps <- coded_data$metadata$fps
    coded_data <- coded_data$coding
  }

  if (!is_reaction_rate_whole(fps) || fps <= 0) {
    stop("`fps` must be a positive integer scalar.", call. = FALSE)
  }
  if (
    !is.numeric(episode_limit) ||
      !is_reaction_rate_scalar(episode_limit) ||
      episode_limit <= 0
  ) {
    stop("`episode_limit` must be a numeric scalar > 0.", call. = FALSE)
  }
  if (
    !is.null(episode_limit_frames) &&
      (!is_reaction_rate_whole(episode_limit_frames) ||
        episode_limit_frames <= 0)
  ) {
    stop(
      "`episode_limit_frames` must be a positive integer scalar or `NULL`.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(exclude_start) ||
      !is_reaction_rate_scalar(exclude_start) ||
      exclude_start < 0
  ) {
    stop("`exclude_start` must be a numeric scalar >= 0.", call. = FALSE)
  }
  if (
    !is.null(exclude_start_frames) &&
      (!is_reaction_rate_whole(exclude_start_frames) ||
        exclude_start_frames < 0)
  ) {
    stop(
      "`exclude_start_frames` must be a non-negative integer scalar or `NULL`.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(minimum_threshold) ||
      !is_reaction_rate_scalar(minimum_threshold) ||
      minimum_threshold < 0 ||
      minimum_threshold > 1
  ) {
    stop(
      "`minimum_threshold` must be a numeric scalar in [0, 1].",
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

  dt <- data.table::as.data.table(coded_data)

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

  empty_summary_result <- data.table::data.table(
    id = dt$id[0],
    subject = character(),
    emotion = character(),
    n_episodes = integer(),
    n_reactions = integer(),
    reaction_rate = numeric()
  )

  empty_episode_result <- data.table::data.table(
    id = dt$id[0],
    subject = character(),
    emotion = character(),
    episode_id = integer(),
    start_frame = integer(),
    end_frame = integer(),
    n_frames = integer(),
    present_prop = numeric(),
    reaction = logical()
  )

  if (nrow(dt) == 0L) {
    return(list(
      dt = dt,
      limit_frames = integer(),
      start_frames = integer(),
      minimum_threshold = minimum_threshold,
      empty_summary_result = empty_summary_result,
      empty_episode_result = empty_episode_result
    ))
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

  list(
    dt = dt,
    limit_frames = limit_frames,
    start_frames = start_frames,
    minimum_threshold = minimum_threshold,
    empty_summary_result = empty_summary_result,
    empty_episode_result = empty_episode_result
  )
}

build_reaction_rate_episode_table <- function(inputs) {
  dt <- data.table::copy(inputs$dt)
  empty_episode_result <- inputs$empty_episode_result

  if (nrow(dt) == 0L) {
    return(empty_episode_result)
  }

  dt[,
    episode_id := data.table::rleid(delta == 1),
    by = .(id, subject, emotion)
  ]

  episodes <- dt[
    delta == 1,
    .(
      start_frame = min(frame),
      end_frame = max(frame),
      n_frames = .N,
      present_prop = if ("value" %in% names(dt)) mean(!is.na(value)) else 1
    ),
    by = .(id, subject, emotion, episode_id)
  ]

  episodes <- episodes[
    n_frames <= inputs$limit_frames &
      present_prop >= inputs$minimum_threshold
  ]

  if (nrow(episodes) == 0L) {
    return(empty_episode_result)
  }

  episodes[, `:=`(
    episode_id = as.integer(episode_id),
    start_frame = as.integer(start_frame),
    end_frame = as.integer(end_frame),
    n_frames = as.integer(n_frames),
    present_prop = as.numeric(present_prop),
    reaction = start_frame >= inputs$start_frames
  )]

  out <- episodes[, .(
    id,
    subject,
    emotion,
    episode_id,
    start_frame,
    end_frame,
    n_frames,
    present_prop,
    reaction
  )]
  data.table::setorder(out, id, subject, emotion, episode_id)
  out
}
