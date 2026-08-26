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
  minimum_threshold = 0,
  constraint_method = "episode"
) {
  fr_episodes <- NULL
  if (inherits(coded_data, "fr_coding")) {
    fps <- coded_data$metadata$fps
    fr_episodes <- coded_data$episodes
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
      !(is_reaction_rate_scalar(episode_limit_frames) &&
        is.numeric(episode_limit_frames) &&
        (is.infinite(episode_limit_frames) ||
          (is_reaction_rate_whole(episode_limit_frames) &&
            episode_limit_frames > 0)))
  ) {
    stop(
      paste0(
        "`episode_limit_frames` must be a positive integer scalar, `Inf`, ",
        "or `NULL`."
      ),
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
  if (
    !is.character(constraint_method) ||
      !is_reaction_rate_scalar(constraint_method) ||
      !constraint_method %in% c("strict", "episode", "loose", "frames")
  ) {
    stop(
      paste0(
        "`constraint_method` must be one of: ",
        "\"strict\", \"episode\", \"loose\", \"frames\"."
      ),
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

  limit_frames <- if (!is.null(episode_limit_frames)) {
    if (is.infinite(episode_limit_frames)) {
      Inf
    } else {
      as.integer(episode_limit_frames)
    }
  } else if (is.infinite(episode_limit)) {
    Inf
  } else {
    as.integer(round(episode_limit * fps))
  }
  start_frames <- if (!is.null(exclude_start_frames)) {
    as.integer(exclude_start_frames)
  } else {
    as.integer(round(exclude_start * fps))
  }

  if (
    constraint_method %in% c("loose", "frames") && is.infinite(limit_frames)
  ) {
    stop(
      paste0(
        "`episode_limit` or `episode_limit_frames` cannot be infinite when ",
        "`constraint_method` is \"loose\" or \"frames\"."
      ),
      call. = FALSE
    )
  }

  if (nrow(dt) == 0L) {
    return(list(
      dt = dt,
      fr_episodes = fr_episodes,
      limit_frames = limit_frames,
      start_frames = start_frames,
      minimum_threshold = minimum_threshold,
      constraint_method = constraint_method,
      subject_names = subject_names,
      exclude_emotions = exclude_emotions,
      empty_summary_result = empty_summary_result,
      empty_episode_result = empty_episode_result
    ))
  }

  list(
    dt = dt,
    fr_episodes = fr_episodes,
    limit_frames = limit_frames,
    start_frames = start_frames,
    minimum_threshold = minimum_threshold,
    constraint_method = constraint_method,
    subject_names = subject_names,
    exclude_emotions = exclude_emotions,
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

  if (!is.null(inputs$fr_episodes) && inputs$constraint_method == "episode") {
    episodes <- data.table::as.data.table(inputs$fr_episodes)

    if (!is.null(inputs$subject_names)) {
      episodes <- episodes[subject %chin% inputs$subject_names]
    }
    if (!is.null(inputs$exclude_emotions)) {
      episodes <- episodes[!emotion %chin% inputs$exclude_emotions]
    }

    if (nrow(episodes) == 0L) {
      return(empty_episode_result)
    }

    episodes[, subject := as.character(subject)]
    episodes[, emotion := as.character(emotion)]

    if ("value" %in% names(dt)) {
      episode_frames <- dt[
        episodes,
        on = .(
          id,
          subject,
          emotion,
          frame >= start_frame,
          frame <= end_frame
        ),
        allow.cartesian = TRUE,
        .(
          id = i.id,
          subject = i.subject,
          emotion = i.emotion,
          run_id = i.run_id,
          frame = x.frame,
          value = x.value
        )
      ]

      present_lookup <- episode_frames[,
        .(present_prop = mean(!is.na(value))),
        by = .(id, subject, emotion, run_id)
      ]
      episodes[
        present_lookup,
        on = .(id, subject, emotion, run_id),
        present_prop := i.present_prop
      ]
      episodes[is.na(present_prop), present_prop := 0]
    } else {
      episodes[, present_prop := 1]
    }

    episodes[, episode_id := seq_len(.N), by = .(id, subject, emotion)]
  } else {
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
  }

  episodes <- episodes[present_prop >= inputs$minimum_threshold]

  if (nrow(episodes) == 0L) {
    return(empty_episode_result)
  }

  episodes[,
    limit_end := if (is.infinite(inputs$limit_frames)) {
      Inf
    } else {
      start_frame + inputs$limit_frames - 1L
    }
  ]

  if (inputs$constraint_method == "strict") {
    episodes[, constraint_end := pmin(end_frame, limit_end)]
  } else if (inputs$constraint_method == "episode") {
    episodes[, constraint_end := end_frame]
  } else if (inputs$constraint_method == "loose") {
    episodes[, constraint_end := pmax(end_frame, limit_end)]
  } else {
    episodes[, constraint_end := limit_end]
  }

  episodes[, reaction_frame := start_frame + inputs$start_frames]
  episodes[, reaction := reaction_frame <= constraint_end]

  episodes[, `:=`(
    episode_id = as.integer(episode_id),
    start_frame = as.integer(start_frame),
    end_frame = as.integer(end_frame),
    n_frames = as.integer(n_frames),
    present_prop = as.numeric(present_prop),
    reaction = as.logical(reaction)
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
