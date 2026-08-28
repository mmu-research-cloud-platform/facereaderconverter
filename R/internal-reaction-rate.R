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
  is_fr_coding <- is.list(coded_data) &&
    !is.null(coded_data$coding) &&
    !is.null(coded_data$episodes)

  if (is_fr_coding && !is.null(coded_data$metadata$fps)) {
    fps <- coded_data$metadata$fps
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
        '"strict", "episode", "loose", "frames".'
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

  if (is_fr_coding) {
    coding <- data.table::as.data.table(coded_data$coding)
    episodes <- data.table::as.data.table(coded_data$episodes)

    required_coding <- c("id", "subject", "emotion", "frame", "delta")
    missing_coding <- setdiff(required_coding, names(coding))
    if (length(missing_coding) > 0L) {
      stop(
        sprintf(
          "`coded_data$coding` is missing required columns: %s.",
          paste(missing_coding, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    required_episodes <- c(
      "id",
      "subject",
      "emotion",
      "run_id",
      "start_frame",
      "end_frame"
    )
    missing_episodes <- setdiff(required_episodes, names(episodes))
    if (length(missing_episodes) > 0L) {
      stop(
        sprintf(
          "`coded_data$episodes` is missing required columns: %s.",
          paste(missing_episodes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  } else {
    coding <- data.table::as.data.table(coded_data)

    required_coding <- c(
      "id",
      "subject",
      "emotion",
      "frame",
      "delta",
      "run_id",
      "in_state"
    )
    missing_coding <- setdiff(required_coding, names(coding))
    if (length(missing_coding) > 0L) {
      stop(
        sprintf(
          "`coded_data` is missing required column(s): %s.",
          paste(missing_coding, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    episodes <- coding[
      in_state == TRUE & !is.na(run_id),
      .(
        start_frame = min(frame),
        end_frame = max(frame),
        n_frames = .N
      ),
      by = .(id, subject, emotion, run_id)
    ]
  }

  coding[, subject := as.character(subject)]
  coding[, emotion := as.character(emotion)]
  episodes[, subject := as.character(subject)]
  episodes[, emotion := as.character(emotion)]
  coding[is.na(delta), delta := 0L]
  coding[, delta := as.integer(delta)]

  if (nrow(coding) < 1L) {
    stop(
      "`coded_data$coding` must contain at least one row.",
      call. = FALSE
    )
  }

  if (!"n_frames" %in% names(episodes)) {
    episodes[, n_frames := end_frame - start_frame + 1L]
  }

  if (!is.null(subject_names)) {
    available_subjects <- unique(coding$subject)
    missing_subjects <- setdiff(subject_names, available_subjects)
    if (length(missing_subjects) > 0L) {
      stop(
        sprintf(
          "`subject_names` must be present in `coded_data$coding`: %s.",
          paste(missing_subjects, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    coding <- coding[subject %chin% subject_names]
    episodes <- episodes[subject %chin% subject_names]
  }

  if (!is.null(exclude_emotions)) {
    coding <- coding[!emotion %chin% exclude_emotions]
    episodes <- episodes[!emotion %chin% exclude_emotions]
  }

  empty_summary_result <- data.table::data.table(
    id = coding$id[0],
    denominator = character(),
    numerator = character(),
    emotion = character(),
    n_episodes = integer(),
    n_reactions = integer(),
    reaction_rate = numeric()
  )

  empty_episode_result <- data.table::data.table(
    id = coding$id[0],
    denominator = character(),
    numerator = character(),
    emotion = character(),
    run_id = integer(),
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
        '`constraint_method` is "loose" or "frames".'
      ),
      call. = FALSE
    )
  }

  if (nrow(coding) == 0L || nrow(episodes) == 0L) {
    return(list(
      coding = coding,
      episodes = episodes,
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
    coding = coding,
    episodes = episodes,
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
  coding <- inputs$coding
  episodes <- inputs$episodes
  empty_episode_result <- inputs$empty_episode_result

  if (nrow(coding) < 1L || nrow(episodes) < 1L) {
    return(empty_episode_result)
  }

  coding_by_id <- split(coding, by = "id", keep.by = TRUE)
  episodes_by_id <- split(episodes, by = "id", keep.by = TRUE)

  results <- list()
  singleton_ids <- character()

  for (id_name in names(coding_by_id)) {
    id_coding <- coding_by_id[[id_name]]
    id_episodes <- episodes_by_id[[id_name]]
    current_id <- id_coding$id[[1L]]

    if (is.null(id_episodes) || nrow(id_episodes) == 0L) {
      next
    }

    id_subjects <- unique(id_coding$subject)
    if (length(id_subjects) < 2L) {
      singleton_ids <- c(
        singleton_ids,
        sprintf("id %s: %s", current_id, id_subjects[[1L]])
      )
      next
    }

    subject_coding <- split(id_coding, by = "subject", keep.by = FALSE)
    subject_episodes <- split(id_episodes, by = "subject", keep.by = FALSE)

    for (denominator_subject in names(subject_episodes)) {
      denominator_episodes <- data.table::copy(subject_episodes[[
        denominator_subject
      ]])
      if (nrow(denominator_episodes) == 0L) {
        next
      }

      denominator_episodes[, n_frames := as.integer(n_frames)]
      denominator_episodes[,
        limit_end := if (is.infinite(inputs$limit_frames)) {
          Inf
        } else {
          start_frame + inputs$limit_frames - 1L
        }
      ]

      if (inputs$constraint_method == "strict") {
        denominator_episodes[, constraint_end := pmin(end_frame, limit_end)]
      } else if (inputs$constraint_method == "episode") {
        denominator_episodes[, constraint_end := end_frame]
      } else if (inputs$constraint_method == "loose") {
        denominator_episodes[, constraint_end := pmax(end_frame, limit_end)]
      } else {
        denominator_episodes[, constraint_end := limit_end]
      }

      denominator_episodes[, reaction_window_start := start_frame]
      denominator_episodes[,
        eligible_start_frame := start_frame + inputs$start_frames
      ]

      for (numerator_subject in id_subjects[
        id_subjects != denominator_subject
      ]) {
        numerator_coding <- subject_coding[[numerator_subject]]
        has_value <- "value" %in% names(numerator_coding)

        per_episode <- numerator_coding[
          denominator_episodes,
          on = .(
            emotion,
            frame >= reaction_window_start,
            frame <= constraint_end
          ),
          allow.cartesian = TRUE,
          .(
            id = current_id,
            denominator = denominator_subject,
            numerator = numerator_subject,
            emotion = i.emotion,
            run_id = i.run_id,
            start_frame = i.start_frame,
            end_frame = i.constraint_end,
            n_frames = i.constraint_end - i.start_frame + 1L,
            present_prop = if (has_value) mean(!is.na(value)) else 1,
            reaction = any(
              delta == 1L & frame >= i.eligible_start_frame,
              na.rm = TRUE
            )
          ),
          by = .EACHI
        ]

        per_episode[is.nan(present_prop), present_prop := 0]
        per_episode <- per_episode[present_prop >= inputs$minimum_threshold]

        if (nrow(per_episode) == 0L) {
          next
        }

        results[[length(results) + 1L]] <- per_episode[, .(
          id,
          denominator,
          numerator,
          emotion,
          run_id = as.integer(run_id),
          start_frame = as.integer(start_frame),
          end_frame = as.integer(end_frame),
          n_frames = as.integer(n_frames),
          present_prop = as.numeric(present_prop),
          reaction = as.logical(reaction)
        )]
      }
    }
  }

  if (length(singleton_ids) > 0L) {
    warning(
      sprintf(
        "Skipping id(s) with fewer than two subjects after filtering: %s.",
        paste(unique(singleton_ids), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (length(results) == 0L) {
    return(empty_episode_result)
  }

  out <- data.table::rbindlist(results, fill = TRUE)
  data.table::setorder(out, id, denominator, numerator, emotion, run_id)
  out
}
