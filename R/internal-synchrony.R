prepare_synchrony_inputs <- function(
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
  is_scalar <- function(x) {
    length(x) == 1L && !is.na(x)
  }

  fps <- resolve_fr_metadata(fps, coded_data, "fps", default = 30L)

  if (
    !is.list(coded_data) ||
      is.null(coded_data$coding) ||
      is.null(coded_data$episodes)
  ) {
    stop(
      "`coded_data` must be the object returned by `convert_to_episodes()`.",
      call. = FALSE
    )
  }

  if (!is.numeric(fps) || !is_scalar(fps) || fps <= 0) {
    stop("`fps` must be a positive numeric scalar.", call. = FALSE)
  }
  if (
    !is.null(time_limit) &&
      (!is.numeric(time_limit) || !is_scalar(time_limit) || time_limit <= 0)
  ) {
    stop("`time_limit` must be a numeric scalar > 0 or `NULL`.", call. = FALSE)
  }
  if (
    !is.null(time_limit_frames) &&
      !(is.numeric(time_limit_frames) &&
        is_scalar(time_limit_frames) &&
        (is.infinite(time_limit_frames) ||
          (abs(time_limit_frames - round(time_limit_frames)) <
            .Machine$double.eps^0.5 &&
            time_limit_frames > 0)))
  ) {
    stop(
      "`time_limit_frames` must be a positive integer scalar, `Inf`, or `NULL`.",
      call. = FALSE
    )
  }
  if (
    !is.character(constraint_method) ||
      !is_scalar(constraint_method) ||
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
  if (
    constraint_method != "episode" &&
      is.null(time_limit) &&
      is.null(time_limit_frames)
  ) {
    stop(
      paste0(
        "`time_limit` or `time_limit_frames` must be set when ",
        '`constraint_method` is not "episode".'
      ),
      call. = FALSE
    )
  }
  if (
    !is.numeric(missing_threshold) ||
      !is_scalar(missing_threshold) ||
      missing_threshold < 0 ||
      missing_threshold > 1
  ) {
    stop(
      "`missing_threshold` must be a numeric scalar in [0, 1].",
      call. = FALSE
    )
  }

  if (!is.character(subject) || !is_scalar(subject) || anyNA(subject)) {
    stop(
      "`subject` must be a character scalar with no missing values.",
      call. = FALSE
    )
  }

  if (!is.character(id) || !is_scalar(id) || anyNA(id)) {
    stop(
      "`id` must be a character scalar with no missing values.",
      call. = FALSE
    )
  }

  if (identical(subject, id)) {
    stop(
      "`subject` and `id` must refer to different columns.",
      call. = FALSE
    )
  }

  if (!is.null(exclude_emotions)) {
    if (!is.character(exclude_emotions) || anyNA(exclude_emotions)) {
      stop(
        "`exclude_emotions` must be a character vector or `NULL`.",
        call. = FALSE
      )
    }
  }

  coding <- data.table::as.data.table(data.table::copy(coded_data$coding))
  episodes <- if (is.null(episodes)) {
    data.table::as.data.table(data.table::copy(coded_data$episodes))
  } else {
    data.table::as.data.table(data.table::copy(episodes))
  }
  if (!"frame" %in% names(coding)) {
    coding[,
      frame := if (is.numeric(video_time)) {
        as.integer(video_time)
      } else {
        parse_time_to_frame(video_time, fps = fps)
      }
    ]
  }

  required_coding <- c(
    id,
    subject,
    "emotion",
    "video_time",
    "value",
    "in_state",
    "run_id"
  )
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

  if (!subject %in% names(episodes) && "denominator" %in% names(episodes)) {
    data.table::setnames(episodes, "denominator", subject)
  }
  required_episodes <- c(
    id,
    subject,
    "emotion",
    "run_id",
    "start_frame",
    "end_frame"
  )
  missing_episodes <- setdiff(required_episodes, names(episodes))
  if (length(missing_episodes) > 0L) {
    stop(
      sprintf(
        "`episodes` is missing required columns: %s.",
        paste(missing_episodes, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (
    !is.numeric(episodes$start_frame) ||
      !is.numeric(episodes$end_frame) ||
      any(!is.finite(episodes$start_frame)) ||
      any(!is.finite(episodes$end_frame)) ||
      any(
        abs(episodes$start_frame - round(episodes$start_frame)) >
          .Machine$double.eps^0.5
      ) ||
      any(
        abs(episodes$end_frame - round(episodes$end_frame)) >
          .Machine$double.eps^0.5
      ) ||
      any(episodes$start_frame > episodes$end_frame)
  ) {
    stop(
      "`episodes` must have finite integer-like inclusive frame bounds with `start_frame <= end_frame`.",
      call. = FALSE
    )
  }
  if (
    anyNA(episodes[[id]]) ||
      anyNA(episodes[[subject]]) ||
      anyNA(episodes$emotion) ||
      anyNA(episodes$run_id)
  ) {
    stop(
      "`episodes` identifiers must not contain missing values.",
      call. = FALSE
    )
  }
  episodes[, `:=`(
    start_frame = as.integer(start_frame),
    end_frame = as.integer(end_frame)
  )]

  if (subject != "subject") {
    data.table::setnames(coding, subject, "subject")
    data.table::setnames(episodes, subject, "subject")
  }
  if (id != "id") {
    data.table::setnames(coding, id, "id")
    data.table::setnames(episodes, id, "id")
  }

  coding[, subject := as.character(subject)]
  coding[, emotion := as.character(emotion)]
  episodes[, subject := as.character(subject)]
  episodes[, emotion := as.character(emotion)]

  if (nrow(coding) < 1L) {
    stop(
      "`coded_data$coding` must contain at least one row.",
      call. = FALSE
    )
  }

  known_episodes <- data.table::copy(episodes)
  keep_emotions <- if (is.null(exclude_emotions)) {
    unique(episodes$emotion)
  } else {
    setdiff(unique(episodes$emotion), exclude_emotions)
  }

  empty_episode_result <- data.table::data.table(
    id = coding$id[0],
    denominator = character(),
    numerator = character(),
    emotion = character(),
    run_id = integer(),
    start_frame = integer(),
    end_frame = integer(),
    present_prop = numeric(),
    synchrony = logical()
  )

  limit_frames <- if (!is.null(time_limit_frames)) {
    if (is.infinite(time_limit_frames)) Inf else as.integer(time_limit_frames)
  } else if (is.null(time_limit) || is.infinite(time_limit)) {
    Inf
  } else {
    as.integer(round(time_limit * fps))
  }
  if (
    constraint_method %in% c("loose", "frames") && is.infinite(limit_frames)
  ) {
    stop(
      paste0(
        "`time_limit` or `time_limit_frames` cannot be infinite when ",
        '`constraint_method` is "loose" or "frames".'
      ),
      call. = FALSE
    )
  }

  metadata <- get_fr_metadata(coded_data)
  empty_episode_result <- attach_fr_metadata(empty_episode_result, metadata)

  if (length(keep_emotions) == 0L) {
    return(list(
      coding = coding[0],
      episodes = episodes[0],
      known_episodes = known_episodes,
      empty_episode_result = empty_episode_result,
      missing_threshold = missing_threshold,
      limit_frames = limit_frames,
      constraint_method = constraint_method,
      metadata = metadata
    ))
  }

  coding <- coding[emotion %chin% keep_emotions]
  episodes <- episodes[emotion %chin% keep_emotions]
  list(
    coding = coding,
    episodes = episodes,
    known_episodes = known_episodes,
    empty_episode_result = attach_fr_metadata(empty_episode_result, metadata),
    missing_threshold = missing_threshold,
    limit_frames = limit_frames,
    constraint_method = constraint_method,
    metadata = metadata
  )
}

build_synchrony_episode_table <- function(inputs) {
  coding <- inputs$coding
  episodes <- inputs$episodes
  missing_threshold <- inputs$missing_threshold
  empty_episode_result <- inputs$empty_episode_result
  limit_frames <- inputs$limit_frames
  constraint_method <- inputs$constraint_method

  if (nrow(coding) < 1L || nrow(episodes) < 1L) {
    return(empty_episode_result)
  }

  subject_counts <- coding[, .(subjects = uniqueN(subject)), by = id]
  singleton_ids <- subject_counts[subjects < 2L, id]
  if (length(singleton_ids) > 0L) {
    singleton_labels <- coding[
      id %in% singleton_ids,
      .(
        label = sprintf("id %s: %s", first(id), first(subject))
      ),
      by = id
    ]$label
    warning(
      sprintf(
        "Skipping id(s) with fewer than two subjects after filtering: %s.",
        paste(singleton_labels, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  denominator_episodes <- episodes[
    !id %in% singleton_ids,
    .(
      id,
      denominator = subject,
      emotion,
      run_id,
      start_frame,
      end_frame
    )
  ]
  if (nrow(denominator_episodes) == 0L) {
    return(empty_episode_result)
  }
  denominator_episodes[,
    limit_end := if (is.infinite(limit_frames)) {
      Inf
    } else {
      start_frame + limit_frames - 1L
    }
  ]
  denominator_episodes[,
    constraint_end := if (constraint_method == "strict") {
      pmin(end_frame, limit_end)
    } else if (constraint_method == "episode") {
      end_frame
    } else if (constraint_method == "loose") {
      pmax(end_frame, limit_end)
    } else {
      limit_end
    }
  ]

  numerator_grid <- unique(coding[
    !id %in% singleton_ids,
    .(id, numerator = subject)
  ])
  expected_episodes <- numerator_grid[
    denominator_episodes,
    on = "id",
    allow.cartesian = TRUE,
    nomatch = 0L
  ][numerator != denominator]

  comparison_frames <- coding[
    !id %in% singleton_ids,
    .(
      id,
      numerator = subject,
      emotion,
      frame,
      comparison_value = value,
      comparison_in_state = in_state
    )
  ]

  expected_frames <- expected_episodes[,
    .(frame = seq.int(start_frame, constraint_end)),
    by = .(id, denominator, numerator, emotion, run_id)
  ]
  out <- comparison_frames[
    expected_frames,
    on = .(id, numerator, emotion, frame),
    nomatch = NA,
    allow.cartesian = TRUE
  ][,
    .(
      present_prop = mean(!is.na(comparison_value)),
      synchrony = as.logical(any(
        comparison_in_state == TRUE,
        na.rm = TRUE
      ))
    ),
    by = .(id, denominator, numerator, emotion, run_id)
  ]

  out <- out[present_prop >= missing_threshold]
  if (nrow(out) == 0L) {
    return(empty_episode_result)
  }

  out <- merge(
    out,
    unique(expected_episodes[, .(
      id,
      denominator,
      numerator,
      emotion,
      run_id,
      start_frame,
      end_frame = constraint_end
    )]),
    by = c("id", "denominator", "numerator", "emotion", "run_id"),
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setcolorder(
    out,
    c(
      "id",
      "denominator",
      "numerator",
      "emotion",
      "run_id",
      "start_frame",
      "end_frame",
      "present_prop",
      "synchrony"
    )
  )
  data.table::setorder(out, id, denominator, numerator, emotion, run_id)
  out
}
