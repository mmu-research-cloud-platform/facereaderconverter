prepare_synchrony_inputs <- function(
  coded_data,
  subject = "subject",
  id = "id",
  missing_threshold = 0,
  exclude_emotions = "neutral"
) {
  is_scalar <- function(x) {
    length(x) == 1L && !is.na(x)
  }

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

  coding <- data.table::as.data.table(coded_data$coding)
  episodes <- data.table::as.data.table(coded_data$episodes)

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
        "`coded_data$episodes` is missing required columns: %s.",
        paste(missing_episodes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

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
    present_prop = numeric(),
    synchrony = logical()
  )

  if (length(keep_emotions) == 0L) {
    return(list(
      coding = coding[0],
      episodes = episodes[0],
      empty_episode_result = empty_episode_result,
      missing_threshold = missing_threshold
    ))
  }

  coding <- coding[emotion %chin% keep_emotions]
  episodes <- episodes[emotion %chin% keep_emotions]

  list(
    coding = coding,
    episodes = episodes,
    empty_episode_result = empty_episode_result,
    missing_threshold = missing_threshold
  )
}

build_synchrony_episode_table <- function(inputs) {
  coding <- inputs$coding
  episodes <- inputs$episodes
  missing_threshold <- inputs$missing_threshold
  empty_episode_result <- inputs$empty_episode_result

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

  denominator_frames <- coding[
    in_state == TRUE & !id %in% singleton_ids,
    .(
      id,
      denominator = subject,
      emotion,
      video_time,
      run_id
    )
  ]
  if (nrow(denominator_frames) == 0L) {
    return(empty_episode_result)
  }

  comparison_frames <- coding[
    !id %in% singleton_ids,
    .(
      id,
      numerator = subject,
      emotion,
      video_time,
      comparison_value = value,
      comparison_in_state = in_state
    )
  ]

  out <- comparison_frames[
    denominator_frames,
    on = .(id, emotion, video_time),
    allow.cartesian = TRUE,
    nomatch = 0L
  ][
    numerator != denominator,
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

  data.table::setorder(out, id, denominator, numerator, emotion, run_id)
  out
}
