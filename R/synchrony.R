#' Calculate synchrony from converted episodes
#'
#' @param coded_data Output from `convert_to_episodes()`.
#' @param subject Character scalar giving the column in `coded_data$coding` and
#'   `coded_data$episodes` that identifies the subject. Default is `"subject"`.
#' @param id Character scalar giving the column in `coded_data$coding` and
#'   `coded_data$episodes` that identifies the case or dyad. Default is `"id"`.
#' @param missing_threshold Numeric scalar in `[0, 1]`. Denominator and numerator episodes are
#'   dropped when the comparison subject is missing for more than this
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
#' @export
synchrony <- function(
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

  id_type <- typeof(coding$id)

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

  empty_result <- data.table::data.table(
    id = coding$id[0],
    denominator = character(),
    numerator = character(),
    emotion = character(),
    n_episodes = integer(),
    synchrony = numeric()
  )

  if (length(keep_emotions) == 0L) {
    return(empty_result)
  }

  coding <- coding[emotion %chin% keep_emotions]
  episodes <- episodes[emotion %chin% keep_emotions]

  cpp_result <- synchrony_cpp(
    coding = as.data.frame(coding[, .(
      id = as.character(id),
      subject,
      emotion,
      video_time,
      value,
      in_state,
      run_id
    )]),
    episodes = as.data.frame(episodes[, .(
      id = as.character(id),
      subject,
      emotion
    )]),
    missing_threshold = missing_threshold
  )

  singleton_ids <- cpp_result$singleton_ids

  if (length(singleton_ids) > 0L) {
    warning(
      sprintf(
        "Skipping id(s) with fewer than two subjects after filtering: %s.",
        paste(unique(singleton_ids), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- data.table::as.data.table(cpp_result$result)
  if (nrow(out) == 0L) {
    return(empty_result)
  }

  if (identical(id_type, "integer")) {
    out[, id := as.integer(id)]
  } else if (identical(id_type, "double")) {
    out[, id := as.numeric(id)]
  }

  out[, n_episodes := as.integer(n_episodes)]
  out[, synchrony := as.numeric(synchrony)]
  data.table::setorder(out, id, denominator, numerator, emotion)
  out
}
