#' Calculate synchrony from converted episodes
#'
#' @param coded_data Output from `convert_to_episodes()`.
#' @param subject_names Character vector of subject labels to compare. If `NULL`,
#'   all unique subjects in `coded_data$coding` are used. If supplied, the data
#'   are filtered to those subject levels before synchrony is calculated.
#' @param missing_threshold Numeric scalar in `[0, 1]`. Denominator episodes are
#'   dropped when the comparison subject is missing for more than this
#'   proportion of frames within the episode.
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
#' synchrony(coded_data, subject_names = c("teen", "parent"), missing_threshold = 1)
#' @export
synchrony <- function(
  coded_data,
  subject_names = NULL,
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

  if (!is.null(subject_names)) {
    if (
      !is.character(subject_names) ||
        length(subject_names) < 1L ||
        anyNA(subject_names)
    ) {
      stop(
        "`subject_names` must be a character vector with no missing values.",
        call. = FALSE
      )
    }
    if (length(unique(subject_names)) != length(subject_names)) {
      stop(
        "`subject_names` must not contain duplicates.",
        call. = FALSE
      )
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

  coding <- data.table::as.data.table(coded_data$coding)
  episodes <- data.table::as.data.table(coded_data$episodes)

  required_coding <- c(
    "id",
    "subject",
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

  coding[, subject := as.character(subject)]
  coding[, emotion := as.character(emotion)]
  episodes[, subject := as.character(subject)]
  episodes[, emotion := as.character(emotion)]

  available_subjects <- unique(c(coding$subject, episodes$subject))
  if (is.null(subject_names)) {
    subject_names <- unique(coding$subject)
  } else {
    missing_subjects <- setdiff(subject_names, available_subjects)
    if (length(missing_subjects) > 0L) {
      stop(
        sprintf(
          "`subject_names` must be present in `coded_data$coding` and `coded_data$episodes`: %s.",
          paste(missing_subjects, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    coding <- coding[subject %chin% subject_names]
    episodes <- episodes[subject %chin% subject_names]
  }

  if (length(subject_names) < 1L) {
    stop(
      "`coded_data$coding` must contain at least one subject or `subject_names` must be supplied.",
      call. = FALSE
    )
  }

  if (is.null(exclude_emotions)) {
    keep_emotions <- unique(episodes$emotion)
  } else {
    keep_emotions <- setdiff(unique(episodes$emotion), exclude_emotions)
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

  id_values <- unique(coding$id)
  results <- list()
  singleton_ids <- character()

  for (current_id in id_values) {
    id_coding <- coding[id == current_id & emotion %chin% keep_emotions]
    id_episodes <- episodes[id == current_id & emotion %chin% keep_emotions]
    id_subjects <- unique(id_coding$subject)

    if (!is.null(subject_names)) {
      id_subjects <- subject_names[subject_names %chin% id_subjects]
    }

    if (length(id_subjects) < 2L) {
      if (length(id_subjects) == 1L) {
        singleton_ids <- c(
          singleton_ids,
          sprintf("id %s: %s", current_id, id_subjects[[1L]])
        )
      }
      next
    }

    for (denominator_subject in id_subjects) {
      denominator_episodes <- id_episodes[subject == denominator_subject]
      if (nrow(denominator_episodes) == 0L) {
        next
      }

      denominator_frames <- id_coding[
        subject == denominator_subject & in_state == TRUE,
        .(id, emotion, video_time, denom_run_id = run_id)
      ]

      if (nrow(denominator_frames) == 0L) {
        next
      }

      for (numerator_subject in id_subjects[
        id_subjects != denominator_subject
      ]) {
        comparison_frames <- id_coding[
          subject == numerator_subject,
          .(
            id,
            emotion,
            video_time,
            comparison_value = value,
            comparison_in_state = in_state
          )
        ]

        per_episode <- merge(
          denominator_frames,
          comparison_frames,
          by = c("id", "emotion", "video_time"),
          all.x = TRUE
        )[,
          .(
            missing_prop = mean(is.na(comparison_value)),
            synchrony_flag = as.integer(any(
              comparison_in_state == TRUE,
              na.rm = TRUE
            ))
          ),
          by = .(id, emotion, denom_run_id)
        ]

        eligible <- per_episode[missing_prop <= missing_threshold]

        summary <- eligible[,
          .(
            n_episodes = .N,
            numerator_count = sum(synchrony_flag)
          ),
          by = .(id, emotion)
        ]

        base <- unique(denominator_episodes[, .(id, emotion)])
        out <- summary[base, on = .(id, emotion)]
        out[is.na(n_episodes), `:=`(n_episodes = 0L, numerator_count = 0L)]
        out[, `:=`(
          denominator = denominator_subject,
          numerator = numerator_subject
        )]
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

        results[[length(results) + 1L]] <- out[, .(
          id,
          denominator,
          numerator,
          emotion,
          n_episodes,
          synchrony
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
    return(empty_result)
  }

  out <- data.table::rbindlist(results, fill = TRUE)
  data.table::setorder(out, id, denominator, numerator, emotion)
  out
}
