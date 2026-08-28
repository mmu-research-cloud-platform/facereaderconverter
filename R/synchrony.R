#' Calculate synchrony from converted episodes
#'
#' @param coded_data Output from `convert_to_episodes()`.
#' @param subject Character scalar giving the column in `coded_data$coding` and
#'   `coded_data$episodes` that identifies the subject. Default is `"subject"`.
#' @param id Character scalar giving the column in `coded_data$coding` and
#'   `coded_data$episodes` that identifies the case or dyad. Default is `"id"`.
#' @param missing_threshold Numeric scalar in `[0, 1]`. Denominator and numerator episodes are
#'   dropped when the comparison subject is missing for less than this
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
#' @seealso [synchrony_by_episode()]
#' @export
synchrony <- function(
  coded_data,
  subject = "subject",
  id = "id",
  missing_threshold = 0,
  exclude_emotions = "neutral"
) {
  inputs <- prepare_synchrony_inputs(
    coded_data = coded_data,
    subject = subject,
    id = id,
    missing_threshold = missing_threshold,
    exclude_emotions = exclude_emotions
  )
  episode_table <- build_synchrony_episode_table(inputs)

  empty_result <- data.table::data.table(
    id = inputs$coding$id[0],
    denominator = character(),
    numerator = character(),
    emotion = character(),
    n_episodes = integer(),
    synchrony = numeric()
  )

  if (nrow(episode_table) == 0L) {
    return(empty_result)
  }

  out <- episode_table[, .(
    n_episodes = .N,
    numerator_count = sum(synchrony)
  ), by = .(id, denominator, numerator, emotion)]

  base_grid <- unique(inputs$episodes[, .(id, denominator = subject, emotion)])
  base_grid <- merge(
    base_grid,
    unique(inputs$coding[, .(id, numerator = subject)]),
    by = "id",
    allow.cartesian = TRUE,
    sort = FALSE
  )
  base_grid <- base_grid[denominator != numerator]

  out <- merge(
    base_grid,
    out,
    by = c("id", "denominator", "numerator", "emotion"),
    all.x = TRUE,
    sort = FALSE
  )
  out[is.na(n_episodes), `:=`(n_episodes = 0L, numerator_count = 0L)]
  out[, synchrony := ifelse(
    n_episodes > 0L,
    numerator_count / n_episodes,
    NA_real_
  )]
  out[, `:=`(
    n_episodes = as.integer(n_episodes),
    synchrony = as.numeric(synchrony)
  )]
  out <- out[, .(
    id,
    denominator,
    numerator,
    emotion,
    n_episodes,
    synchrony
  )]
  data.table::setorder(out, id, denominator, numerator, emotion)
  out
}
#' Calculate synchrony by denominator episode
#'
#' @inheritParams synchrony
#'
#' @return A data.table with columns `id`, `denominator`, `numerator`,
#'   `emotion`, `run_id`, `present_prop`, and `synchrony`.
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
#' synchrony_by_episode(coded_data, subject = "subject", id = "id", missing_threshold = 0)
#' @seealso [synchrony()]
#' @export
synchrony_by_episode <- function(
  coded_data,
  subject = "subject",
  id = "id",
  missing_threshold = 0,
  exclude_emotions = "neutral"
) {
  inputs <- prepare_synchrony_inputs(
    coded_data = coded_data,
    subject = subject,
    id = id,
    missing_threshold = missing_threshold,
    exclude_emotions = exclude_emotions
  )
  build_synchrony_episode_table(inputs)
}
