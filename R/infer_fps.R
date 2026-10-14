#' Infer the frame rate of coded data from its time stamps
#'
#' Estimates frames per second for each `id` when no frame rate is available
#' from file metadata. For each `id` and `subject`, time stamps are sorted and
#' the mean step between consecutive stamps is computed, ignoring gaps (steps
#' longer than 1.5 times the median step, e.g. dropped frames). The per-subject
#' estimates are combined by their median. Estimates within `tolerance`
#' (relative) of a common rate are snapped to it; others are returned as
#' estimated.
#'
#' @param x A data frame with columns `id` and `video_time` (and optionally
#'   `subject`), or an `fr_coding` object (its `coding` table is used).
#' @param ids Optional vector of ids to restrict the inference to.
#' @param tolerance Relative tolerance for snapping to a common rate.
#'   Default 0.005.
#' @param standard_rates Numeric vector of common frame rates to snap to.
#' @param min_rows Minimum number of distinct time stamps needed for an
#'   estimate. Default 10.
#' @return A data.table with one row per `id`: `fps` (snapped when possible,
#'   `NA` if it could not be inferred), `fps_raw` (unsnapped estimate), `n`
#'   (largest number of distinct time stamps for a subject), `gap_prop`
#'   (proportion of steps treated as gaps) and `snapped`.
#' @details Warnings are issued when an `id` has too few time stamps, when
#'   subjects within an `id` disagree by more than `tolerance`, or when more
#'   than 5% of steps are gaps. Inference assumes regularly sampled time
#'   stamps; prefer the frame rate recorded in the file when it exists.
#' @examples
#' \dontrun{
#' infer_fps(coding_df)
#' infer_fps(coding_df, ids = 1)
#' }
#' @export
infer_fps <- function(
  x,
  ids = NULL,
  tolerance = 0.005,
  standard_rates = c(24, 25, 29.97, 30, 50, 59.94, 60),
  min_rows = 10L
) {
  dt <- if (is_fr_coding(x)) x$coding else x
  if (!is.data.frame(dt) || !all(c("id", "video_time") %in% names(dt))) {
    stop("`x` must have columns `id` and `video_time`.", call. = FALSE)
  }
  keep <- c("id", "video_time", intersect("subject", names(dt)))
  dt <- data.table::as.data.table(dt)[, .SD, .SDcols = keep]
  if (!"subject" %in% names(dt)) {
    dt[, subject := "all"]
  }
  dt[, id := as.character(id)]
  if (!is.null(ids)) {
    dt <- dt[id %in% as.character(ids) | (is.na(id) & anyNA(ids))]
  }
  dt <- dt[!is.na(video_time)]
  dt[, subject := as.character(subject)]
  dt <- unique(dt)

  per_subject <- dt[,
    {
      secs <- sort(unique(parse_time_to_seconds(video_time)))
      steps <- diff(secs)
      steps <- steps[steps > 0]
      if (length(secs) < min_rows || length(steps) == 0L) {
        list(n = length(secs), fps_raw = NA_real_, gap_prop = NA_real_)
      } else {
        regular <- steps <= 1.5 * stats::median(steps)
        list(
          n = length(secs),
          fps_raw = 1 / mean(steps[regular]),
          gap_prop = mean(!regular)
        )
      }
    },
    by = .(id, subject)
  ]

  per_id <- per_subject[,
    list(
      n = max(n),
      fps_raw = if (all(is.na(fps_raw))) {
        NA_real_
      } else {
        stats::median(fps_raw, na.rm = TRUE)
      },
      spread = if (all(is.na(fps_raw))) {
        NA_real_
      } else {
        diff(range(fps_raw, na.rm = TRUE)) /
          stats::median(fps_raw, na.rm = TRUE)
      },
      gap_prop = if (all(is.na(gap_prop))) {
        NA_real_
      } else {
        max(gap_prop, na.rm = TRUE)
      }
    ),
    by = id
  ]

  nearest <- vapply(
    per_id$fps_raw,
    function(r) {
      if (is.na(r)) {
        return(NA_real_)
      }
      cand <- standard_rates[which.min(abs(standard_rates - r))]
      if (abs(cand - r) / cand <= tolerance) cand else NA_real_
    },
    numeric(1)
  )
  per_id[, snapped := !is.na(nearest)]
  per_id[, fps := ifelse(snapped, nearest, fps_raw)]

  too_few <- is.na(per_id$fps_raw)
  if (any(too_few)) {
    warning(
      "Could not infer fps (too few distinct time stamps) for id(s): ",
      paste(utils::head(per_id$id[too_few], 5L), collapse = ", "),
      call. = FALSE
    )
  }
  disagree <- !is.na(per_id$spread) & per_id$spread > tolerance
  if (any(disagree)) {
    warning(
      "Subjects disagree on inferred fps for id(s): ",
      paste(utils::head(per_id$id[disagree], 5L), collapse = ", "),
      call. = FALSE
    )
  }
  gappy <- !is.na(per_id$gap_prop) & per_id$gap_prop > 0.05
  if (any(gappy)) {
    warning(
      "More than 5% of time steps are gaps (irregular sampling) for id(s): ",
      paste(utils::head(per_id$id[gappy], 5L), collapse = ", "),
      call. = FALSE
    )
  }

  per_id[, .(id, fps, fps_raw, n, gap_prop, snapped)][]
}
