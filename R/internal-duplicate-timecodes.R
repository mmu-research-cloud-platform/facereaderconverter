check_duplicate_timecodes <- function(df, duplicate_timecodes_as_error = TRUE) {
  has_participant_group <- all(
    c("Participant Name", "Analysis Index") %in% names(df)
  )

  if (has_participant_group) {
    counts <- df |>
      dplyr::count(
        `Participant Name`,
        `Analysis Index`,
        `Video Time`,
        name = "n"
      )
  } else {
    counts <- df |>
      dplyr::count(`Video Time`, name = "n")
  }

  timecount <- if (nrow(counts) == 0) 0L else max(counts$n)
  if (timecount > 1 && duplicate_timecodes_as_error) {
    stop("Duplicate timecodes")
  } else if (timecount > 1) {
    warning("Duplicate timecodes", call. = FALSE)
  }

  invisible(df)
}
