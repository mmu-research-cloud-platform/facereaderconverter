#' Merge several fr_coding objects
#'
#' Row-binds the \code{coding}, \code{episodes} and \code{deltas} tables of
#' several \code{fr_coding} objects. The metadata is carried through once.
#'
#' @param x A list of \code{fr_coding} objects, or a character vector of names
#'   of \code{fr_coding} objects held in memory (the default way of supplying
#'   objects). A list may also contain symbols or names (e.g.
#'   \code{list(quote(a), "b")}), which are looked up in \code{envir}.
#' @param envir Environment in which object names are looked up. Default:
#'   the calling environment.
#' @return An \code{fr_coding} object. \code{run_id} and \code{delta_id} are
#'   offset for each subsequent object so they stay unique, and remain
#'   consistent between \code{coding}, \code{episodes} and \code{deltas}.
#' @details An error is thrown if the metadata of the objects differ in any
#'   way, or if any \code{subject}, \code{id}, \code{emotion} combination
#'   occurs in more than one object.
#' @examples
#' \dontrun{
#' a <- convert_to_episodes(coding_a)
#' b <- convert_to_episodes(coding_b)
#' merge_episodes(c("a", "b"))
#' merge_episodes(list(a, b))
#' }
#' @import data.table
#' @export
merge_episodes <- function(x, envir = parent.frame()) {
  if (is_fr_coding(x)) {
    stop(
      "`x` must be a list of fr_coding objects, not a single one.",
      call. = FALSE
    )
  }
  if (!is.list(x) && !is.character(x)) {
    stop(
      "`x` must be a list of fr_coding objects or a character vector of object names.",
      call. = FALSE
    )
  }
  if (length(x) < 1L) {
    stop("`x` must contain at least one fr_coding object.", call. = FALSE)
  }

  objs <- lapply(as.list(x), function(el) {
    if (is.symbol(el)) {
      el <- as.character(el)
    }
    if (is.character(el) && length(el) == 1L) {
      if (!exists(el, envir = envir, inherits = TRUE)) {
        stop(sprintf("Object `%s` not found.", el), call. = FALSE)
      }
      el <- get(el, envir = envir, inherits = TRUE)
    }
    el
  })

  ok <- vapply(objs, is_fr_coding, logical(1))
  if (!all(ok)) {
    stop(
      sprintf("Element(s) %s are not fr_coding objects.", toString(which(!ok))),
      call. = FALSE
    )
  }

  metadata <- get_fr_metadata(objs[[1L]])
  same <- vapply(
    objs[-1L],
    function(o) identical(get_fr_metadata(o), metadata),
    logical(1)
  )
  if (!all(same)) {
    stop(
      sprintf(
        "Metadata mismatch between object 1 and object(s) %s.",
        toString(which(!same) + 1L)
      ),
      call. = FALSE
    )
  }

  keys <- lapply(seq_along(objs), function(i) {
    k <- unique(data.table::as.data.table(objs[[i]]$coding)[, .(
      id,
      subject,
      emotion
    )])
    k[, `:=`(
      id = as.character(id),
      subject = as.character(subject),
      emotion = as.character(emotion),
      version = i
    )]
  })
  keys <- data.table::rbindlist(keys)
  dup <- keys[,
    if (.N > 1L) .(versions = toString(version)),
    by = .(id, subject, emotion)
  ]
  if (nrow(dup) > 0L) {
    shown <- utils::head(dup, 5L)
    stop(
      paste0(
        "Duplicated subject/id/emotion across objects: ",
        paste(
          sprintf(
            "id=%s, subject=%s, emotion=%s (objects %s)",
            shown$id,
            shown$subject,
            shown$emotion,
            shown$versions
          ),
          collapse = "; "
        ),
        if (nrow(dup) > 5L) sprintf(" ... and %d more", nrow(dup) - 5L) else ""
      ),
      call. = FALSE
    )
  }

  run_offset <- 0L
  delta_offset <- 0L
  parts <- lapply(objs, function(o) {
    coding <- data.table::copy(data.table::as.data.table(o$coding))
    episodes <- data.table::copy(data.table::as.data.table(o$episodes))
    deltas <- data.table::copy(data.table::as.data.table(o$deltas))

    if ("run_id" %in% names(coding)) {
      coding[, run_id := run_id + run_offset]
    }
    if ("run_id" %in% names(episodes)) {
      episodes[, run_id := run_id + run_offset]
    }
    if ("delta_id" %in% names(coding)) {
      coding[, delta_id := delta_id + delta_offset]
    }
    if ("delta_id" %in% names(deltas)) {
      deltas[, delta_id := delta_id + delta_offset]
    }

    run_ids <- c(episodes$run_id, coding$run_id)
    delta_ids <- c(deltas$delta_id, coding$delta_id)
    if (length(run_ids) > 0L && !all(is.na(run_ids))) {
      run_offset <<- max(run_ids, na.rm = TRUE)
    }
    if (length(delta_ids) > 0L && !all(is.na(delta_ids))) {
      delta_offset <<- max(delta_ids, na.rm = TRUE)
    }
    list(coding = coding, episodes = episodes, deltas = deltas)
  })

  bind <- function(name) {
    data.table::rbindlist(
      lapply(parts, `[[`, name),
      use.names = TRUE,
      fill = TRUE
    )
  }

  fr_coding(
    coding = bind("coding"),
    episodes = bind("episodes"),
    deltas = bind("deltas"),
    metadata = metadata
  )
}
