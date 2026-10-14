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
#'   way other than frame rate, or if any \code{subject}, \code{id},
#'   \code{emotion} combination occurs in more than one object. Frame rates
#'   (\code{metadata$fps}) are combined per \code{id}: objects may use
#'   different rates for different ids and the result stores a per-id table (or
#'   a single value when all ids share one rate), but an error is thrown if the
#'   same \code{id} has different rates in different objects.
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

  # fps is merged per id below; everything else must be identical
  metadata <- get_fr_metadata(objs[[1L]])
  without_fps <- function(o) {
    m <- get_fr_metadata(o)
    m$fps <- NULL
    m
  }
  same <- vapply(
    objs[-1L],
    function(o) identical(without_fps(o), without_fps(objs[[1L]])),
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

  fps_tables <- lapply(seq_along(objs), function(i) {
    o <- objs[[i]]
    stored <- get_fr_metadata(o)$fps
    if (is.null(stored)) {
      return(NULL)
    }
    ids <- unique(as.character(c(o$coding$id, o$episodes$id)))
    tbl <- as_fps_table(stored, ids)
    tbl[, object := i]
    tbl
  })
  fps_tables <- data.table::rbindlist(fps_tables, use.names = TRUE)
  if (nrow(fps_tables) > 0L) {
    clash <- fps_tables[,
      if (data.table::uniqueN(fps) > 1L) {
        list(
          rates = paste(format(unique(fps)), collapse = ", "),
          objects = toString(object)
        )
      },
      by = id
    ]
    if (nrow(clash) > 0L) {
      shown <- utils::head(clash, 5L)
      stop(
        paste0(
          "Conflicting frame rates for the same id across objects: ",
          paste(
            sprintf(
              "id=%s (%s FPS; objects %s)",
              shown$id,
              shown$rates,
              shown$objects
            ),
            collapse = "; "
          ),
          if (nrow(clash) > 5L) {
            sprintf(" ... and %d more", nrow(clash) - 5L)
          } else {
            ""
          }
        ),
        call. = FALSE
      )
    }
    merged_fps <- unique(fps_tables[, list(id, fps)])
    metadata$fps <- fps_to_metadata(merged_fps)
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
