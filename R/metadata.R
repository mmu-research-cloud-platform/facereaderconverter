is_fr_coding <- function(x) {
  is.list(x) &&
    !is.data.frame(x) &&
    !is.null(x$coding) &&
    !is.null(x$episodes)
}

get_fr_metadata <- function(x) {
  if (!is_fr_coding(x) || !is.list(x$metadata)) {
    return(list())
  }
  x$metadata
}

resolve_fr_metadata <- function(value, x, name, default = value) {
  metadata <- get_fr_metadata(x)
  if (!is.null(metadata[[name]])) {
    return(metadata[[name]])
  }
  if (missing(value) || is.null(value)) {
    return(default)
  }
  value
}

# Per-id fps helpers -------------------------------------------------------

# Coerce an fps specification to a data.table(id, fps). `ids` is only used to
# expand an unnamed scalar to every id.
as_fps_table <- function(fps, ids = character()) {
  if (is.null(fps)) {
    return(data.table::data.table(id = character(), fps = numeric()))
  }
  if (is.data.frame(fps)) {
    if (!all(c("id", "fps") %in% names(fps))) {
      stop("`fps` table must have columns `id` and `fps`.", call. = FALSE)
    }
    return(data.table::data.table(
      id = as.character(fps$id),
      fps = as.numeric(fps$fps)
    ))
  }
  if (is.numeric(fps) && !is.null(names(fps))) {
    return(data.table::data.table(id = names(fps), fps = as.numeric(fps)))
  }
  if (is.numeric(fps) && length(fps) == 1L) {
    return(data.table::data.table(
      id = as.character(unique(ids)),
      fps = as.numeric(fps)
    ))
  }
  stop(
    "`fps` must be a single number, a named numeric vector, or a ",
    "data frame with columns `id` and `fps`.",
    call. = FALSE
  )
}

validate_fps_table <- function(tbl) {
  if (!all(c("id", "fps") %in% names(tbl))) {
    stop("fps table must have columns `id` and `fps`.", call. = FALSE)
  }
  # An NA id is a legitimate (single) group in coded data, so it is allowed
  if (anyDuplicated(tbl$id) > 0L) {
    stop("fps table must have exactly one row per id.", call. = FALSE)
  }
  if (!is.numeric(tbl$fps) || any(!is.finite(tbl$fps)) || any(tbl$fps <= 0)) {
    stop("fps values must be finite and positive.", call. = FALSE)
  }
  invisible(tbl)
}

# Resolve fps for `ids`: stored metadata, then `value`, then inference from
# `data` (if supplied), then `default`. A scalar in either source applies to
# all ids. Warns when an explicit `value` disagrees with stored metadata
# (metadata wins). Returns data.table(id, fps, fps_source).
resolve_fr_fps <- function(
  value = NULL,
  x = NULL,
  ids,
  default = 30,
  data = NULL,
  inherited = NULL
) {
  keys <- unique(as.character(ids))
  from_meta <- as_fps_table(get_fr_metadata(x)$fps, keys)
  from_arg <- as_fps_table(value, keys)
  from_file <- as_fps_table(inherited, keys)

  # Rates inherited from a file header rank with stored metadata
  meta_fps <- from_meta$fps[match(keys, from_meta$id)]
  file_fps <- from_file$fps[match(keys, from_file$id)]
  meta_fps[is.na(meta_fps)] <- file_fps[is.na(meta_fps)]
  arg_fps <- from_arg$fps[match(keys, from_arg$id)]

  conflict <- !is.na(meta_fps) &
    !is.na(arg_fps) &
    abs(meta_fps - arg_fps) > 1e-8
  if (any(conflict)) {
    warning(
      "`fps` argument conflicts with stored metadata for id(s): ",
      paste(utils::head(keys[conflict], 5L), collapse = ", "),
      "; using metadata.",
      call. = FALSE
    )
  }

  out <- meta_fps
  source <- ifelse(is.na(out), NA_character_, "metadata")
  use_arg <- is.na(out) & !is.na(arg_fps)
  out[use_arg] <- arg_fps[use_arg]
  source[use_arg] <- "argument"

  # Infer from the time stamps for ids with no recorded or supplied fps
  need <- is.na(out)
  if (any(need) && !is.null(data)) {
    inferred <- suppressWarnings(infer_fps(data, ids = keys[need]))
    hit <- match(keys[need], inferred$id)
    got <- inferred$fps[hit]
    out[need] <- got
    source[need] <- ifelse(is.na(got), NA_character_, "inferred")
    if (any(!is.na(got))) {
      message(
        "fps inferred from time stamps for id(s): ",
        paste(utils::head(keys[need][!is.na(got)], 5L), collapse = ", "),
        "."
      )
    }
  }

  use_default <- is.na(out)
  if (any(use_default)) {
    out[use_default] <- default
    source[use_default] <- "default"
  }

  tbl <- data.table::data.table(id = keys, fps = out, fps_source = source)
  validate_fps_table(tbl)
  tbl
}

fr_metadata_attr <- function(fps) {
  if (length(fps) != 1L || !is.finite(fps) || fps <= 0) {
    return(NULL)
  }
  list(fps = as.numeric(fps))
}

# Frame rate carried on data returned by the convertFR*/loadFRfile functions
fr_inherited_fps <- function(data) {
  attr(data, "fr_metadata", exact = TRUE)$fps
}

# Look up fps for each element of `ids`; returns a numeric vector.
fps_for_ids <- function(ids, fps_tbl) {
  out <- fps_tbl$fps[match(as.character(ids), as.character(fps_tbl$id))]
  if (anyNA(out)) {
    missing_ids <- unique(as.character(ids)[is.na(out)])
    stop(
      "No fps available for id(s): ",
      paste(utils::head(missing_ids, 5L), collapse = ", "),
      call. = FALSE
    )
  }
  out
}

# Data for infer_fps(): NULL when time stamps are unusable (absent, or numeric
# and so already frame indices).
fps_inference_data <- function(dt, id = "id", subject = "subject") {
  if (
    !all(c(id, "video_time") %in% names(dt)) || is.numeric(dt[["video_time"]])
  ) {
    return(NULL)
  }
  out <- data.table::data.table(id = dt[[id]], video_time = dt[["video_time"]])
  if (subject %in% names(dt)) {
    out[, subject := dt[[subject]]]
  }
  out
}

# Metadata form of an fps table: a scalar when every id shares one rate (as
# older objects stored it), otherwise a data.table(id, fps).
fps_to_metadata <- function(fps_tbl) {
  rates <- unique(fps_tbl$fps)
  if (length(rates) == 1L) {
    f <- rates[[1L]]
    return(if (abs(f - round(f)) < 1e-8) as.integer(round(f)) else f)
  }
  data.table::data.table(id = fps_tbl$id, fps = fps_tbl$fps)
}

# Seconds to frames per id. Rounds once, here. Returns a scalar when all ids
# agree, otherwise an integer vector named by id.
seconds_to_frames_by_id <- function(seconds, ids, fps_tbl) {
  keys <- unique(as.character(ids))
  out <- as.integer(round(seconds * fps_for_ids(keys, fps_tbl)))
  names(out) <- keys
  if (length(unique(out)) <= 1L) {
    unname(out)[seq_len(min(1L, length(out)))]
  } else {
    out
  }
}

# Expand a scalar-or-named frame limit to one value per element of `ids`.
frames_for_ids <- function(ids, frames) {
  if (length(frames) <= 1L && is.null(names(frames))) {
    return(rep(frames, length(ids)))
  }
  out <- unname(frames[as.character(ids)])
  if (anyNA(out)) {
    stop("No frame limit available for some ids.", call. = FALSE)
  }
  out
}

update_fr_metadata <- function(x, metadata = list()) {
  current <- get_fr_metadata(x)
  x$metadata <- utils::modifyList(current, metadata)
  x
}

attach_fr_metadata <- function(x, metadata) {
  if (length(metadata) > 0L) {
    attr(x, "fr_metadata") <- metadata
  }
  x
}

fr_coding <- function(
  coding,
  episodes = NULL,
  deltas = NULL,
  metadata = list()
) {
  structure(
    list(
      episodes = episodes,
      deltas = deltas,
      coding = coding,
      metadata = metadata
    ),
    class = c("fr_coding", "list")
  )
}
