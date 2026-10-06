fr_filename_stem <- function(path) {
  path <- gsub("\\\\", "/", path)
  tools::file_path_sans_ext(basename(path))
}

fr_media_id <- function(video_filename) {
  if (
    is.null(video_filename) ||
      length(video_filename) != 1L ||
      is.na(video_filename) ||
      !nzchar(trimws(video_filename))
  ) {
    return(NULL)
  }
  fr_filename_stem(trimws(video_filename))
}

resolve_conversion_metadata <- function(
  id,
  subject,
  inpath,
  video_filename = NULL,
  id_pattern = NULL,
  subject_pattern = NULL,
  use_full_path = FALSE
) {
  resolve_one <- function(value, name) {
    if (is.null(value)) {
      return(NULL)
    }
    if (is.function(value)) {
      value <- value(inpath)
    }
    if (!is.atomic(value) || length(value) != 1L || is.na(value)) {
      stop(
        "`",
        name,
        "` must resolve to one non-missing string or numeric value."
      )
    }
    if (!is.character(value) && !is.numeric(value)) {
      stop(
        "`",
        name,
        "` must be a string, numeric value, or function returning one."
      )
    }
    value
  }

  inferred <- infer_conversion_metadata(
    inpath,
    video_filename,
    id_pattern,
    subject_pattern,
    use_full_path
  )
  resolved_id <- resolve_one(id, "id")
  resolved_subject <- resolve_one(subject, "subject")
  if (is.null(resolved_id)) {
    resolved_id <- inferred$id
  }
  if (is.null(resolved_subject)) {
    resolved_subject <- inferred$subject
  }

  list(id = resolved_id, subject = resolved_subject)
}

infer_conversion_metadata <- function(
  inpath,
  video_filename = NULL,
  id_pattern = NULL,
  subject_pattern = NULL,
  use_full_path = FALSE
) {
  validate_metadata_pattern(id_pattern, "id_pattern")
  validate_metadata_pattern(subject_pattern, "subject_pattern")
  if (
    !is.logical(use_full_path) ||
      length(use_full_path) != 1L ||
      is.na(use_full_path)
  ) {
    stop("`use_full_path` must be TRUE or FALSE.", call. = FALSE)
  }

  if (tolower(tools::file_ext(inpath)) == "csv") {
    # CSVs are usually converted outputs that already carry metadata columns,
    # so only an explicit subject pattern infers metadata for them.
    subject_source <- if (use_full_path) inpath else fr_filename_stem(inpath)
    return(list(
      id = NULL,
      subject = if (is.null(subject_pattern)) {
        NULL
      } else {
        extract_metadata_pattern(subject_source, subject_pattern)
      }
    ))
  }
  id_source <- fr_media_id(video_filename)
  subject_source <- fr_filename_stem(inpath)
  if (use_full_path && !is.null(id_pattern)) {
    id_source <- video_filename
  }
  if (use_full_path && !is.null(subject_pattern)) {
    subject_source <- inpath
  }

  list(
    id = extract_metadata_pattern(id_source, id_pattern),
    subject = extract_metadata_pattern(subject_source, subject_pattern)
  )
}

validate_metadata_pattern <- function(pattern, name) {
  if (
    !is.null(pattern) &&
      (!is.character(pattern) ||
        length(pattern) != 1L ||
        is.na(pattern) ||
        !nzchar(pattern))
  ) {
    stop(
      "`",
      name,
      "` must be NULL or one non-empty regular expression.",
      call. = FALSE
    )
  }
  invisible(pattern)
}

extract_metadata_pattern <- function(source, pattern) {
  if (
    is.null(source) || length(source) != 1L || is.na(source) || !nzchar(source)
  ) {
    return(NULL)
  }
  if (is.null(pattern)) {
    return(source)
  }
  match <- stringr::str_extract(source, pattern)
  if (is.na(match)) NULL else stringr::str_trim(match)
}

metadata_columns <- function(data) {
  get_value <- function(column) {
    if (!column %in% names(data)) {
      return(NULL)
    }
    values <- unique(data[[column]])
    values <- values[!is.na(values)]
    if (length(values) == 1L) values[[1L]] else NULL
  }
  list(id = get_value("id"), subject = get_value("subject"))
}

# Output filenames only use metadata the caller asked for explicitly, so
# default inferred metadata does not rename converted CSVs.
fr_naming_metadata <- function(
  metadata,
  id = NULL,
  subject = NULL,
  id_pattern = NULL,
  subject_pattern = NULL
) {
  list(
    id = if (is.null(id) && is.null(id_pattern)) NULL else metadata$id,
    subject = if (is.null(subject) && is.null(subject_pattern)) {
      NULL
    } else {
      metadata$subject
    }
  )
}

fr_conversion_type <- function(data) {
  if ("neutral" %in% names(data) || "Neutral" %in% names(data)) {
    return("detailed")
  }
  if (
    "dominant_expression" %in%
      names(data) ||
      "Dominant Expression" %in% names(data)
  ) {
    return("state")
  }
  NULL
}

add_fr_metadata <- function(data, metadata) {
  metadata <- metadata[!vapply(metadata, is.null, logical(1))]
  collisions <- intersect(names(data), names(metadata))
  if (length(collisions) > 0L) {
    stop(
      "Metadata column(s) already exist in input: ",
      paste(collisions, collapse = ", ")
    )
  }

  for (name in names(metadata)) {
    data[[name]] <- rep(metadata[[name]], nrow(data))
  }
  data
}

fr_output_path <- function(outpath, metadata, type = NULL) {
  values <- metadata[c("id", "subject")]
  has_values <- vapply(values, Negate(is.null), logical(1))
  has_values <- has_values &
    !vapply(values, function(value) anyNA(value), logical(1))
  source_stem <- tools::file_path_sans_ext(basename(outpath))
  output_stem <- source_stem

  if (all(has_values)) {
    components <- vapply(values, as.character, character(1))
    if (any(!nzchar(components)) || any(grepl("[/\\\\:*?\"<>|]", components))) {
      stop("`id` and `subject` must produce safe filename components.")
    }
    output_parts <- components
    if (!identical(components[[2L]], source_stem)) {
      output_parts <- c(output_parts, source_stem)
    }
    output_stem <- paste(output_parts, collapse = "_")
    if (!is.null(type) && type %in% c("detailed", "state")) {
      output_stem <- paste(output_stem, type, sep = "_")
    }
  }

  output_dir <- normalizePath(
    dirname(outpath),
    winslash = "/",
    mustWork = FALSE
  )
  max_stem_length <- min(
    240L,
    250L - nchar(output_dir) - 1L - nchar(".csv")
  )
  if (max_stem_length < 1L) {
    stop("Output directory path is too long to create a CSV file.")
  }
  if (nchar(output_stem) > max_stem_length) {
    hash <- 0
    for (codepoint in utf8ToInt(output_stem)) {
      hash <- (hash * 31 + codepoint) %% 2147483647
    }
    suffix <- sprintf("_%08x", as.integer(hash))
    prefix_length <- max_stem_length - nchar(suffix)
    if (prefix_length < 1L) {
      stop("Output directory path is too long to create a CSV file.")
    }
    output_stem <- paste0(
      substr(output_stem, 1L, prefix_length),
      suffix
    )
  }

  file.path(dirname(outpath), paste0(output_stem, ".csv"))
}
