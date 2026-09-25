resolve_conversion_metadata <- function(id, subject, inpath) {
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

  list(
    id = resolve_one(id, "id"),
    subject = resolve_one(subject, "subject")
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
  source_stem <- tools::file_path_sans_ext(basename(outpath))
  output_stem <- source_stem

  if (all(has_values)) {
    components <- vapply(values, as.character, character(1))
    if (any(!nzchar(components)) || any(grepl("[/\\\\:*?\"<>|]", components))) {
      stop("`id` and `subject` must produce safe filename components.")
    }
    output_stem <- paste(c(components, source_stem), collapse = "_")
    if (!is.null(type) && type %in% c("detailed", "state")) {
      output_stem <- paste(output_stem, type, sep = "_")
    }
  }

  file.path(dirname(outpath), paste0(output_stem, ".csv"))
}
