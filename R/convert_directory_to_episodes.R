#' Convert a directory of detailed FaceReader exports to episodes
#'
#' Recursively loads detailed TXT and XLSX exports, combines their emotion
#' values, and calls [convert_to_episodes()] once. All exports must have the
#' same valid frame rate in their metadata (rounded to an integer). State
#' exports are ignored. Returns the combined result in memory and saves it
#' as `coded_data` in one `.RDa` file.
#'
#' @param inpath Directory containing FaceReader exports.
#' @param outpath Path to the output `.RDa` file. Defaults to `episodes.RDa`
#'   inside `inpath`.
#' @param id_pattern,subject_pattern Optional regular expressions matched using
#'   [extract_subject_id_metadata()]. For each field, the filename is checked
#'   first, followed by enclosing folder names from nearest to outermost,
#'   including `inpath`. If a rule is `NULL`, that field uses the filename
#'   without its extension. A supplied pattern must match every detailed export.
#'   If `id_pattern` matches distinct IDs anywhere in these names, conversion
#'   stops rather than choosing one.
#' @param recursive Whether to search subdirectories.
#' @param overwrite Whether to replace an existing output file. Defaults to
#'   `FALSE`.
#' @param T_up,T_down,delta,delta_window,min_dur_sec,consecutive_missing,cores
#'   Passed to [convert_to_episodes()].
#'
#' @return The combined `fr_coding` object returned by
#'   [convert_to_episodes()], invisibly. The same object is saved as
#'   `coded_data` in `outpath`.
#' @examples
#' \dontrun{
#' coded_data <- convert_directory_to_episodes(
#'   "path/to/exports",
#'   id_pattern = "[0-9]{4}",
#'   subject_pattern = "mum|teen"
#' )
#' }
#' @export
convert_directory_to_episodes <- function(
  inpath,
  outpath = file.path(inpath, "episodes.RDa"),
  id_pattern = NULL,
  subject_pattern = NULL,
  recursive = TRUE,
  overwrite = FALSE,
  T_up = 0.20,
  T_down = 0.1,
  delta = 0.10,
  delta_window = 0.2,
  min_dur_sec = 0.1,
  consecutive_missing = 150L,
  cores = 0L
) {
  if (
    !is.character(inpath) ||
      length(inpath) != 1L ||
      is.na(inpath) ||
      !dir.exists(inpath)
  ) {
    stop("`inpath` must be one existing directory.", call. = FALSE)
  }
  if (
    !is.character(outpath) ||
      length(outpath) != 1L ||
      is.na(outpath) ||
      !nzchar(outpath) ||
      !tolower(tools::file_ext(outpath)) %in% c("rda", "rdata")
  ) {
    stop("`outpath` must be a .RDa or .RData file path.", call. = FALSE)
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (file.exists(outpath) && !overwrite) {
    stop("Output already exists: ", outpath, call. = FALSE)
  }
  for (pattern in list(id_pattern, subject_pattern)) {
    if (
      !is.null(pattern) &&
        (!is.character(pattern) ||
          length(pattern) != 1L ||
          is.na(pattern) ||
          !nzchar(pattern))
    ) {
      stop(
        "ID and subject patterns must be NULL or one non-empty regex.",
        call. = FALSE
      )
    }
  }

  files <- sort(list.files(
    inpath,
    recursive = recursive,
    full.names = TRUE,
    pattern = "\\.(txt|xlsx)$",
    ignore.case = TRUE
  ))
  if (!length(files)) {
    stop("No FaceReader TXT or XLSX files were found.", call. = FALSE)
  }
  headers <- lapply(files, synchrony_fr_header_metadata)
  detailed <- vapply(
    headers,
    function(header) identical(header$type, "detailed"),
    logical(1)
  )
  files <- files[detailed]
  headers <- headers[detailed]
  if (!length(files)) {
    stop("No detailed FaceReader exports were found.", call. = FALSE)
  }

  fps <- vapply(headers, `[[`, numeric(1), "fps")
  invalid <- !is.finite(fps) | fps <= 0 | fps != round(fps)
  if (any(invalid)) {
    stop(
      "Invalid or missing frame rate in: ",
      paste(files[invalid], collapse = ", "),
      call. = FALSE
    )
  }
  if (length(unique(fps)) != 1L) {
    stop(
      "Conflicting frame rates in: ",
      paste(sprintf("%s (%s FPS)", files, fps), collapse = ", "),
      call. = FALSE
    )
  }

  stems <- tools::file_path_sans_ext(basename(files))
  input_root <- normalizePath(inpath, winslash = "/", mustWork = TRUE)
  identifiers <- lapply(seq_along(files), function(i) {
    file_dir <- normalizePath(
      dirname(files[[i]]),
      winslash = "/",
      mustWork = TRUE
    )
    relative_dir <- if (identical(file_dir, input_root)) {
      ""
    } else {
      substring(file_dir, nchar(input_root) + 2L)
    }
    folder_names <- if (nzchar(relative_dir)) {
      rev(strsplit(relative_dir, "/", fixed = TRUE)[[1L]])
    } else {
      character()
    }
    candidates <- c(basename(files[[i]]), folder_names, basename(input_root))
    if (!is.null(id_pattern)) {
      id_matches <- unique(unlist(
        lapply(candidates, function(candidate) {
          stringr::str_trim(stringr::str_extract_all(candidate, id_pattern)[[
            1L
          ]])
        }),
        use.names = FALSE
      ))
      id_matches <- id_matches[nzchar(id_matches)]
      if (length(id_matches) > 1L) {
        stop(
          "Multiple different IDs match `id_pattern` in: ",
          files[[i]],
          ": ",
          paste(id_matches, collapse = ", "),
          call. = FALSE
        )
      }
    }
    find_match <- function(pattern, field, fallback) {
      if (is.null(pattern)) {
        return(fallback)
      }
      for (candidate in candidates) {
        result <- extract_subject_id_metadata(
          candidate,
          id_pattern = if (field == "id") pattern else "(?!)",
          subject_pattern = if (field == "subject") pattern else "(?!)"
        )[[field]]
        if (!is.na(result) && nzchar(result)) {
          return(result)
        }
      }
      NA_character_
    }
    list(
      id = find_match(id_pattern, "id", stems[[i]]),
      subject = find_match(subject_pattern, "subject", stems[[i]])
    )
  })
  for (i in seq_along(files)) {
    if (
      anyNA(unlist(identifiers[[i]])) ||
        any(!nzchar(unlist(identifiers[[i]])))
    ) {
      stop("ID or subject pattern did not match: ", files[[i]], call. = FALSE)
    }
  }

  groups <- vapply(
    identifiers,
    function(x) paste(x$id, x$subject, sep = "\r"),
    character(1)
  )
  if (anyDuplicated(groups)) {
    duplicate <- which(duplicated(groups) | duplicated(groups, fromLast = TRUE))
    stop(
      "Multiple exports resolve to the same ID and subject: ",
      paste(files[duplicate], collapse = ", "),
      call. = FALSE
    )
  }

  emotion_columns <- c(
    "neutral",
    "happy",
    "sad",
    "angry",
    "surprised",
    "scared",
    "disgusted"
  )
  inputs <- lapply(seq_along(files), function(i) {
    data <- loadFRfile(files[[i]], values_as_numeric = TRUE, clean_names = TRUE)
    if (is.null(data) || !"video_time" %in% names(data)) {
      stop(
        "Missing detailed coding or video time in: ",
        files[[i]],
        call. = FALSE
      )
    }
    if (
      "participant_name" %in%
        names(data) &&
        length(unique(stats::na.omit(trimws(as.character(
          data$participant_name
        ))))) >
          1L
    ) {
      stop("Multiple participants in one export: ", files[[i]], call. = FALSE)
    }
    columns <- intersect(emotion_columns, names(data))
    if (!length(columns)) {
      stop("No emotion columns in: ", files[[i]], call. = FALSE)
    }
    data.frame(
      id = identifiers[[i]]$id,
      subject = identifiers[[i]]$subject,
      video_time = data$video_time,
      data[, columns, drop = FALSE],
      check.names = FALSE
    )
  })
  coding_input <- dplyr::bind_rows(inputs)
  coded_data <- convert_to_episodes(
    coding_input,
    T_up = T_up,
    T_down = T_down,
    delta = delta,
    delta_window = delta_window,
    min_dur_sec = min_dur_sec,
    consecutive_missing = consecutive_missing,
    fps = as.integer(fps[[1L]]),
    cores = cores
  )
  output_dir <- dirname(outpath)
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist: ", output_dir, call. = FALSE)
  }
  save(coded_data, file = outpath)
  invisible(coded_data)
}
