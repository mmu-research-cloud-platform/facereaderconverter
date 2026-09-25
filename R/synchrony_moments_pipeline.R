#' Create synchrony moments and video clips from FaceReader outputs
#'
#' Recursively discovers videos and FaceReader detailed TXT/XLSX exports below a
#' folder. Each export is matched to a video using the basename in its FaceReader
#' `Filename` metadata, ignoring case, directories, and file extensions. Videos
#' without matching detailed exports are skipped with a message.
#' Videos with other than two detailed outputs containing a single stable
#' `Participant Name` are skipped with a warning. Repeated participant names
#' across multiple outputs for one video are errors.
#'
#' @param inpath Existing directory containing videos and FaceReader exports.
#' @param output_dir Optional root directory for clip outputs. Defaults to `inpath`.
#' @param video_extension Regular expression matching supported video filename
#'   extensions. Defaults to common video formats.
#' @param video_pattern Optional case-insensitive regular expression for video
#'   filenames. When supplied, only matching videos are considered.
#' @param video_path Optional existing video path. When supplied, only this video
#'   is considered and both video filters are ignored.
#' @param subject_from_filename Use the FaceReader export filename as the subject
#'   when its `Participant Name` column is absent.
#' @param subject_map Optional named character vector mapping FaceReader export
#'   basenames, including extensions, to subject names. Matching ignores case;
#'   mapped names override `Participant Name`.
#' @param verbose Whether to list videos with and without matching FaceReader
#'   detailed exports and display FFmpeg output. Videos without matching detailed
#'   exports are always reported as skipped; otherwise, clip progress is shown.
#' @param T_up,T_down,delta,delta_window,min_dur_sec,consecutive_missing,cores
#'   Arguments passed to [convert_to_episodes()]. The frame rate is read from each
#'   FaceReader export's `Frame rate` metadata, rounded to the nearest integer,
#'   and must agree within a video.
#' @param time_limit,time_limit_frames,constraint_method,missing_threshold,exclude_emotions
#'   Arguments passed to [shared_synchronous_episodes()].
#' @param n,emotion,optimised_subject,only_synchronies,buffer,buffer_units,output,overwrite,ffmpeg
#'   Arguments passed to [export_shared_synchrony_clips()].
#'
#' @return A `synchrony_moments_pipeline` list containing discovery `manifest`,
#'   all in-scope `videos` with match counts, frame rates, and status, and named
#'   per-video `results` for completed videos.
#' @examples
#' \dontrun{
#' synchrony_moments_pipeline("path/to/study-folder", output_dir = "clips")
#' }
#' @export
synchrony_moments_pipeline <- function(
  inpath,
  output_dir = inpath,
  video_extension = "\\.(mp4|mov|mkv|avi)$",
  video_pattern = NULL,
  video_path = NULL,
  subject_from_filename = TRUE,
  subject_map = NULL,
  verbose = FALSE,
  T_up = 0.20,
  T_down = 0.1,
  delta = 0.10,
  delta_window = 0.2,
  min_dur_sec = 0.1,
  consecutive_missing = 150L,
  cores = 0L,
  time_limit = 3,
  time_limit_frames = NULL,
  constraint_method = "episode",
  missing_threshold = 0,
  exclude_emotions = "neutral",
  n = 10L,
  emotion = "happy",
  optimised_subject = "both",
  only_synchronies = TRUE,
  buffer = c(before = 5, after = 3),
  buffer_units = c("seconds", "frames"),
  output = c("zip", "folder"),
  overwrite = FALSE,
  ffmpeg = "ffmpeg"
) {
  if (!is.character(inpath) || length(inpath) != 1L || !dir.exists(inpath)) {
    stop("`inpath` must be one existing directory.", call. = FALSE)
  }
  if (
    !is.character(output_dir) || length(output_dir) != 1L || !nzchar(output_dir)
  ) {
    stop("`output_dir` must be one non-empty directory path.", call. = FALSE)
  }
  if (
    !is.character(video_extension) ||
      length(video_extension) != 1L ||
      is.na(video_extension) ||
      !nzchar(video_extension)
  ) {
    stop(
      "`video_extension` must be one non-empty regular expression.",
      call. = FALSE
    )
  }
  if (
    !is.null(video_pattern) &&
      (!is.character(video_pattern) ||
        length(video_pattern) != 1L ||
        is.na(video_pattern) ||
        !nzchar(video_pattern))
  ) {
    stop(
      "`video_pattern` must be NULL or one non-empty regular expression.",
      call. = FALSE
    )
  }
  buffer_units <- match.arg(buffer_units, c("seconds", "frames"))
  output <- match.arg(output, c("zip", "folder"))
  if (
    !is.null(video_path) &&
      (!is.character(video_path) ||
        length(video_path) != 1L ||
        !file.exists(video_path))
  ) {
    stop("`video_path` must be NULL or one existing video file.", call. = FALSE)
  }
  if (
    !is.logical(subject_from_filename) ||
      length(subject_from_filename) != 1L ||
      is.na(subject_from_filename)
  ) {
    stop("`subject_from_filename` must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    !is.null(subject_map) &&
      (!is.character(subject_map) ||
        is.null(names(subject_map)) ||
        anyNA(subject_map) ||
        any(!nzchar(trimws(subject_map))) ||
        anyNA(names(subject_map)) ||
        any(!nzchar(names(subject_map))) ||
        anyDuplicated(tolower(names(subject_map))))
  ) {
    stop(
      paste0(
        "`subject_map` must be NULL or a uniquely named character vector ",
        "with non-empty names and values."
      ),
      call. = FALSE
    )
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  files <- list.files(
    inpath,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
  videos <- if (!is.null(video_path)) {
    normalizePath(video_path, winslash = "/", mustWork = TRUE)
  } else {
    extension_videos <- files[grepl(
      video_extension,
      basename(files),
      ignore.case = TRUE
    )]
    if (is.null(video_pattern)) {
      extension_videos
    } else {
      extension_videos[grepl(
        video_pattern,
        basename(extension_videos),
        ignore.case = TRUE
      )]
    }
  }
  fr_files <- files[tolower(tools::file_ext(files)) %in% c("txt", "xlsx")]
  if (length(videos) == 0L) {
    stop("No videos matching the video filters were found.", call. = FALSE)
  }
  if (length(fr_files) == 0L) {
    stop("No FaceReader TXT or XLSX files were found.", call. = FALSE)
  }

  video_keys <- synchrony_video_key(videos)
  duplicate_keys <- unique(video_keys[duplicated(video_keys)])
  if (length(duplicate_keys) > 0L) {
    stop(
      "Video basenames must be unique ignoring case: ",
      paste(duplicate_keys, collapse = ", "),
      call. = FALSE
    )
  }
  header_metadata <- lapply(fr_files, synchrony_fr_header_metadata)
  manifest <- data.table::rbindlist(lapply(seq_along(fr_files), function(i) {
    metadata <- header_metadata[[i]]
    data.table::data.table(
      fr_path = normalizePath(fr_files[[i]], winslash = "/", mustWork = TRUE),
      type = metadata$type,
      video_filename = metadata$video_filename,
      video_key = synchrony_video_key(metadata$video_filename),
      fps = metadata$fps,
      video_path = NA_character_,
      participant = NA_character_,
      status = "discovered",
      error = NA_character_
    )
  }))
  other_manifest <- manifest[type != "detailed"]
  manifest <- manifest[type == "detailed"]
  if (nrow(manifest) == 0L) {
    stop("No detailed FaceReader outputs were found.", call. = FALSE)
  }
  if (
    is.null(video_path) &&
      (anyNA(manifest$video_key) || any(!nzchar(manifest$video_key)))
  ) {
    stop(
      "Each detailed FaceReader output must contain a Filename metadata value.",
      call. = FALSE
    )
  }
  if (is.null(video_path)) {
    if (verbose) {
      synchrony_report_video_matches(videos, manifest$video_key)
    } else {
      synchrony_report_unmatched_videos(videos, manifest$video_key)
    }
    manifest[
      type == "detailed" & !video_key %in% video_keys,
      `:=`(
        status = "skipped",
        error = "Filename metadata is outside the selected video filters."
      )
    ]
    manifest[
      type == "detailed" & video_key %in% video_keys,
      video_path := videos[match(video_key, video_keys)]
    ]
  } else {
    selected_video_key <- synchrony_video_key(videos)
    manifest[
      type == "detailed" & video_key == selected_video_key,
      video_path := videos[[1L]]
    ]
    manifest[
      type == "detailed" & (is.na(video_key) | video_key != selected_video_key),
      `:=`(
        status = "skipped",
        error = "Filename metadata does not match the selected video."
      )
    ]
  }

  detailed_indices <- which(
    manifest$type == "detailed" & !is.na(manifest$video_path)
  )
  loaded <- lapply(detailed_indices, function(i) {
    data <- loadFRfile(
      manifest$fr_path[[i]],
      values_as_numeric = TRUE,
      clean_names = TRUE
    )
    export_name <- basename(manifest$fr_path[[i]])
    map_index <- if (is.null(subject_map)) {
      NA_integer_
    } else {
      match(tolower(export_name), tolower(names(subject_map)))
    }
    mapped_subject <- if (is.na(map_index)) {
      NULL
    } else {
      trimws(unname(subject_map[[map_index]]))
    }
    participants <- if (
      !is.null(mapped_subject) && length(mapped_subject) == 1L
    ) {
      mapped_subject
    } else if ("participant_name" %in% names(data)) {
      unique(trimws(as.character(data$participant_name)))
    } else {
      character()
    }
    participants <- participants[!is.na(participants) & nzchar(participants)]
    if (length(participants) == 0L && subject_from_filename) {
      participants <- tools::file_path_sans_ext(basename(manifest$fr_path[[i]]))
    }
    if (length(participants) != 1L) {
      manifest$status[[i]] <<- "skipped"
      manifest$error[[i]] <<- paste0(
        "Expected exactly one non-missing Participant Name; found ",
        length(participants)
      )
      return(NULL)
    }
    manifest$participant[[i]] <<- participants
    manifest$status[[i]] <<- "validated"
    data
  })
  names(loaded) <- manifest$fr_path[detailed_indices]
  video_rows <- manifest[type == "detailed" & !is.na(video_path)]
  duplicate_participants <- video_rows[
    !is.na(participant),
    .N,
    by = .(video_path, participant)
  ][N > 1L]
  if (nrow(duplicate_participants) > 0L) {
    stop(
      "Multiple FaceReader outputs have the same Participant Name for video ",
      duplicate_participants$video_path[[1L]],
      ": ",
      duplicate_participants$participant[[1L]],
      call. = FALSE
    )
  }
  counts <- if (nrow(video_rows) == 0L) {
    data.table::data.table(
      video_path = character(),
      n_outputs = integer(),
      n_matched_outputs = integer(),
      n_valid_fps = integer(),
      n_unique_fps = integer(),
      fps = numeric()
    )
  } else {
    video_rows[,
      .(
        n_outputs = .N,
        n_matched_outputs = sum(!is.na(participant)),
        n_valid_fps = sum(is.finite(fps) & fps > 0 & fps == round(fps)),
        n_unique_fps = data.table::uniqueN(fps[is.finite(fps) & fps > 0]),
        fps = fps[[1L]]
      ),
      by = video_path
    ]
  }
  invalid <- counts[
    n_outputs != 2L |
      n_matched_outputs != 2L |
      n_valid_fps != 2L |
      n_unique_fps != 1L
  ]
  if (nrow(invalid) > 0L) {
    for (i in seq_len(nrow(invalid))) {
      skip_reason <- paste0(
        "Skipping video ",
        invalid$video_path[[i]],
        ": found ",
        invalid$n_matched_outputs[[i]],
        " sufficiently matched outputs out of ",
        invalid$n_outputs[[i]],
        " detailed outputs (found ",
        invalid$n_valid_fps[[i]],
        " valid frame rates across ",
        invalid$n_unique_fps[[i]],
        " unique values)."
      )
      warning(skip_reason, call. = FALSE)
      manifest[
        video_path == invalid$video_path[[i]],
        `:=`(status = "skipped", error = skip_reason)
      ]
    }
    video_rows <- video_rows[
      video_path %in%
        counts[
          n_outputs == 2L &
            n_matched_outputs == 2L &
            n_valid_fps == 2L &
            n_unique_fps == 1L,
          video_path
        ]
    ]
  }

  data.table::setorder(video_rows, video_path, participant)
  video_table <- data.table::data.table(
    video_path = videos,
    video_filename = basename(videos)
  )
  matched_counts <- manifest[
    type == "detailed" & !is.na(video_path),
    .(
      n_matched_outputs = .N,
      n_validated_outputs = sum(status == "validated"),
      n_valid_fps = sum(is.finite(fps) & fps > 0 & fps == round(fps)),
      n_unique_fps = data.table::uniqueN(fps[is.finite(fps) & fps > 0]),
      fps = if (any(is.finite(fps) & fps > 0)) {
        fps[which(is.finite(fps) & fps > 0)[[1L]]]
      } else {
        NA_real_
      }
    ),
    by = video_path
  ]
  completed_details <- if (nrow(video_rows) == 0L) {
    data.table::data.table(
      video_path = character(),
      participant1 = character(),
      participant2 = character(),
      fr_path1 = character(),
      fr_path2 = character()
    )
  } else {
    video_rows[,
      .(
        participant1 = participant[[1L]],
        participant2 = participant[[2L]],
        fr_path1 = fr_path[[1L]],
        fr_path2 = fr_path[[2L]]
      ),
      by = video_path
    ]
  }
  video_table <- merge(
    video_table,
    matched_counts,
    by = "video_path",
    all.x = TRUE,
    sort = FALSE
  )
  video_table <- merge(
    video_table,
    completed_details,
    by = "video_path",
    all.x = TRUE,
    sort = FALSE
  )
  video_table[is.na(n_matched_outputs), n_matched_outputs := 0L]
  video_table[is.na(n_validated_outputs), n_validated_outputs := 0L]
  video_table[is.na(n_valid_fps), n_valid_fps := 0L]
  video_table[is.na(n_unique_fps), n_unique_fps := 0L]
  video_table[,
    status := data.table::fcase(
      n_matched_outputs == 0L                                            ,
      "unmatched"                                                        ,
      n_validated_outputs == 2L & n_valid_fps == 2L & n_unique_fps == 1L ,
      "completed"                                                        ,
      default = "failed"
    )
  ]
  data.table::setorder(video_table, video_path)
  video_table[,
    id := ifelse(
      status == "completed",
      paste0("video_", sprintf("%03d", cumsum(status == "completed"))),
      NA_character_
    )
  ]
  video_rows <- merge(
    video_rows,
    video_table[status == "completed", .(video_path, id)],
    by = "video_path",
    sort = FALSE
  )
  data.table::setorder(video_rows, video_path, participant)

  completed_videos <- video_table[status == "completed"]
  results <- vector("list", nrow(completed_videos))
  names(results) <- completed_videos$id
  for (i in seq_len(nrow(completed_videos))) {
    rows <- video_rows[video_path == completed_videos$video_path[[i]]]
    inputs <- unname(loaded[rows$fr_path])
    inputs <- Map(
      function(data, subject) {
        data$id <- completed_videos$id[[i]]
        data$subject <- subject
        data
      },
      inputs,
      rows$participant
    )
    coding_input <- dplyr::bind_rows(inputs)
    emotion_columns <- intersect(
      c("neutral", "happy", "sad", "angry", "surprised", "scared", "disgusted"),
      names(coding_input)
    )
    coding_input <- coding_input[, c(
      "id",
      "subject",
      "video_time",
      emotion_columns
    )]
    coded_data <- convert_to_episodes(
      coding_input,
      T_up,
      T_down,
      delta,
      delta_window,
      min_dur_sec,
      consecutive_missing,
      completed_videos$fps[[i]],
      cores
    )
    shared <- shared_synchronous_episodes(
      coded_data,
      time_limit = time_limit,
      time_limit_frames = time_limit_frames,
      constraint_method = constraint_method,
      fps = completed_videos$fps[[i]],
      missing_threshold = missing_threshold,
      exclude_emotions = exclude_emotions
    )
    selected_emotions <- emotion
    selected <- if (identical(optimised_subject, "both") || only_synchronies) {
      if (is.null(selected_emotions)) {
        shared
      } else {
        shared[emotion %in% selected_emotions]
      }
    } else {
      episodes <- coded_data$episodes
      if (is.null(selected_emotions)) {
        episodes
      } else {
        episodes[emotion %in% selected_emotions]
      }
    }
    clip_path <- synchrony_clip_output_path(
      output_dir,
      completed_videos$video_path[[i]],
      completed_videos$id[[i]],
      output
    )
    clip_manifest <- if (nrow(selected) == 0L) {
      data.table::data.table()
    } else {
      export_shared_synchrony_clips(
        coded_data,
        shared,
        stats::setNames(
          completed_videos$video_path[[i]],
          completed_videos$id[[i]]
        ),
        n = n,
        emotion = emotion,
        optimised_subject = optimised_subject,
        only_synchronies = only_synchronies,
        buffer = buffer,
        buffer_units = buffer_units,
        output_path = clip_path,
        output = output,
        overwrite = overwrite,
        ffmpeg = ffmpeg,
        verbose = verbose
      )
    }
    results[[i]] <- list(
      coding_input = coding_input,
      coded_data = coded_data,
      shared_moments = shared,
      clip_manifest = clip_manifest,
      clip_output = clip_path
    )
  }
  structure(
    list(
      manifest = data.table::rbindlist(
        list(manifest, other_manifest),
        use.names = TRUE,
        fill = TRUE
      ),
      videos = video_table,
      results = results
    ),
    class = "synchrony_moments_pipeline"
  )
}

synchrony_report_video_matches <- function(videos, fr_video_keys) {
  video_keys <- synchrony_video_key(videos)
  matched <- videos[video_keys %in% fr_video_keys]
  unmatched <- videos[!video_keys %in% fr_video_keys]
  message(
    "Videos matched to FaceReader files:\n",
    if (length(matched)) paste(sort(matched), collapse = "\n") else "(none)",
    "\nVideos without matching FaceReader files:\n",
    if (length(unmatched)) paste(sort(unmatched), collapse = "\n") else "(none)"
  )
  synchrony_report_unmatched_videos(videos, fr_video_keys)
}

synchrony_report_unmatched_videos <- function(videos, fr_video_keys) {
  unmatched <- videos[!synchrony_video_key(videos) %in% fr_video_keys]
  if (length(unmatched) > 0L) {
    message(
      "Skipped videos without matching detailed FaceReader exports:\n",
      paste(sort(unmatched), collapse = "\n")
    )
  }
}

synchrony_video_key <- function(path) {
  path <- gsub("\\\\", "/", path)
  ifelse(
    is.na(path),
    NA_character_,
    tolower(tools::file_path_sans_ext(basename(path)))
  )
}

synchrony_fr_header_metadata <- function(path) {
  switch(
    tolower(tools::file_ext(path)),
    txt = synchrony_fr_txt_header_metadata(path),
    xlsx = synchrony_fr_xlsx_header_metadata(path),
    stop("Unsupported FaceReader file extension: ", path, call. = FALSE)
  )
}

synchrony_fr_txt_header_metadata <- function(path) {
  lines <- readr::read_lines(path, n_max = 200L)
  list(
    type = synchrony_fr_output_type(lines),
    video_filename = synchrony_fr_txt_filename(lines),
    fps = synchrony_fr_txt_fps(lines)
  )
}

synchrony_fr_xlsx_header_metadata <- function(path) {
  values <- suppressMessages(readxl::read_excel(
    path,
    n_max = 200L,
    col_names = FALSE
  ))
  values <- as.data.frame(values, stringsAsFactors = FALSE)
  list(
    type = synchrony_fr_output_type(unlist(values, use.names = FALSE)),
    video_filename = synchrony_fr_xlsx_filename(values),
    fps = synchrony_fr_xlsx_fps(values)
  )
}

synchrony_fr_output_type <- function(metadata) {
  metadata <- as.character(metadata)
  if (any(grepl("video analysis detailed", metadata, ignore.case = TRUE))) {
    "detailed"
  } else if (any(grepl("video analysis state", metadata, ignore.case = TRUE))) {
    "state"
  } else {
    "other"
  }
}

synchrony_fr_txt_fps <- function(lines) {
  fps_lines <- lines[grepl(
    "^[[:space:]]*Frame rate[[:space:]]*",
    lines,
    ignore.case = TRUE
  )]
  if (length(fps_lines) == 0L) {
    return(NA_real_)
  }
  fps_line <- fps_lines[[1L]]
  synchrony_fr_parse_fps(trimws(sub(
    "^[[:space:]]*Frame rate[[:space:]]*[:\\t]*",
    "",
    fps_line,
    ignore.case = TRUE
  )))
}

synchrony_fr_txt_filename <- function(lines) {
  filename_line <- lines[grepl(
    "^[[:space:]]*Filename[[:space:]]*",
    lines,
    ignore.case = TRUE
  )][1L]
  trimws(sub(
    "^[[:space:]]*Filename[[:space:]]*[:\\t]*",
    "",
    filename_line,
    ignore.case = TRUE
  ))
}

synchrony_fr_parse_fps <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value) || value <= 0) {
    return(NA_real_)
  }
  round(value)
}

synchrony_fr_xlsx_fps <- function(values) {
  fps_columns <- which(vapply(
    values,
    function(column) {
      any(grepl(
        "^[[:space:]]*Frame rate[[:space:]]*$",
        column,
        ignore.case = TRUE
      ))
    },
    logical(1)
  ))
  fps_column <- if (length(fps_columns) == 0L) {
    NA_integer_
  } else {
    fps_columns[[1L]]
  }
  fps_row <- if (is.na(fps_column)) {
    NA_integer_
  } else {
    which(grepl(
      "^[[:space:]]*Frame rate[[:space:]]*$",
      values[[fps_column]],
      ignore.case = TRUE
    ))[1L]
  }
  if (
    is.na(fps_column) ||
      is.na(fps_row) ||
      fps_column == ncol(values)
  ) {
    return(NA_real_)
  }
  synchrony_fr_parse_fps(as.character(
    values[[fps_column + 1L]][[fps_row]]
  ))
}

synchrony_fr_xlsx_filename <- function(values) {
  filename_column <- which(vapply(
    values,
    function(column) {
      any(grepl(
        "^[[:space:]]*Filename[[:space:]]*$",
        column,
        ignore.case = TRUE
      ))
    },
    logical(1)
  ))[1L]
  filename_row <- if (is.na(filename_column)) {
    NA_integer_
  } else {
    which(grepl(
      "^[[:space:]]*Filename[[:space:]]*$",
      values[[filename_column]],
      ignore.case = TRUE
    ))[1L]
  }
  if (
    is.na(filename_column) ||
      is.na(filename_row) ||
      filename_column == ncol(values)
  ) {
    return(NA_character_)
  }
  trimws(as.character(values[[filename_column + 1L]][[filename_row]]))
}

synchrony_clip_output_path <- function(output_dir, video_path, id, output) {
  stem <- tools::file_path_sans_ext(basename(video_path))
  output_stem <- paste0(stem, "-", id, "-shared-synchrony-clips")
  if (output == "zip") {
    file.path(output_dir, paste0(output_stem, ".zip"))
  } else {
    file.path(output_dir, output_stem)
  }
}
