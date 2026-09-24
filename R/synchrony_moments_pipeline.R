#' Create synchrony moments and video clips from FaceReader outputs
#'
#' Recursively discovers videos and FaceReader detailed TXT/XLSX exports below a
#' folder. Each export is matched to a video using the basename in its FaceReader
#' `Filename` metadata, ignoring case, directories, and file extensions. Videos
#' without matching detailed exports are skipped with a message.
#' Each matched video must have exactly two detailed outputs with distinct, stable
#' `Participant Name` values.
#'
#' @param inpath Existing directory containing videos and FaceReader exports.
#' @param output_dir Optional root directory for clip outputs. Defaults to `inpath`.
#' @param video_pattern Case-insensitive regular expression for video filenames.
#' @param video_path Optional existing video path. When supplied, all detailed
#'   exports are paired to this one video instead of their header filenames.
#' @param subject_from_filename Use the FaceReader export filename as the subject
#'   when its `Participant Name` column is absent.
#' @param verbose Whether to list videos with and without matching FaceReader
#'   detailed exports. Videos without matching detailed exports are always reported
#'   as skipped.
#' @param T_up,T_down,delta,delta_window,min_dur_sec,consecutive_missing,fps,cores
#'   Arguments passed to [convert_to_episodes()].
#' @param time_limit,time_limit_frames,constraint_method,missing_threshold,exclude_emotions
#'   Arguments passed to [shared_synchronous_episodes()].
#' @param n,emotion,optimised_subject,only_synchronies,buffer,buffer_units,output,overwrite,ffmpeg
#'   Arguments passed to [export_shared_synchrony_clips()].
#'
#' @return A `synchrony_moments_pipeline` list containing discovery `manifest`,
#'   validated `videos`, and named per-video `results`.
#' @examples
#' \dontrun{
#' synchrony_moments_pipeline("path/to/study-folder", output_dir = "clips")
#' }
#' @export
synchrony_moments_pipeline <- function(
  inpath,
  output_dir = inpath,
  video_pattern = "\\.(mp4|mov|mkv|avi)$",
  video_path = NULL,
  subject_from_filename = FALSE,
  verbose = FALSE,
  T_up = 0.20,
  T_down = 0.1,
  delta = 0.10,
  delta_window = 0.2,
  min_dur_sec = 0.1,
  consecutive_missing = 150L,
  fps = 30L,
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
  buffer = 0,
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
    !is.character(video_pattern) ||
      length(video_pattern) != 1L ||
      !nzchar(video_pattern)
  ) {
    stop(
      "`video_pattern` must be one non-empty regular expression.",
      call. = FALSE
    )
  }
  buffer_units <- match.arg(buffer_units)
  output <- match.arg(output)
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
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  files <- list.files(
    inpath,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
  discovered_videos <- files[grepl(
    video_pattern,
    basename(files),
    ignore.case = TRUE
  )]
  videos <- if (is.null(video_path)) {
    discovered_videos
  } else {
    normalizePath(video_path, winslash = "/", mustWork = TRUE)
  }
  fr_files <- files[tolower(tools::file_ext(files)) %in% c("txt", "xlsx")]
  if (length(videos) == 0L) {
    stop("No videos matching `video_pattern` were found.", call. = FALSE)
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
      video_path = NA_character_,
      participant = NA_character_,
      status = "discovered",
      error = NA_character_
    )
  }))
  detailed <- manifest[type == "detailed"]
  if (nrow(detailed) == 0L) {
    stop("No detailed FaceReader outputs were found.", call. = FALSE)
  }
  if (
    is.null(video_path) &&
      (anyNA(detailed$video_key) || any(!nzchar(detailed$video_key)))
  ) {
    stop(
      "Each detailed FaceReader output must contain a Filename metadata value.",
      call. = FALSE
    )
  }
  if (is.null(video_path)) {
    if (verbose) {
      synchrony_report_video_matches(videos, detailed$video_key)
    } else {
      synchrony_report_unmatched_videos(videos, detailed$video_key)
    }
    unmatched <- setdiff(unique(detailed$video_key), video_keys)
    if (length(unmatched) > 0L) {
      stop(
        "No discovered video matches FaceReader Filename metadata: ",
        paste(unmatched, collapse = ", "),
        call. = FALSE
      )
    }
    manifest[
      type == "detailed" & video_key %in% video_keys,
      video_path := videos[match(video_key, video_keys)]
    ]
  } else {
    manifest[type == "detailed", video_path := videos[[1L]]]
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
    participants <- if ("participant_name" %in% names(data)) {
      unique(trimws(as.character(data$participant_name)))
    } else {
      character()
    }
    participants <- participants[!is.na(participants) & nzchar(participants)]
    if (length(participants) == 0L && subject_from_filename) {
      participants <- tools::file_path_sans_ext(basename(manifest$fr_path[[i]]))
    }
    if (length(participants) != 1L) {
      stop(
        "FaceReader output must contain exactly one non-missing Participant Name, or set `subject_from_filename = TRUE`: ",
        manifest$fr_path[[i]],
        call. = FALSE
      )
    }
    manifest$participant[[i]] <<- participants
    manifest$status[[i]] <<- "validated"
    data
  })
  names(loaded) <- manifest$fr_path[detailed_indices]
  video_rows <- manifest[type == "detailed" & !is.na(video_path)]
  counts <- data.table::data.table(video_path = videos)[
    video_rows[,
      .(n_outputs = .N, n_participants = data.table::uniqueN(participant)),
      by = video_path
    ],
    on = "video_path"
  ]
  counts[is.na(n_outputs), `:=`(n_outputs = 0L, n_participants = 0L)]
  invalid <- counts[n_outputs != 2L | n_participants != 2L]
  if (nrow(invalid) > 0L) {
    stop(
      "Each matched video must have exactly two detailed outputs with distinct Participant Name values.",
      call. = FALSE
    )
  }

  data.table::setorder(video_rows, video_path, participant)
  video_table <- video_rows[,
    .(
      video_filename = basename(video_path[[1L]]),
      participant1 = participant[[1L]],
      participant2 = participant[[2L]],
      fr_path1 = fr_path[[1L]],
      fr_path2 = fr_path[[2L]]
    ),
    by = video_path
  ]
  data.table::setorder(video_table, video_path)
  video_table[, id := paste0("video_", sprintf("%03d", seq_len(.N)))]
  video_rows <- merge(
    video_rows,
    video_table[, .(video_path, id)],
    by = "video_path",
    sort = FALSE
  )
  data.table::setorder(video_rows, video_path, participant)

  results <- vector("list", nrow(video_table))
  names(results) <- video_table$id
  for (i in seq_len(nrow(video_table))) {
    rows <- video_rows[video_path == video_table$video_path[[i]]]
    inputs <- unname(loaded[rows$fr_path])
    inputs <- Map(
      function(data, subject) {
        data$id <- video_table$id[[i]]
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
      fps,
      cores
    )
    shared <- shared_synchronous_episodes(
      coded_data,
      time_limit = time_limit,
      time_limit_frames = time_limit_frames,
      constraint_method = constraint_method,
      fps = fps,
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
      video_table$video_path[[i]],
      video_table$id[[i]],
      output
    )
    clip_manifest <- if (nrow(selected) == 0L) {
      data.table::data.table()
    } else {
      export_shared_synchrony_clips(
        coded_data,
        shared,
        stats::setNames(video_table$video_path[[i]], video_table$id[[i]]),
        n = n,
        emotion = emotion,
        optimised_subject = optimised_subject,
        only_synchronies = only_synchronies,
        buffer = buffer,
        buffer_units = buffer_units,
        output_path = clip_path,
        output = output,
        overwrite = overwrite,
        ffmpeg = ffmpeg
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
    list(manifest = manifest, videos = video_table, results = results),
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
  ext <- tolower(tools::file_ext(path))
  if (ext == "txt") {
    lines <- readr::read_lines(path, n_max = 200L)
    type <- if (grepl("detailed", lines[[1L]], ignore.case = TRUE)) {
      "detailed"
    } else if (grepl("state", lines[[1L]], ignore.case = TRUE)) {
      "state"
    } else {
      "other"
    }
    filename_line <- lines[grepl(
      "^[[:space:]]*Filename[[:space:]]*",
      lines,
      ignore.case = TRUE
    )][1L]
    filename <- sub(
      "^[[:space:]]*Filename[[:space:]]*[:\\t]*",
      "",
      filename_line,
      ignore.case = TRUE
    )
  } else {
    values <- suppressMessages(readxl::read_excel(
      path,
      n_max = 200L,
      col_names = FALSE
    ))
    values <- as.data.frame(values, stringsAsFactors = FALSE)
    text <- as.character(unlist(values, use.names = FALSE))
    type <- if (
      any(grepl("video analysis detailed", text, ignore.case = TRUE))
    ) {
      "detailed"
    } else if (any(grepl("video analysis state", text, ignore.case = TRUE))) {
      "state"
    } else {
      "other"
    }
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
    filename <- if (
      is.na(filename_column) ||
        is.na(filename_row) ||
        filename_column == ncol(values)
    ) {
      NA_character_
    } else {
      as.character(values[[filename_column + 1L]][[filename_row]])
    }
  }
  list(type = type, video_filename = trimws(filename))
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
